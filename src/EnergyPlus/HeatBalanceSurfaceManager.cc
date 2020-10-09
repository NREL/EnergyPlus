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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <chrono>
// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
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
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataStringGlobals.hh>
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
#include <EnergyPlus/HeatBalanceMovableInsulation.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/Material.hh>
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
#include <EnergyPlus/WindowModel.hh>
#include <WCECommon.hpp>
#include <WCEMultiLayerOptics.hpp>
#include <WCESingleLayerOptics.hpp>

namespace EnergyPlus {

namespace HeatBalanceSurfaceManager {

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
    using namespace DataGlobals;
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using DataMoistureBalance::HAirFD;
    using DataMoistureBalance::HConvExtFD;
    using DataMoistureBalance::HConvInFD;
    using DataMoistureBalance::HGrndFD;
    using DataMoistureBalance::HMassConvExtFD;
    using DataMoistureBalance::HMassConvInFD;
    using DataMoistureBalance::HSkyFD;
    using DataMoistureBalance::RhoVaporAirIn;
    using DataMoistureBalance::RhoVaporAirOut;
    using DataMoistureBalance::RhoVaporSurfIn;
    using DataMoistureBalance::TempOutsideAirFD;

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
    Array1D<Real64> RefAirTemp; // inside surface convection reference air temperatures

    namespace {
        bool ManageSurfaceHeatBalancefirstTime(true);
        bool InitSurfaceHeatBalancefirstTime(true);
        bool ComputeIntSWAbsorpFactorsfirstTime(true); // First time through routine
        bool UpdateThermalHistoriesFirstTimeFlag(true);
        bool CalculateZoneMRTfirstTime(true); // Flag for first time calculations
        bool calcHeatBalanceInsideSurfFirstTime(true);
        bool reportThermalResilienceFirstTime(true);
        bool reportVarHeatIndex(false);
        bool reportVarHumidex(false);
        bool hasPierceSET(true);
        bool reportCO2ResilienceFirstTime(true);
        bool reportVisualResilienceFirstTime(true);
        std::vector<Real64> lowSETLongestHours;
        std::vector<Real64> highSETLongestHours;
        std::vector<int> lowSETLongestStart;
        std::vector<int> highSETLongestStart;
        bool calcHeatBalInsideSurfFirstTime(true);
        bool calcHeatBalInsideSurfCTFOnlyFirstTime(true);
        int calcHeatBalInsideSurfErrCount(0);
        int calcHeatBalInsideSurfErrPointer(0);
        int calcHeatBalInsideSurfWarmupErrCount(0);
        bool calcHeatBalInsideSurEnvrnFlag(true);
    } // namespace

    // These are now external subroutines
    // PUBLIC  CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
    // PUBLIC  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

    void clear_state()
    {
        ManageSurfaceHeatBalancefirstTime = true;
        InitSurfaceHeatBalancefirstTime = true;
        ComputeIntSWAbsorpFactorsfirstTime = true;
        UpdateThermalHistoriesFirstTimeFlag = true;
        CalculateZoneMRTfirstTime = true;
        calcHeatBalanceInsideSurfFirstTime = true;
        reportThermalResilienceFirstTime = true;
        reportVarHeatIndex = false;
        reportVarHumidex = false;
        hasPierceSET = true;
        reportCO2ResilienceFirstTime = true;
        reportVisualResilienceFirstTime = true;
        lowSETLongestHours.clear();
        highSETLongestHours.clear();
        lowSETLongestStart.clear();
        highSETLongestStart.clear();
        calcHeatBalInsideSurfFirstTime = true;
        calcHeatBalInsideSurfCTFOnlyFirstTime = true;
        calcHeatBalInsideSurfErrCount = 0;
        calcHeatBalInsideSurfErrPointer = 0;
        calcHeatBalInsideSurfWarmupErrCount = 0;
        calcHeatBalInsideSurEnvrnFlag = true;
        RefAirTemp.deallocate();
    }

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
        using HeatBalFiniteDiffManager::SurfaceFD;
        using OutputReportTabular::GatherComponentLoadsSurface; // for writing tabular component loads output reports
        using ThermalComfort::ManageThermalComfort;

        int SurfNum;
        int ConstrNum;

        if (ManageSurfaceHeatBalancefirstTime) DisplayString("Initializing Surfaces");
        InitSurfaceHeatBalance(state); // Initialize all heat balance related parameters

        // Solve the zone heat balance 'Detailed' solution
        // Call the outside and inside surface heat balances
        if (ManageSurfaceHeatBalancefirstTime) DisplayString("Calculate Outside Surface Heat Balance");
        CalcHeatBalanceOutsideSurf(state);
        if (ManageSurfaceHeatBalancefirstTime) DisplayString("Calculate Inside Surface Heat Balance");
        CalcHeatBalanceInsideSurf(state);

        // The air heat balance must be called before the temperature history
        // updates because there may be a radiant system in the building
        if (ManageSurfaceHeatBalancefirstTime) DisplayString("Calculate Air Heat Balance");
        ManageAirHeatBalance(state);

        // IF NECESSARY, do one final "average" heat balance pass.  This is only
        // necessary if a radiant system is present and it was actually on for
        // part or all of the time step.
        UpdateFinalSurfaceHeatBalance(state);

        // Before we leave the Surface Manager the thermal histories need to be updated
        if (DataHeatBalance::AnyCTF || DataHeatBalance::AnyEMPD) {
            UpdateThermalHistories(); // Update the thermal histories
        }

        if (DataHeatBalance::AnyCondFD) {
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).Construction <= 0) continue; // Shading surface, not really a heat transfer surface
                ConstrNum = Surface(SurfNum).Construction;
                if (dataConstruction.Construct(ConstrNum).TypeIsWindow) continue; //  Windows simulated in Window module
                if (Surface(SurfNum).HeatTransferAlgorithm != HeatTransferModel_CondFD) continue;
                SurfaceFD(SurfNum).UpdateMoistureBalance();
            }
        }

        ManageThermalComfort(state, false); // "Record keeping" for the zone

        ReportSurfaceHeatBalance();
        if (ZoneSizingCalc) GatherComponentLoadsSurface();

        CalcThermalResilience(state);

        if (OutputReportTabular::displayThermalResilienceSummary) {
            ReportThermalResilience();
        }

        if (OutputReportTabular::displayCO2ResilienceSummary) {
            ReportCO2Resilience();
        }

        if (OutputReportTabular::displayVisualResilienceSummary) {
            ReportVisualResilience();
        }

        ManageSurfaceHeatBalancefirstTime = false;
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataDaylighting::mapResultsToReport;
        using DataDaylighting::NoDaylighting;
        using DataDaylighting::TotIllumMaps;
        using DataDaylighting::ZoneDaylight;
        using DataDaylightingDevices::NumOfTDDPipes;
        using DataDElight::LUX2FC;
        using namespace SolarShading;
        using ConvectionCoefficients::InitInteriorConvectionCoeffs;
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataLoopNode::Node;
        using DataRoomAirModel::IsZoneCV;
        using DataRoomAirModel::IsZoneDV;
        using DataRoomAirModel::IsZoneUI;
        using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
        using HeatBalFiniteDiffManager::InitHeatBalFiniteDiff;
        using InternalHeatGains::ManageInternalHeatGains;
        // RJH DElight Modification Begin
        using namespace DElightManagerF;
        using namespace std::chrono;
        // RJH DElight Modification End

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");
        static ObjexxFCL::gio::Fmt fmtLD("*");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FLOW:

        assert(equal_dimensions(TH, QH));

        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Outdoor environment for Surfaces");

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
        SetSurfaceOutBulbTempAt();
        CheckSurfaceOutBulbTempAt();

        SetSurfaceWindSpeedAt();
        SetSurfaceWindDirAt();
        //  DO SurfNum = 1, TotSurfaces
        //    IF (Surface(SurfNum)%ExtWind) Surface(SurfNum)%WindSpeed = WindSpeedAt(Surface(SurfNum)%Centroid%z)
        //  END DO
        if (AnyLocalEnvironmentsInModel) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).HasLinkedOutAirNode) {
                    Surface(SurfNum).OutDryBulbTemp = Node(Surface(SurfNum).LinkedOutAirNode).OutAirDryBulb;
                    Surface(SurfNum).OutWetBulbTemp = Node(Surface(SurfNum).LinkedOutAirNode).OutAirWetBulb;
                    Surface(SurfNum).WindSpeed = Node(Surface(SurfNum).LinkedOutAirNode).OutAirWindSpeed;
                    Surface(SurfNum).WindDir = Node(Surface(SurfNum).LinkedOutAirNode).OutAirWindDir;
                }

                if (InitSurfaceHeatBalancefirstTime && Surface(SurfNum).HasSurroundingSurfProperties) {
                    Real64 SrdSurfsNum = Surface(SurfNum).SurroundingSurfacesNum;
                    Real64 SrdSurfsViewFactor = 0;
                    if (SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0) {
                        SrdSurfsViewFactor += SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                    }
                    if (SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                        SrdSurfsViewFactor += SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                    }
                    for (int SrdSurfNum = 1; SrdSurfNum <= SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
                        SrdSurfsViewFactor += SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                    }
                    // Check if the sum of all defined view factors > 1.0
                    if (SrdSurfsViewFactor > 1.0) {
                        ShowSevereError("Illegal surrounding surfaces view factors for " + Surface(SurfNum).Name + ".");
                        ShowContinueError(" The sum of sky, ground, and all surrounding surfaces view factors should be less than 1.0.");
                    }
                    if (SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0 && SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                        // If both surface sky and ground view factor defined, overwrite with the defined value
                        Surface(SurfNum).ViewFactorSkyIR = SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                        Surface(SurfNum).ViewFactorGroundIR = SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                    } else if (SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0 &&
                               SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor < 0) {
                        // If only sky view factor defined, gound view factor = 1 - all other defined view factors.
                        Surface(SurfNum).ViewFactorSkyIR = SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                        Surface(SurfNum).ViewFactorGroundIR = 1 - SrdSurfsViewFactor;
                    } else if (SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor < 0 &&
                               SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                        // If only ground view factor defined, sky view factor = 1 - all other defined view factors.
                        Surface(SurfNum).ViewFactorGroundIR = SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
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
        if (AnyEnergyManagementSystemInModel) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).OutDryBulbTempEMSOverrideOn) {
                    Surface(SurfNum).OutDryBulbTemp = Surface(SurfNum).OutDryBulbTempEMSOverrideValue;
                }
                if (Surface(SurfNum).OutWetBulbTempEMSOverrideOn) {
                    Surface(SurfNum).OutWetBulbTemp = Surface(SurfNum).OutWetBulbTempEMSOverrideValue;
                }
                if (Surface(SurfNum).WindSpeedEMSOverrideOn) {
                    Surface(SurfNum).WindSpeed = Surface(SurfNum).WindSpeedEMSOverrideValue;
                }
                if (Surface(SurfNum).WindDirEMSOverrideOn) {
                    Surface(SurfNum).WindDir = Surface(SurfNum).WindDirEMSOverrideValue;
                }
            }
        }

        // Do the Begin Simulation initializations
        if (BeginSimFlag) {
            AllocateSurfaceHeatBalArrays(state); // Allocate the Module Arrays before any inits take place
            InterZoneWindow = std::any_of(Zone.begin(), Zone.end(), [](DataHeatBalance::ZoneData const &e) { return e.HasInterZoneWindow; });
            IsZoneDV.dimension(NumOfZones, false);
            IsZoneCV.dimension(NumOfZones, false);
            IsZoneUI.dimension(NumOfZones, false);
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag) {
            if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Temperature and Flux Histories");
            InitThermalAndFluxHistories(); // Set initial temperature and flux histories
        }

        // There are no daily initializations done in this portion of the surface heat balance

        // There are no hourly initializations done in this portion of the surface heat balance

        // Need to be called each timestep in order to check if surface points to new construction (EMS) and if does then
        // complex fenestration needs to be initialized for additional states
        TimestepInitComplexFenestration(state);

        // Calculate exterior-surface multipliers that account for anisotropy of
        // sky radiance
        if (SunIsUp && DifSolarRad > 0.0) {
            AnisoSkyViewFactors();
        } else {
            AnisoSkyMult = 0.0;
        }

        // Set shading flag for exterior windows (except flags related to daylighting) and
        // window construction (unshaded or shaded) to be used in heat balance calculation
        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Window Shading");

        WindowShadingManager(state);

        // Calculate factors that are used to determine how much long-wave radiation from internal
        // gains is absorbed by interior surfaces
        if (InitSurfaceHeatBalancefirstTime) DisplayString("Computing Interior Absorption Factors");
        if (InitSurfaceHeatBalancefirstTime) HeatBalanceIntRadExchange::InitInteriorRadExchange(state);
        ComputeIntThermalAbsorpFactors();

        // Calculate factors for diffuse solar absorbed by room surfaces and interior shades
        if (InitSurfaceHeatBalancefirstTime) DisplayString("Computing Interior Diffuse Solar Absorption Factors");
        ComputeIntSWAbsorpFactors();

        if (InterZoneWindow) {
            if (InitSurfaceHeatBalancefirstTime) {
                DisplayString("Computing Interior Diffuse Solar Exchange through Interzone Windows");
            }
            ComputeDifSolExcZonesWIZWindows(NumOfZones);
        }

        // For daylit zones, calculate interior daylight illuminance at reference points and
        // simulate lighting control system to get overhead electric lighting reduction
        // factor due to daylighting.
        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                if (Surface(SurfNum).ExtSolar) {
                    SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep = 0.0;
                    SurfaceWindow(SurfNum).LumWinFromRefPtRep = 0.0;
                }
            }
        }

        for (int NZ = 1; NZ <= NumOfZones; ++NZ) {
            if (ZoneDaylight(NZ).DaylightMethod == NoDaylighting) continue;
            ZoneDaylight(NZ).DaylIllumAtRefPt = 0.0;
            ZoneDaylight(NZ).GlareIndexAtRefPt = 0.0;
            ZoneDaylight(NZ).ZonePowerReductionFactor = 1.0;
            ZoneDaylight(NZ).InterReflIllFrIntWins = 0.0; // inter-reflected illuminance from interior windows
            if (ZoneDaylight(NZ).TotalDaylRefPoints != 0) {
                ZoneDaylight(NZ).TimeExceedingGlareIndexSPAtRefPt = 0.0;
                ZoneDaylight(NZ).TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0;
            }

            if (SunIsUp && ZoneDaylight(NZ).TotalDaylRefPoints != 0) {
                if (InitSurfaceHeatBalancefirstTime) DisplayString("Computing Interior Daylighting Illumination");
                DayltgInteriorIllum(NZ);
                if (!DoingSizing) DayltgInteriorMapIllum(NZ);
            }

            if (SunIsUp && NumOfTDDPipes > 0 && NZ == 1) {
                if (InitSurfaceHeatBalancefirstTime) DisplayString("Computing Interior Daylighting Illumination for TDD pipes");
                DayltgInteriorTDDIllum();
            }

            // RJH DElight Modification Begin - Call to DElight electric lighting control subroutine
            // Check if the sun is up and the current Thermal Zone hosts a Daylighting:DElight object
            if (SunIsUp && ZoneDaylight(NZ).TotalDaylRefPoints != 0 && (ZoneDaylight(NZ).DaylightMethod == DataDaylighting::DElightDaylighting)) {
                // Call DElight interior illuminance and electric lighting control subroutine
                Real64 dPowerReducFac = 1.0;       // Return value Electric Lighting Power Reduction Factor for current Zone and Timestep
                Real64 dHISKFFC = HISKF * LUX2FC;
                Real64 dHISUNFFC = HISUNF * LUX2FC;
                Real64 dSOLCOS1 = SOLCOS(1);
                Real64 dSOLCOS2 = SOLCOS(2);
                Real64 dSOLCOS3 = SOLCOS(3);
                Real64 dLatitude = Latitude;
                Real64 dCloudFraction = CloudFraction;
                // Init Error Flag to 0 (no Warnings or Errors) (returned from DElight)
                int iErrorFlag = 0;
                bool elOpened;

                int iReadStatus;        // Error File Read Status
                std::string cErrorMsg;  // Each DElight Error Message can be up to 200 characters long
                bool bEndofErrFile;     // End of Error File flag

                DElightElecLtgCtrl(len(Zone(NZ).Name),
                                   Zone(NZ).Name,
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
                    auto iDElightErrorFile = state.files.outputDelightDfdmpFileName.try_open(state.files.outputControl.delightdfdmp);
                     if (iDElightErrorFile.good()) {
                         elOpened = true;
                     } else {
                         elOpened = false;
                     }
                    //            IF (iwriteStatus /= 0) THEN
                    //              CALL ShowFatalError('InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output (readwrite).')
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
                            ShowWarningError(cErrorMsg);
                        }
                        // Is the current line an Error message?
                        if (has_prefix(cErrorLine.data, "ERROR: ")) {
                            cErrorMsg = cErrorLine.data.substr(7);
                            ShowSevereError(cErrorMsg);
                            iErrorFlag = 1;
                        }
                    }

                    // Close DElight Error File and delete

                    if (elOpened) {
                        iDElightErrorFile.close();
                        FileSystem::removeFile(iDElightErrorFile.fileName);
                    };
                    // If any DElight Error occurred then ShowFatalError to terminate
                    if (iErrorFlag > 0) {
                        ShowFatalError("End of DElight Error Messages");
                    }
                } else { // RJH 2008-03-07: No errors
                    // extract reference point illuminance values from DElight Electric Lighting dump file for reporting
                    // Open DElight Electric Lighting Dump File for reading
                    auto iDElightErrorFile = state.files.outputDelightEldmpFileName.try_open(state.files.outputControl.delighteldmp);
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
                        if (iDElightRefPt <= ZoneDaylight(NZ).TotalDaylRefPoints) {
                            ZoneDaylight(NZ).DaylIllumAtRefPt(iDElightRefPt) = dRefPtIllum;
                        }
                    }

                    // Close DElight Electric Lighting Dump File and delete
                    if (elOpened) {
                        iDElightErrorFile.close();
                        FileSystem::removeFile(iDElightErrorFile.fileName);
                    };
                }
                // Store the calculated total zone Power Reduction Factor due to DElight daylighting
                // in the ZoneDaylight structure for later use
                ZoneDaylight(NZ).ZonePowerReductionFactor = dPowerReducFac;
            }
            // RJH DElight Modification End - Call to DElight electric lighting control subroutine
        }

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
                if (SurfWinShadingFlag(SurfNum) > 0) {
                    SurfWinFracTimeShadingDeviceOn(SurfNum) = 1.0;
                } else {
                    SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
                }
            }
        }

        CalcInteriorRadExchange(TH(2, 1, _), 0, SurfNetLWRadToSurf, _, "Main");

        if (AirflowWindows) WindowGapAirflowControl();

        // The order of these initializations is important currently.  Over time we hope to
        //  take the appropriate parts of these inits to the other heat balance managers
        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Solar Heat Gains");

        InitSolarHeatGains(state);
        if (SunIsUp && (BeamSolarRad + GndSolarRad + DifSolarRad > 0.0)) {
            for (int NZ = 1; NZ <= NumOfZones; ++NZ) {
                if (ZoneDaylight(NZ).TotalDaylRefPoints > 0) {
                    if (Zone(NZ).HasInterZoneWindow) {
                        DayltgInterReflIllFrIntWins(NZ);
                        DayltgGlareWithIntWins(ZoneDaylight(NZ).GlareIndexAtRefPt, NZ);
                    }
                    DayltgElecLightingControl(state, NZ);
                }
            }
        } else if (mapResultsToReport && TimeStep == NumOfTimeStepInHour) {
            for (int MapNum = 1; MapNum <= TotIllumMaps; ++MapNum) {
                ReportIllumMap(state, MapNum);
            }
            mapResultsToReport = false;
        }

        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Internal Heat Gains");
        ManageInternalHeatGains(state, false);
        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Interior Solar Distribution");
        InitIntSolarDistribution();

        if (InitSurfaceHeatBalancefirstTime) DisplayString("Initializing Interior Convection Coefficients");
        InitInteriorConvectionCoeffs(state, TempSurfInTmp);

        if (BeginSimFlag) { // Now's the time to report surfaces, if desired
            //    if (firstTime) CALL DisplayString('Reporting Surfaces')
            //    CALL ReportSurfaces
            if (InitSurfaceHeatBalancefirstTime) DisplayString("Gathering Information for Predefined Reporting");
            GatherForPredefinedReport(state);
        }

        // Initialize the temperature history terms for conduction through the surfaces
        if (DataHeatBalance::AnyCondFD) {
            InitHeatBalFiniteDiff(state);
        }

        CTFConstOutPart = 0.0;
        CTFConstInPart = 0.0;
        if (AnyConstructInternalSourceInInput) {
            CTFTsrcConstPart = 0.0;
            CTFTuserConstPart = 0.0;
        }

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {// Loop through all surfaces...
            int const firstSurfOpaque = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpaque = Zone(zoneNum).NonWindowSurfaceLast;
            for (int SurfNum = firstSurfOpaque; SurfNum <= lastSurfOpaque; ++SurfNum) {
                auto const &surface(Surface(SurfNum));

                if (!surface.HeatTransSurf) continue; // Skip non-heat transfer surfaces
                if (surface.HeatTransferAlgorithm != HeatTransferModel_CTF &&
                    surface.HeatTransferAlgorithm != HeatTransferModel_EMPD)
                    continue;
                // Outside surface temp of "normal" windows not needed in Window5 calculation approach
                // Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

                int ConstrNum = surface.Construction;
                auto const &construct(dataConstruction.Construct(ConstrNum));
                if (construct.NumCTFTerms > 1) { // COMPUTE CONSTANT PORTION OF CONDUCTIVE FLUXES.

                    Real64 QIC = 0.0;
                    Real64 QOC = 0.0;
                    Real64 TSC;
                    Real64 TUC;
                    if (construct.SourceSinkPresent) {
                        TSC = 0.0;
                        TUC = 0.0;
                    }
                    auto l11(TH.index(1, 2, SurfNum));
                    auto l12(TH.index(2, 2, SurfNum));
                    auto const s3(TH.size3());
                    for (int Term = 1; Term <=
                                   construct.NumCTFTerms; ++Term, l11 += s3, l12 += s3) { // [ l11 ] == ( 1, Term + 1, SurfNum ), [ l12 ] == ( 1, Term + 1, SurfNum )

                        // Sign convention for the various terms in the following two equations
                        // is based on the form of the Conduction Transfer Function equation
                        // given by:
                        // Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old)
                        // Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old)
                        // In both equations, flux is positive from outside to inside.

                        // Tuned Aliases and linear indexing
                        Real64 const ctf_cross(construct.CTFCross(Term));
                        Real64 const TH11(TH[l11]);
                        Real64 const TH12(TH[l12]);

                        QIC += ctf_cross * TH11 - construct.CTFInside(Term) * TH12 + construct.CTFFlux(Term) * QH[l12];

                        QOC += construct.CTFOutside(Term) * TH11 - ctf_cross * TH12 + construct.CTFFlux(Term) * QH[l11];

                        if (construct.SourceSinkPresent) {
                            Real64 const QsrcHist1(QsrcHist(SurfNum, Term + 1));

                            QIC += construct.CTFSourceIn(Term) * QsrcHist1;

                            QOC += construct.CTFSourceOut(Term) * QsrcHist1;

                            TSC += construct.CTFTSourceOut(Term) * TH11 + construct.CTFTSourceIn(Term) * TH12 +
                                   construct.CTFTSourceQ(Term) * QsrcHist1 +
                                   construct.CTFFlux(Term) * TsrcHist(SurfNum, Term + 1);

                            TUC += construct.CTFTUserOut(Term) * TH11 + construct.CTFTUserIn(Term) * TH12 +
                                   construct.CTFTUserSource(Term) * QsrcHist1 +
                                   construct.CTFFlux(Term) * TuserHist(SurfNum, Term + 1);
                        }
                    }

                    CTFConstOutPart(SurfNum) = QOC;
                    CTFConstInPart(SurfNum) = QIC;
                    if (construct.SourceSinkPresent) {
                        CTFTsrcConstPart(SurfNum) = TSC;
                        CTFTuserConstPart(SurfNum) = TUC;
                    }
                } else { // Number of CTF Terms = 1-->Resistance only constructions have no history terms.

                    CTFConstOutPart(SurfNum) = 0.0;
                    CTFConstInPart(SurfNum) = 0.0;
                    if (construct.SourceSinkPresent) {
                        CTFTsrcConstPart(SurfNum) = 0.0;
                        CTFTuserConstPart(SurfNum) = 0.0;
                    }
                }
            }

        } // ...end of surfaces DO loop for initializing temperature history terms for the surface heat balances

        // Zero out all of the radiant system heat balance coefficient arrays
        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {// Loop through all surfaces...
            int const firstSurf = Zone(zoneNum).SurfaceFirst;
            int const lastSurf = Zone(zoneNum).SurfaceLast;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                RadSysTiHBConstCoef(SurfNum) = 0.0;
                RadSysTiHBToutCoef(SurfNum) = 0.0;
                RadSysTiHBQsrcCoef(SurfNum) = 0.0;
                RadSysToHBConstCoef(SurfNum) = 0.0;
                RadSysToHBTinCoef(SurfNum) = 0.0;
                RadSysToHBQsrcCoef(SurfNum) = 0.0;

                QRadSysSource(SurfNum) = 0.0;
                QPVSysSource(SurfNum) = 0.0;
                QHTRadSysSurf(SurfNum) = 0.0;
                QHWBaseboardSurf(SurfNum) = 0.0;
                QSteamBaseboardSurf(SurfNum) = 0.0;
                QElecBaseboardSurf(SurfNum) = 0.0;
                QCoolingPanelSurf(SurfNum) = 0.0;
                QPoolSurfNumerator(SurfNum) = 0.0;
                PoolHeatTransCoefs(SurfNum) = 0.0;
            } // ...end of Zone Surf loop
        } // ...end of Zone loop

        if (ZoneSizingCalc) GatherComponentLoadsSurfAbsFact();

        if (InitSurfaceHeatBalancefirstTime) DisplayString("Completed Initializing Surface Heat Balance");
        InitSurfaceHeatBalancefirstTime = false;
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
        Array1D_int numSurfaces(20);
        Array1D_int numExtSurfaces(20);
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
        static Real64 windowAreaWMult(0.0);
        static Real64 fenTotArea(0.0);
        static Real64 fenTotAreaNorth(0.0);
        static Real64 fenTotAreaNonNorth(0.0);
        static Real64 ufactArea(0.0);
        static Real64 ufactAreaNorth(0.0);
        static Real64 ufactAreaNonNorth(0.0);
        static Real64 shgcArea(0.0);
        static Real64 shgcAreaNorth(0.0);
        static Real64 shgcAreaNonNorth(0.0);
        static Real64 vistranArea(0.0);
        static Real64 vistranAreaNorth(0.0);
        static Real64 vistranAreaNonNorth(0.0);
        static Real64 intFenTotArea(0.0);
        static Real64 intUfactArea(0.0);
        static Real64 intShgcArea(0.0);
        static Real64 intVistranArea(0.0);
        bool isNorth;

        numSurfaces = 0;
        numExtSurfaces = 0;

        computedNetArea.allocate(TotSurfaces);
        computedNetArea = 0.0; // start at zero, add wall area and subtract window and door area

        for (int iSurf : DataSurfaces::AllSurfaceListReportOrder) {
            zonePt = Surface(iSurf).Zone;
            // only exterior surfaces including underground
            if ((Surface(iSurf).ExtBoundCond == ExternalEnvironment) || (Surface(iSurf).ExtBoundCond == Ground) ||
                (Surface(iSurf).ExtBoundCond == GroundFCfactorMethod) || (Surface(iSurf).ExtBoundCond == KivaFoundation)) {
                isExterior = true;
                {
                    auto const SELECT_CASE_var(Surface(iSurf).Class);
                    if ((SELECT_CASE_var == SurfaceClass_Wall) || (SELECT_CASE_var == SurfaceClass_Floor) || (SELECT_CASE_var == SurfaceClass_Roof)) {
                        surfName = Surface(iSurf).Name;
                        curCons = Surface(iSurf).Construction;
                        PreDefTableEntry(pdchOpCons, surfName, dataConstruction.Construct(curCons).Name);
                        PreDefTableEntry(pdchOpRefl, surfName, 1 - dataConstruction.Construct(curCons).OutsideAbsorpSolar);
                        PreDefTableEntry(pdchOpUfactNoFilm, surfName, NominalU(Surface(iSurf).Construction), 3);
                        mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier;
                        PreDefTableEntry(pdchOpGrArea, surfName, Surface(iSurf).GrossArea * mult);
                        computedNetArea(iSurf) += Surface(iSurf).GrossArea * mult;
                        curAzimuth = Surface(iSurf).Azimuth;
                        // Round to two decimals, like the display in tables
                        // (PreDefTableEntry uses a fortran style write, that rounds rather than trim)
                        curAzimuth = round(curAzimuth * 100.0) / 100.0;
                        PreDefTableEntry(pdchOpAzimuth, surfName, curAzimuth);
                        curTilt = Surface(iSurf).Tilt;
                        PreDefTableEntry(pdchOpTilt, surfName, curTilt);
                        if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                            if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                                PreDefTableEntry(pdchOpDir, surfName, "N");
                            } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                                PreDefTableEntry(pdchOpDir, surfName, "E");
                            } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                                PreDefTableEntry(pdchOpDir, surfName, "S");
                            } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                                PreDefTableEntry(pdchOpDir, surfName, "W");
                            }
                        }
                    } else if ((SELECT_CASE_var == SurfaceClass_Window) || (SELECT_CASE_var == SurfaceClass_TDD_Dome)) {
                        surfName = Surface(iSurf).Name;
                        curCons = Surface(iSurf).Construction;
                        PreDefTableEntry(pdchFenCons, surfName, dataConstruction.Construct(curCons).Name);
                        zonePt = Surface(iSurf).Zone;
                        mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier * Surface(iSurf).Multiplier;
                        // include the frame area if present
                        windowArea = Surface(iSurf).GrossArea;
                        frameArea = 0.0;
                        dividerArea = 0.0;
                        frameDivNum = Surface(iSurf).FrameDivider;
                        if (frameDivNum != 0) {
                            frameWidth = FrameDivider(frameDivNum).FrameWidth;
                            frameArea = (Surface(iSurf).Height + 2.0 * frameWidth) * (Surface(iSurf).Width + 2.0 * frameWidth) -
                                        (Surface(iSurf).Height * Surface(iSurf).Width);
                            windowArea += frameArea;
                            dividerArea = FrameDivider(frameDivNum).DividerWidth *
                                          (FrameDivider(frameDivNum).HorDividers * Surface(iSurf).Width +
                                           FrameDivider(frameDivNum).VertDividers * Surface(iSurf).Height -
                                           FrameDivider(frameDivNum).HorDividers * FrameDivider(frameDivNum).VertDividers *
                                               FrameDivider(frameDivNum).DividerWidth);
                            PreDefTableEntry(pdchFenFrameConductance, surfName, FrameDivider(frameDivNum).FrameConductance, 3);
                            PreDefTableEntry(pdchFenDividerConductance, surfName, FrameDivider(frameDivNum).DividerConductance, 3);
                        }
                        windowAreaWMult = windowArea * mult;
                        PreDefTableEntry(pdchFenAreaOf1, surfName, windowArea);
                        PreDefTableEntry(pdchFenFrameAreaOf1, surfName, frameArea);
                        PreDefTableEntry(pdchFenDividerAreaOf1, surfName, dividerArea);
                        PreDefTableEntry(pdchFenGlassAreaOf1, surfName, windowArea - (frameArea + dividerArea));
                        PreDefTableEntry(pdchFenArea, surfName, windowAreaWMult);
                        computedNetArea(Surface(iSurf).BaseSurf) -= windowAreaWMult;
                        nomUfact = NominalU(Surface(iSurf).Construction);
                        PreDefTableEntry(pdchFenUfact, surfName, nomUfact, 3);
                        // if the construction report is requested the SummerSHGC is already calculated
                        if (dataConstruction.Construct(curCons).SummerSHGC != 0) {
                            SHGCSummer = dataConstruction.Construct(curCons).SummerSHGC;
                            TransVisNorm = dataConstruction.Construct(curCons).VisTransNorm;
                        } else {
                            // must calculate Summer SHGC
                            if (!dataConstruction.Construct(curCons).WindowTypeEQL) {
                                CalcNominalWindowCond(state, curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag);
                            }
                        }
                        PreDefTableEntry(pdchFenSHGC, surfName, SHGCSummer, 3);
                        PreDefTableEntry(pdchFenVisTr, surfName, TransVisNorm, 3);
                        PreDefTableEntry(pdchFenParent, surfName, Surface(iSurf).BaseSurfName);
                        curAzimuth = Surface(iSurf).Azimuth;
                        // Round to two decimals, like the display in tables
                        curAzimuth = round(curAzimuth * 100.0) / 100.0;
                        PreDefTableEntry(pdchFenAzimuth, surfName, curAzimuth);
                        isNorth = false;
                        curTilt = Surface(iSurf).Tilt;
                        PreDefTableEntry(pdchFenTilt, surfName, curTilt);
                        if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                            if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                                PreDefTableEntry(pdchFenDir, surfName, "N");
                                isNorth = true;
                            } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                                PreDefTableEntry(pdchFenDir, surfName, "E");
                            } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                                PreDefTableEntry(pdchFenDir, surfName, "S");
                            } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                                PreDefTableEntry(pdchFenDir, surfName, "W");
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
                            PreDefTableEntry(pdchFenSwitchable, surfName, "Yes");
                            // shading report
                            PreDefTableEntry(pdchWscName, surfName, WindowShadingControl(curWSC).Name);
                            {
                                auto const SELECT_CASE_var1(WindowShadingControl(curWSC).ShadingType);
                                if (SELECT_CASE_var1 == WSC_ST_NoShade) {
                                    PreDefTableEntry(pdchWscShading, surfName, "No Shade");
                                } else if (SELECT_CASE_var1 == WSC_ST_InteriorShade) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Interior Shade");
                                } else if (SELECT_CASE_var1 == WSC_ST_SwitchableGlazing) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Switchable Glazing");
                                } else if (SELECT_CASE_var1 == WSC_ST_ExteriorShade) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Exterior Shade");
                                } else if (SELECT_CASE_var1 == WSC_ST_InteriorBlind) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Interior Blind");
                                } else if (SELECT_CASE_var1 == WSC_ST_ExteriorBlind) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Exterior Blind");
                                } else if (SELECT_CASE_var1 == WSC_ST_BetweenGlassShade) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Between Glass Shade");
                                } else if (SELECT_CASE_var1 == WSC_ST_BetweenGlassBlind) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Between Glass Blind");
                                } else if (SELECT_CASE_var1 == WSC_ST_ExteriorScreen) {
                                    PreDefTableEntry(pdchWscShading, surfName, "Exterior Screen");
                                }
                            }
                            {
                                auto const SELECT_CASE_var1(WindowShadingControl(curWSC).ShadingControlType);
                                if (SELECT_CASE_var1 == WSCT_AlwaysOn) {
                                    PreDefTableEntry(pdchWscControl, surfName, "AlwaysOn");
                                } else if (SELECT_CASE_var1 == WSCT_AlwaysOff) {
                                    PreDefTableEntry(pdchWscControl, surfName, "AlwaysOff");
                                } else if (SELECT_CASE_var1 == WSCT_OnIfScheduled) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfScheduleAllows");
                                } else if (SELECT_CASE_var1 == WSCT_HiSolar) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighSolarOnWindow");
                                } else if (SELECT_CASE_var1 == WSCT_HiHorzSolar) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighHorizontalSolar");
                                } else if (SELECT_CASE_var1 == WSCT_HiOutAirTemp) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighOutdoorAirTemperature");
                                } else if (SELECT_CASE_var1 == WSCT_HiZoneAirTemp) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighZoneAirTemperature");
                                } else if (SELECT_CASE_var1 == WSCT_HiZoneCooling) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighZoneCooling");
                                } else if (SELECT_CASE_var1 == WSCT_HiGlare) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighGlare");
                                } else if (SELECT_CASE_var1 == WSCT_MeetDaylIlumSetp) {
                                    PreDefTableEntry(pdchWscControl, surfName, "MeetDaylightIlluminanceSetpoint");
                                } else if (SELECT_CASE_var1 == WSCT_OnNightLoOutTemp_OffDay) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightIfLowOutdoorTempAndOffDay");
                                } else if (SELECT_CASE_var1 == WSCT_OnNightLoInTemp_OffDay) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightIfLowInsideTempAndOffDay");
                                } else if (SELECT_CASE_var1 == WSCT_OnNightIfHeating_OffDay) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightIfHeatingAndOffDay");
                                } else if (SELECT_CASE_var1 == WSCT_OnNightLoOutTemp_OnDayCooling) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightIfLowOutdoorTempAndOnDayIfCooling");
                                } else if (SELECT_CASE_var1 == WSCT_OnNightIfHeating_OnDayCooling) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightIfHeatingAndOnDayIfCooling");
                                } else if (SELECT_CASE_var1 == WSCT_OffNight_OnDay_HiSolarWindow) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OffNightAndOnDayIfCoolingAndHighSolarOnWindow");
                                } else if (SELECT_CASE_var1 == WSCT_OnNight_OnDay_HiSolarWindow) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnNightAndOnDayIfCoolingAndHighSolarOnWindow");
                                } else if (SELECT_CASE_var1 == WSCT_OnHiOutTemp_HiSolarWindow) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighOutdoorAirTempAndHighSolarOnWindow");
                                } else if (SELECT_CASE_var1 == WSCT_OnHiOutTemp_HiHorzSolar) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighOutdoorAirTempAndHighHorizontalSolar");
                                } else if (SELECT_CASE_var1 == WSCT_OnHiZoneTemp_HiSolarWindow) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighZoneAirTempAndHighSolarOnWindow");
                                } else if (SELECT_CASE_var1 == WSCT_OnHiZoneTemp_HiHorzSolar) {
                                    PreDefTableEntry(pdchWscControl, surfName, "OnIfHighZoneAirTempAndHighHorizontalSolar");
                                }
                            }

                            // output list of all possible shading contructions for shaded windows including those with storms
                            std::string names = "";
                            for (auto construction : Surface(iSurf).shadedConstructionList) {
                                if (!names.empty()) names.append("; ");
                                names.append(dataConstruction.Construct(construction).Name);
                            }
                            for (auto construction : Surface(iSurf).shadedStormWinConstructionList) {
                                if (!names.empty()) names.append("; ");
                                names.append(dataConstruction.Construct(construction).Name);
                            }
                            PreDefTableEntry(pdchWscShadCons, surfName, names);

                            if (WindowShadingControl(curWSC).GlareControlIsActive) {
                                PreDefTableEntry(pdchWscGlare, surfName, "Yes");
                            } else {
                                PreDefTableEntry(pdchWscGlare, surfName, "No");
                            }
                        } else {
                            PreDefTableEntry(pdchFenSwitchable, surfName, "No");
                        }
                    } else if (SELECT_CASE_var == SurfaceClass_Door) {
                        surfName = Surface(iSurf).Name;
                        curCons = Surface(iSurf).Construction;
                        PreDefTableEntry(pdchDrCons, surfName, dataConstruction.Construct(curCons).Name);
                        PreDefTableEntry(pdchDrUfactNoFilm, surfName, NominalU(Surface(iSurf).Construction), 3);
                        mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier;
                        PreDefTableEntry(pdchDrGrArea, surfName, Surface(iSurf).GrossArea * mult);
                        PreDefTableEntry(pdchDrParent, surfName, Surface(iSurf).BaseSurfName);
                        computedNetArea(Surface(iSurf).BaseSurf) -= Surface(iSurf).GrossArea * mult;
                    }
                }
            } else {
                // interior surfaces
                isExterior = false;
                if((Surface(iSurf).Class == SurfaceClass_Wall) || (Surface(iSurf).Class == SurfaceClass_Floor) || (Surface(iSurf).Class == SurfaceClass_Roof)) {
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(pdchIntOpCons, surfName, dataConstruction.Construct(curCons).Name);
                    PreDefTableEntry(pdchIntOpRefl, surfName, 1 - dataConstruction.Construct(curCons).OutsideAbsorpSolar);
                    PreDefTableEntry(pdchIntOpUfactNoFilm, surfName, NominalU(Surface(iSurf).Construction), 3);
                    mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier;
                    PreDefTableEntry(pdchIntOpGrArea, surfName, Surface(iSurf).GrossArea * mult);
                    computedNetArea(iSurf) += Surface(iSurf).GrossArea * mult;
                    curAzimuth = Surface(iSurf).Azimuth;
                    // Round to two decimals, like the display in tables
                    // (PreDefTableEntry uses a fortran style write, that rounds rather than trim)
                    curAzimuth = round(curAzimuth * 100.0) / 100.0;
                    PreDefTableEntry(pdchIntOpAzimuth, surfName, curAzimuth);
                    curTilt = Surface(iSurf).Tilt;
                    PreDefTableEntry(pdchIntOpTilt, surfName, curTilt);
                    if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                        if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                            PreDefTableEntry(pdchIntOpDir, surfName, "N");
                        } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                            PreDefTableEntry(pdchIntOpDir, surfName, "E");
                        } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                            PreDefTableEntry(pdchIntOpDir, surfName, "S");
                        } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                            PreDefTableEntry(pdchIntOpDir, surfName, "W");
                        }
                    }
                // interior window report
                }else if (((Surface(iSurf).Class == SurfaceClass_Window) || (Surface(iSurf).Class == SurfaceClass_TDD_Dome)) && (!dataConstruction.Construct(Surface(iSurf).Construction).TypeIsAirBoundaryInteriorWindow)) {
                    if (!has_prefix(Surface(iSurf).Name, "iz-")) { // don't count created interzone surfaces that are mirrors of other surfaces
                        surfName = Surface(iSurf).Name;
                        curCons = Surface(iSurf).Construction;
                        PreDefTableEntry(pdchIntFenCons, surfName, dataConstruction.Construct(curCons).Name);
                        zonePt = Surface(iSurf).Zone;
                        mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier * Surface(iSurf).Multiplier;
                        // include the frame area if present
                        windowArea = Surface(iSurf).GrossArea;
                        if (Surface(iSurf).FrameDivider != 0) {
                            frameWidth = FrameDivider(Surface(iSurf).FrameDivider).FrameWidth;
                            frameArea = (Surface(iSurf).Height + 2 * frameWidth) * (Surface(iSurf).Width + 2 * frameWidth) -
                                        (Surface(iSurf).Height * Surface(iSurf).Width);
                            windowArea += frameArea;
                        }
                        windowAreaWMult = windowArea * mult;
                        PreDefTableEntry(pdchIntFenAreaOf1, surfName, windowArea);
                        PreDefTableEntry(pdchIntFenArea, surfName, windowAreaWMult);
                        computedNetArea(Surface(iSurf).BaseSurf) -= windowAreaWMult;
                        nomUfact = NominalU(Surface(iSurf).Construction);
                        PreDefTableEntry(pdchIntFenUfact, surfName, nomUfact, 3);
                        // if the construction report is requested the SummerSHGC is already calculated
                        if (dataConstruction.Construct(curCons).SummerSHGC != 0) {
                            SHGCSummer = dataConstruction.Construct(curCons).SummerSHGC;
                            TransVisNorm = dataConstruction.Construct(curCons).VisTransNorm;
                        } else {
                            // must calculate Summer SHGC
                            if (!dataConstruction.Construct(curCons).WindowTypeEQL) {
                                CalcNominalWindowCond(state, curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag);
                            }
                        }
                        PreDefTableEntry(pdchIntFenSHGC, surfName, SHGCSummer, 3);
                        PreDefTableEntry(pdchIntFenVisTr, surfName, TransVisNorm, 3);
                        PreDefTableEntry(pdchIntFenParent, surfName, Surface(iSurf).BaseSurfName);
                        // compute totals for area weighted averages
                        intFenTotArea += windowAreaWMult;
                        intUfactArea += nomUfact * windowAreaWMult;
                        intShgcArea += SHGCSummer * windowAreaWMult;
                        intVistranArea += TransVisNorm * windowAreaWMult;
                    }
                }else if (Surface(iSurf).Class == SurfaceClass_Door) {
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(pdchIntDrCons, surfName, dataConstruction.Construct(curCons).Name);
                    PreDefTableEntry(pdchIntDrUfactNoFilm, surfName, NominalU(Surface(iSurf).Construction), 3);
                    mult = Zone(zonePt).Multiplier * Zone(zonePt).ListMultiplier;
                    PreDefTableEntry(pdchIntDrGrArea, surfName, Surface(iSurf).GrossArea * mult);
                    PreDefTableEntry(pdchIntDrParent, surfName, Surface(iSurf).BaseSurfName);
                    computedNetArea(Surface(iSurf).BaseSurf) -= Surface(iSurf).GrossArea * mult;
                }
            }
            if ((Surface(iSurf).Class <= 20) && (Surface(iSurf).Class >= 1)) {
                ++numSurfaces(Surface(iSurf).Class);
                if (isExterior) {
                    ++numExtSurfaces(Surface(iSurf).Class);
                }
                if (Surface(iSurf).Class == SurfaceClass_Window) {
                    if (SurfWinOriginalClass(iSurf) == SurfaceClass_GlassDoor ||
                        SurfWinOriginalClass(iSurf) == SurfaceClass_TDD_Diffuser) {
                        ++numSurfaces(SurfWinOriginalClass(iSurf));
                        if (isExterior) {
                            ++numExtSurfaces(SurfWinOriginalClass(iSurf));
                        }
                    }
                }
            }
        }
        // for fins and overhangs just add them explicitly since not otherwise classified
        int totOverhangs = inputProcessor->getNumObjectsFound("Shading:Overhang") + inputProcessor->getNumObjectsFound("Shading:Overhang:Projection");
        numSurfaces(SurfaceClass_Overhang) = totOverhangs;
        numExtSurfaces(SurfaceClass_Overhang) = totOverhangs;
        int totFins = inputProcessor->getNumObjectsFound("Shading:Fin") + inputProcessor->getNumObjectsFound("Shading:Fin:Projection");
        numSurfaces(SurfaceClass_Fin) = totFins;
        numExtSurfaces(SurfaceClass_Fin) = totFins;
        // go through all the surfaces again and this time insert the net area results
        for (int iSurf : DataSurfaces::AllSurfaceListReportOrder) {
            zonePt = Surface(iSurf).Zone;
            auto const SurfaceClass(Surface(iSurf).Class);
            // exterior surfaces including underground
            if ((Surface(iSurf).ExtBoundCond == ExternalEnvironment) || (Surface(iSurf).ExtBoundCond == Ground) ||
                (Surface(iSurf).ExtBoundCond == GroundFCfactorMethod) || (Surface(iSurf).ExtBoundCond == KivaFoundation)) {
                if ((SurfaceClass == SurfaceClass_Wall) || (SurfaceClass == SurfaceClass_Floor) || (SurfaceClass == SurfaceClass_Roof)) {
                    surfName = Surface(iSurf).Name;
                    PreDefTableEntry(pdchOpNetArea, surfName, computedNetArea(iSurf));
                }
            }else {
                if ((SurfaceClass == SurfaceClass_Wall) || (SurfaceClass == SurfaceClass_Floor) || (SurfaceClass == SurfaceClass_Roof)) {
                    surfName = Surface(iSurf).Name;
                    PreDefTableEntry(pdchIntOpNetArea, surfName, computedNetArea(iSurf));
                }
            }// interior surfaces
        }
        // total
        PreDefTableEntry(pdchFenArea, "Total or Average", fenTotArea);
        if (fenTotArea > 0.0) {
            PreDefTableEntry(pdchFenUfact, "Total or Average", ufactArea / fenTotArea, 3);
            PreDefTableEntry(pdchFenSHGC, "Total or Average", shgcArea / fenTotArea, 3);
            PreDefTableEntry(pdchFenVisTr, "Total or Average", vistranArea / fenTotArea, 3);
        } else {
            PreDefTableEntry(pdchFenUfact, "Total or Average", "-");
            PreDefTableEntry(pdchFenSHGC, "Total or Average", "-");
            PreDefTableEntry(pdchFenVisTr, "Total or Average", "-");
        }
        // north
        PreDefTableEntry(pdchFenArea, "North Total or Average", fenTotAreaNorth);
        if (fenTotAreaNorth > 0.0) {
            PreDefTableEntry(pdchFenUfact, "North Total or Average", ufactAreaNorth / fenTotAreaNorth, 3);
            PreDefTableEntry(pdchFenSHGC, "North Total or Average", shgcAreaNorth / fenTotAreaNorth, 3);
            PreDefTableEntry(pdchFenVisTr, "North Total or Average", vistranAreaNorth / fenTotAreaNorth, 3);
        } else {
            PreDefTableEntry(pdchFenUfact, "North Total or Average", "-");
            PreDefTableEntry(pdchFenSHGC, "North Total or Average", "-");
            PreDefTableEntry(pdchFenVisTr, "North Total or Average", "-");
        }
        // non-north
        PreDefTableEntry(pdchFenArea, "Non-North Total or Average", fenTotAreaNonNorth);
        if (fenTotAreaNonNorth > 0.0) {
            PreDefTableEntry(pdchFenUfact, "Non-North Total or Average", ufactAreaNonNorth / fenTotAreaNonNorth, 3);
            PreDefTableEntry(pdchFenSHGC, "Non-North Total or Average", shgcAreaNonNorth / fenTotAreaNonNorth, 3);
            PreDefTableEntry(pdchFenVisTr, "Non-North Total or Average", vistranAreaNonNorth / fenTotAreaNonNorth, 3);
        } else {
            PreDefTableEntry(pdchFenUfact, "Non-North Total or Average", "-");
            PreDefTableEntry(pdchFenSHGC, "Non-North Total or Average", "-");
            PreDefTableEntry(pdchFenVisTr, "Non-North Total or Average", "-");
        }
        // interior fenestration totals
        PreDefTableEntry(pdchIntFenArea, "Total or Average", intFenTotArea);
        if (intFenTotArea > 0.0) {
            PreDefTableEntry(pdchIntFenUfact, "Total or Average", intUfactArea / intFenTotArea, 3);
            PreDefTableEntry(pdchIntFenSHGC, "Total or Average", intShgcArea / intFenTotArea, 3);
            PreDefTableEntry(pdchIntFenVisTr, "Total or Average", intVistranArea / intFenTotArea, 3);
        } else {
            PreDefTableEntry(pdchIntFenUfact, "Total or Average", "-");
            PreDefTableEntry(pdchIntFenSHGC, "Total or Average", "-");
            PreDefTableEntry(pdchIntFenVisTr, "Total or Average", "-");
        }
        // counts
        PreDefTableEntry(pdchSurfCntTot, "Wall", numSurfaces(SurfaceClass_Wall));
        PreDefTableEntry(pdchSurfCntExt, "Wall", numExtSurfaces(SurfaceClass_Wall));
        PreDefTableEntry(pdchSurfCntTot, "Floor", numSurfaces(SurfaceClass_Floor));
        PreDefTableEntry(pdchSurfCntExt, "Floor", numExtSurfaces(SurfaceClass_Floor));
        PreDefTableEntry(pdchSurfCntTot, "Roof", numSurfaces(SurfaceClass_Roof));
        PreDefTableEntry(pdchSurfCntExt, "Roof", numExtSurfaces(SurfaceClass_Roof));
        PreDefTableEntry(pdchSurfCntTot, "Internal Mass", numSurfaces(SurfaceClass_IntMass));
        PreDefTableEntry(pdchSurfCntExt, "Internal Mass", numExtSurfaces(SurfaceClass_IntMass));
        PreDefTableEntry(pdchSurfCntTot, "Building Detached Shading", numSurfaces(SurfaceClass_Detached_B));
        PreDefTableEntry(pdchSurfCntExt, "Building Detached Shading", numExtSurfaces(SurfaceClass_Detached_B));
        PreDefTableEntry(pdchSurfCntTot, "Fixed Detached Shading", numSurfaces(SurfaceClass_Detached_F));
        PreDefTableEntry(pdchSurfCntExt, "Fixed Detached Shading", numExtSurfaces(SurfaceClass_Detached_F));
        PreDefTableEntry(pdchSurfCntTot, "Window", numSurfaces(SurfaceClass_Window));
        PreDefTableEntry(pdchSurfCntExt, "Window", numExtSurfaces(SurfaceClass_Window));
        PreDefTableEntry(pdchSurfCntTot, "Door", numSurfaces(SurfaceClass_Door));
        PreDefTableEntry(pdchSurfCntExt, "Door", numExtSurfaces(SurfaceClass_Door));
        PreDefTableEntry(pdchSurfCntTot, "Glass Door", numSurfaces(SurfaceClass_GlassDoor));
        PreDefTableEntry(pdchSurfCntExt, "Glass Door", numExtSurfaces(SurfaceClass_GlassDoor));
        PreDefTableEntry(pdchSurfCntTot, "Shading", numSurfaces(SurfaceClass_Shading));
        PreDefTableEntry(pdchSurfCntExt, "Shading", numExtSurfaces(SurfaceClass_Shading));
        PreDefTableEntry(pdchSurfCntTot, "Overhang", numSurfaces(SurfaceClass_Overhang));
        PreDefTableEntry(pdchSurfCntExt, "Overhang", numExtSurfaces(SurfaceClass_Overhang));
        PreDefTableEntry(pdchSurfCntTot, "Fin", numSurfaces(SurfaceClass_Fin));
        PreDefTableEntry(pdchSurfCntExt, "Fin", numExtSurfaces(SurfaceClass_Fin));
        PreDefTableEntry(pdchSurfCntTot, "Tubular Daylighting Device Dome", numSurfaces(SurfaceClass_TDD_Dome));
        PreDefTableEntry(pdchSurfCntExt, "Tubular Daylighting Device Dome", numExtSurfaces(SurfaceClass_TDD_Dome));
        PreDefTableEntry(pdchSurfCntTot, "Tubular Daylighting Device Diffuser", numSurfaces(SurfaceClass_TDD_Diffuser));
        PreDefTableEntry(pdchSurfCntExt, "Tubular Daylighting Device Diffuser", numExtSurfaces(SurfaceClass_TDD_Diffuser));
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

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        // FLOW:

        // Use the total number of surfaces to allocate variables to avoid a surface number limit
        CTFConstInPart.dimension(TotSurfaces, 0.0);
        CTFConstOutPart.dimension(TotSurfaces, 0.0);
        CTFCross0.dimension(TotSurfaces, 0.0);
        CTFInside0.dimension(TotSurfaces, 0.0);
        TH11Surf.dimension(TotSurfaces, 0.0);
        CTFSourceIn0.dimension(TotSurfaces, 0.0);
        QsrcHistSurf1.dimension(TotSurfaces, 0.0);
        IsAdiabatic.dimension(TotSurfaces, 0);
        IsNotAdiabatic.dimension(TotSurfaces, 0);
        IsSource.dimension(TotSurfaces, 0);
        IsNotSource.dimension(TotSurfaces, 0);
        IsPoolSurf.dimension(TotSurfaces, 0);
        IsNotPoolSurf.dimension(TotSurfaces, 0);
        TempTermSurf.dimension(TotSurfaces, 0);
        TempDivSurf.dimension(TotSurfaces, 0);
        if (AnyConstructInternalSourceInInput) {
            CTFTsrcConstPart.dimension(TotSurfaces, 0.0);
            CTFTuserConstPart.dimension(TotSurfaces, 0.0);
        }
        TempEffBulkAir.dimension(TotSurfaces, 23.0);
        HConvIn.dimension(TotSurfaces, 0.0);
        HcExtSurf.dimension(TotSurfaces, 0.0);
        HAirExtSurf.dimension(TotSurfaces, 0.0);
        HSkyExtSurf.dimension(TotSurfaces, 0.0);
        HGrdExtSurf.dimension(TotSurfaces, 0.0);

        TempSurfIn.dimension(TotSurfaces, 0.0);
        TempInsOld.dimension(TotSurfaces, 0.0);
        TempSurfInTmp.dimension(TotSurfaces, 0.0);
        TempSurfInTmp.dimension(TotSurfaces, 0.0);
        RefAirTemp.dimension(TotSurfaces, 0.0);
        SurfQRadLWOutSrdSurfs.dimension(TotSurfaces, 0.0);

        SurfWinQRadSWwinAbs.dimension(DataWindowEquivalentLayer::CFSMAXNL + 1, TotSurfaces, 0.0);
        SurfWinInitialDifSolwinAbs.dimension(DataWindowEquivalentLayer::CFSMAXNL, TotSurfaces, 0.0);
        SurfQRadSWOutMvIns.dimension(TotSurfaces, 0.0);
        SurfQRadThermInAbs.dimension(TotSurfaces, 0.0);
        SurfQAdditionalHeatSourceOutside.dimension(TotSurfaces, 0.0);
        SurfQAdditionalHeatSourceInside.dimension(TotSurfaces, 0.0);
        SUMH.dimension(TotSurfaces, 0);

        TH.dimension(2, Construction::MaxCTFTerms, TotSurfaces, 0.0);
        SurfTempSurfOut.dimension(TotSurfaces, 0.0);
        TempSurfInRep.dimension(TotSurfaces, 0.0);
        TempSurfInMovInsRep.dimension(TotSurfaces, 0.0);
        QConvInReport.dimension(TotSurfaces, 0.0);
        QdotConvInRepPerArea.dimension(TotSurfaces, 0.0);
        QdotConvInRep.dimension(TotSurfaces, 0.0);

        QRadNetSurfInReport.dimension(TotSurfaces, 0.0);
        QdotRadNetSurfInRep.dimension(TotSurfaces, 0.0);
        QdotRadNetSurfInRepPerArea.dimension(TotSurfaces, 0.0);

        QRadSolarInReport.dimension(TotSurfaces, 0.0);
        QdotRadSolarInRep.dimension(TotSurfaces, 0.0);
        QdotRadSolarInRepPerArea.dimension(TotSurfaces, 0.0);

        QRadLightsInReport.dimension(TotSurfaces, 0.0);
        QdotRadLightsInRep.dimension(TotSurfaces, 0.0);
        QdotRadLightsInRepPerArea.dimension(TotSurfaces, 0.0);

        QRadIntGainsInReport.dimension(TotSurfaces, 0.0);
        QdotRadIntGainsInRep.dimension(TotSurfaces, 0.0);
        QdotRadIntGainsInRepPerArea.dimension(TotSurfaces, 0.0);

        QRadHVACInReport.dimension(TotSurfaces, 0.0);
        QdotRadHVACInRep.dimension(TotSurfaces, 0.0);
        QdotRadHVACInRepPerArea.dimension(TotSurfaces, 0.0);

        QConvOutReport.dimension(TotSurfaces, 0.0);
        QdotConvOutRepPerArea.dimension(TotSurfaces, 0.0);
        QdotConvOutRep.dimension(TotSurfaces, 0.0);

        QdotRadOutRep.dimension(TotSurfaces, 0.0);
        QdotRadOutRepPerArea.dimension(TotSurfaces, 0.0);
        QRadOutReport.dimension(TotSurfaces, 0.0);

        QAirExtReport.dimension(TotSurfaces, 0.0);
        QHeatEmiReport.dimension(TotSurfaces, 0.0);

        SurfOpaqSWOutAbsTotalReport.dimension(TotSurfaces, 0.0);
        SurfOpaqSWOutAbsEnergyReport.dimension(TotSurfaces, 0.0);

        SurfOpaqInsFaceConduction.dimension(TotSurfaces, 0.0);
        SurfOpaqInsFaceConductionFlux.dimension(TotSurfaces, 0.0);
        SurfOpaqInsFaceCondGainRep.dimension(TotSurfaces, 0.0);
        SurfOpaqInsFaceCondLossRep.dimension(TotSurfaces, 0.0);
        SurfOpaqInsFaceConductionEnergy.dimension(TotSurfaces, 0.0);

        SurfOpaqOutsideFaceConduction.dimension(TotSurfaces, 0.0);
        SurfOpaqOutsideFaceConductionFlux.dimension(TotSurfaces, 0.0);
        SurfOpaqExtFaceCondGainRep.dimension(TotSurfaces, 0.0);
        SurfOpaqExtFaceCondLossRep.dimension(TotSurfaces, 0.0);
        SurfOpaqOutsideFaceConductionEnergy.dimension(TotSurfaces, 0.0);

        SurfOpaqAvgFaceCondGainRep.dimension(TotSurfaces, 0.0);
        SurfOpaqAvgFaceCondLossRep.dimension(TotSurfaces, 0.0);
        SurfOpaqAvgFaceConduction.dimension(TotSurfaces, 0.0);
        SurfOpaqAvgFaceConductionFlux.dimension(TotSurfaces, 0.0);
        SurfOpaqAvgFaceConductionEnergy.dimension(TotSurfaces, 0.0);

        SurfOpaqStorageGainRep.dimension(TotSurfaces, 0.0);
        SurfOpaqStorageCondLossRep.dimension(TotSurfaces, 0.0);
        SurfOpaqStorageConduction.dimension(TotSurfaces, 0.0);
        SurfOpaqStorageConductionFlux.dimension(TotSurfaces, 0.0);
        SurfOpaqStorageConductionEnergy.dimension(TotSurfaces, 0.0);

        SurfOpaqInsFaceBeamSolAbsorbed.dimension(TotSurfaces, 0.0);

        SurfOpaqQRadSWOutAbs.dimension(TotSurfaces, 0.0);
        SurfOpaqQRadSWInAbs.dimension(TotSurfaces, 0.0);

        SurfOpaqInitialDifSolInAbs.dimension(TotSurfaces, 0.0);
        SurfWinInitialDifSolInTrans.dimension(TotSurfaces, 0.0);

        QH.dimension(2, Construction::MaxCTFTerms, TotSurfaces, 0.0);
        THM.dimension(2, Construction::MaxCTFTerms, TotSurfaces, 0.0);
        QHM.dimension(2, Construction::MaxCTFTerms, TotSurfaces, 0.0);
        if (AnyConstructInternalSourceInInput) {
            TempSource.dimension(TotSurfaces, 0.0);
            TempUserLoc.dimension(TotSurfaces, 0.0);
            TsrcHist.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
            TuserHist.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
            QsrcHist.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
            TsrcHistM.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
            TuserHistM.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
            QsrcHistM.dimension(TotSurfaces, Construction::MaxCTFTerms, 0.0);
        }

        RadSysTiHBConstCoef.dimension(TotSurfaces, 0.0);
        RadSysTiHBToutCoef.dimension(TotSurfaces, 0.0);
        RadSysTiHBQsrcCoef.dimension(TotSurfaces, 0.0);
        RadSysToHBConstCoef.dimension(TotSurfaces, 0.0);
        RadSysToHBTinCoef.dimension(TotSurfaces, 0.0);
        RadSysToHBQsrcCoef.dimension(TotSurfaces, 0.0);
        QRadSysSource.dimension(TotSurfaces, 0.0);
        TCondFDSourceNode.dimension(TotSurfaces, 15.0);
        QHTRadSysSurf.dimension(TotSurfaces, 0.0);
        QHWBaseboardSurf.dimension(TotSurfaces, 0.0);
        QSteamBaseboardSurf.dimension(TotSurfaces, 0.0);
        QElecBaseboardSurf.dimension(TotSurfaces, 0.0);
        QCoolingPanelSurf.dimension(TotSurfaces, 0.0);
        QRadSurfAFNDuct.dimension(TotSurfaces, 0.0);

        // allocate terms used for pool surface heat balance
        QPoolSurfNumerator.dimension(TotSurfaces, 0.0);
        PoolHeatTransCoefs.dimension(TotSurfaces, 0.0);

        // allocate term used as sink for PV electricity
        QPVSysSource.dimension(TotSurfaces, 0.0);

        // Allocate the moisture balance arrays
        TempOutsideAirFD.dimension(TotSurfaces, 0.0);
        RhoVaporAirOut.dimension(TotSurfaces, 0.0);
        RhoVaporSurfIn.dimension(TotSurfaces, 0.0);
        RhoVaporAirIn.dimension(TotSurfaces, 0.0);
        HConvExtFD.dimension(TotSurfaces, 0.0);
        HMassConvExtFD.dimension(TotSurfaces, 0.0);
        HConvInFD.dimension(TotSurfaces, 0.0);
        HMassConvInFD.dimension(TotSurfaces, 0.0);
        HSkyFD.dimension(TotSurfaces, 0.0);
        HGrndFD.dimension(TotSurfaces, 0.0);
        HAirFD.dimension(TotSurfaces, 0.0);

        SurfNetLWRadToSurf.dimension(TotSurfaces, 0.0);
        SurfOpaqQRadSWLightsInAbs.dimension(TotSurfaces, 0.0);
        SurfSkySolarInc.dimension(TotSurfaces, 0);
        SurfGndSolarInc.dimension(TotSurfaces, 0);

        DisplayString("Setting up Surface Reporting Variables");

        // Setup surface report variables CurrentModuleObject='Opaque Surfaces'
        for (int loop = 1; loop <= TotSurfaces; ++loop) {
            if (!Surface(loop).HeatTransSurf) continue;
            SetupOutputVariable(state,
                "Surface Inside Face Temperature", OutputProcessor::Unit::C, TempSurfInRep(loop), "Zone", "State", Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Interior Movable Insulation Temperature",
                                OutputProcessor::Unit::C,
                                TempSurfInMovInsRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);

            if (Surface(loop).ExtBoundCond != KivaFoundation) {
                SetupOutputVariable(state,
                    "Surface Outside Face Temperature", OutputProcessor::Unit::C, SurfTempSurfOut(loop), "Zone", "State", Surface(loop).Name);
            }

            SetupOutputVariable(state,
                "Surface Inside Face Adjacent Air Temperature", OutputProcessor::Unit::C, TempEffBulkAir(loop), "Zone", "State", Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Convection Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                HConvIn(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                "Surface Inside Face Convection Heat Gain Rate", OutputProcessor::Unit::W, QdotConvInRep(loop), "Zone", "State", Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Convection Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                QdotConvInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                "Surface Inside Face Convection Heat Gain Energy", OutputProcessor::Unit::J, QConvInReport(loop), "Zone", "Sum", Surface(loop).Name);

            SetupOutputVariable(state, "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                QdotRadNetSurfInRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                QdotRadNetSurfInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Net Surface Thermal Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                QRadNetSurfInReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);

            if (Surface(loop).Class != SurfaceClass_Window) {
                SetupOutputVariable(state, "Surface Inside Face Solar Radiation Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    QdotRadSolarInRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Solar Radiation Heat Gain Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    QdotRadSolarInRepPerArea(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Solar Radiation Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    QRadSolarInReport(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);

                SetupOutputVariable(state, "Surface Inside Face Lights Radiation Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    QdotRadLightsInRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Lights Radiation Heat Gain Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    QdotRadLightsInRepPerArea(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Lights Radiation Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    QRadLightsInReport(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);
            }

            SetupOutputVariable(state, "Surface Inside Face Internal Gains Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                QdotRadIntGainsInRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Internal Gains Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                QdotRadIntGainsInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Internal Gains Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                QRadIntGainsInReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);

            SetupOutputVariable(state, "Surface Inside Face System Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                QdotRadHVACInRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face System Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                QdotRadHVACInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face System Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                QRadHVACInReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);

            if (Surface(loop).ExtBoundCond == ExternalEnvironment || DisplayAdvancedReportVariables) {
                SetupOutputVariable(state, "Surface Outside Face Outdoor Air Drybulb Temperature",
                                    OutputProcessor::Unit::C,
                                    Surface(loop).OutDryBulbTemp,
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Outdoor Air Wetbulb Temperature",
                                    OutputProcessor::Unit::C,
                                    Surface(loop).OutWetBulbTemp,
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Outdoor Air Wind Speed",
                                    OutputProcessor::Unit::m_s,
                                    Surface(loop).WindSpeed,
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Outdoor Air Wind Direction",
                                    OutputProcessor::Unit::deg,
                                    Surface(loop).WindDir,
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Convection Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    QdotConvOutRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Convection Heat Gain Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    QdotConvOutRepPerArea(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Convection Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    QConvOutReport(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Convection Heat Transfer Coefficient",
                                    OutputProcessor::Unit::W_m2K,
                                    HcExtSurf(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Net Thermal Radiation Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    QdotRadOutRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Net Thermal Radiation Heat Gain Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    QdotRadOutRepPerArea(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Net Thermal Radiation Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    QRadOutReport(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Thermal Radiation to Air Heat Transfer Coefficient",
                                    OutputProcessor::Unit::W_m2K,
                                    HAirExtSurf(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Thermal Radiation to Sky Heat Transfer Coefficient",
                                    OutputProcessor::Unit::W_m2K,
                                    HSkyExtSurf(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Thermal Radiation to Ground Heat Transfer Coefficient",
                                    OutputProcessor::Unit::W_m2K,
                                    HGrdExtSurf(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Thermal Radiation to Air Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    QAirExtReport(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Heat Emission to Air Rate",
                                    OutputProcessor::Unit::W,
                                    QHeatEmiReport(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                if (Surface(loop).Class != SurfaceClass_Window) {
                    SetupOutputVariable(state, "Surface Outside Face Solar Radiation Heat Gain Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqSWOutAbsTotalReport(loop),
                                        "Zone",
                                        "Average",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Solar Radiation Heat Gain Rate per Area",
                                        OutputProcessor::Unit::W_m2,
                                        SurfOpaqQRadSWOutAbs(loop),
                                        "Zone",
                                        "Average",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Solar Radiation Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        SurfOpaqSWOutAbsEnergyReport(loop),
                                        "Zone",
                                        "Sum",
                                        Surface(loop).Name);
                }
            }
            if (Surface(loop).Class == SurfaceClass_Floor || Surface(loop).Class == SurfaceClass_Wall ||
                Surface(loop).Class == SurfaceClass_IntMass || Surface(loop).Class == SurfaceClass_Roof || Surface(loop).Class == SurfaceClass_Door) {
                //      IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
                SetupOutputVariable(state, "Surface Inside Face Conduction Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    SurfOpaqInsFaceConduction(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Conduction Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    SurfOpaqInsFaceCondGainRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Conduction Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    SurfOpaqInsFaceCondLossRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Conduction Heat Transfer Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfOpaqInsFaceConductionFlux(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Inside Face Conduction Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    SurfOpaqInsFaceConductionEnergy(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);

                if (Surface(loop).ExtBoundCond != KivaFoundation) {
                    SetupOutputVariable(state, "Surface Outside Face Conduction Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqOutsideFaceConduction(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Conduction Heat Gain Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqExtFaceCondGainRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Conduction Heat Loss Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqExtFaceCondLossRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Conduction Heat Transfer Rate per Area",
                                        OutputProcessor::Unit::W_m2,
                                        SurfOpaqOutsideFaceConductionFlux(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Outside Face Conduction Heat Transfer Energy",
                                        OutputProcessor::Unit::J,
                                        SurfOpaqOutsideFaceConductionEnergy(loop),
                                        "Zone",
                                        "Sum",
                                        Surface(loop).Name);

                    SetupOutputVariable(state, "Surface Average Face Conduction Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqAvgFaceConduction(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Average Face Conduction Heat Gain Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqAvgFaceCondGainRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Average Face Conduction Heat Loss Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqAvgFaceCondLossRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Average Face Conduction Heat Transfer Rate per Area",
                                        OutputProcessor::Unit::W_m2,
                                        SurfOpaqAvgFaceConductionFlux(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Average Face Conduction Heat Transfer Energy",
                                        OutputProcessor::Unit::J,
                                        SurfOpaqAvgFaceConductionEnergy(loop),
                                        "Zone",
                                        "Sum",
                                        Surface(loop).Name);

                    SetupOutputVariable(state,
                        "Surface Heat Storage Rate", OutputProcessor::Unit::W, SurfOpaqStorageConduction(loop), "Zone", "State", Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Heat Storage Gain Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqStorageGainRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Heat Storage Loss Rate",
                                        OutputProcessor::Unit::W,
                                        SurfOpaqStorageCondLossRep(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Heat Storage Rate per Area",
                                        OutputProcessor::Unit::W_m2,
                                        SurfOpaqStorageConductionFlux(loop),
                                        "Zone",
                                        "State",
                                        Surface(loop).Name);
                    SetupOutputVariable(state, "Surface Heat Storage Energy",
                                        OutputProcessor::Unit::J,
                                        SurfOpaqStorageConductionEnergy(loop),
                                        "Zone",
                                        "Sum",
                                        Surface(loop).Name);
                }

                //      ENDIF
                // CurrentModuleObject='Opaque Surfaces'

                SetupOutputVariable(state, "Surface Inside Face Beam Solar Radiation Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    SurfOpaqInsFaceBeamSolAbsorbed(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
            }
            if (dataConstruction.Construct(Surface(loop).Construction).SourceSinkPresent) {
                SetupOutputVariable(state,
                    "Surface Internal Source Location Temperature", OutputProcessor::Unit::C, TempSource(loop), "Zone", "State", Surface(loop).Name);
                SetupOutputVariable(state, "Surface Internal User Specified Location Temperature",
                                    OutputProcessor::Unit::C,
                                    TempUserLoc(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
            }
            if (Surface(loop).Class == SurfaceClass_Window) { // CurrentModuleObject='Windows'
                SetupOutputVariable(state, "Surface Shading Device Is On Time Fraction",
                                    OutputProcessor::Unit::None,
                                    SurfWinFracTimeShadingDeviceOn(loop),
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Storm Window On Off Status",
                                    OutputProcessor::Unit::None,
                                    SurfWinStormWinFlag(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Window Blind Slat Angle",
                                    OutputProcessor::Unit::deg,
                                    SurfWinSlatAngThisTSDeg(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
            }
            //    IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
            SetupOutputVariable(state, "Surface Inside Face Convection Classification Index",
                                OutputProcessor::Unit::None,
                                Surface(loop).IntConvClassification,
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Convection Model Equation Index",
                                OutputProcessor::Unit::None,
                                Surface(loop).IntConvHcModelEq,
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Inside Face Convection Reference Air Index",
                                OutputProcessor::Unit::None,
                                Surface(loop).TAirRef,
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            if (Surface(loop).ExtBoundCond == ExternalEnvironment) {
                SetupOutputVariable(state, "Surface Outside Face Convection Classification Index",
                                    OutputProcessor::Unit::None,
                                    Surface(loop).OutConvClassification,
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Forced Convection Model Equation Index",
                                    OutputProcessor::Unit::None,
                                    Surface(loop).OutConvHfModelEq,
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
                SetupOutputVariable(state, "Surface Outside Face Natural Convection Model Equation Index",
                                    OutputProcessor::Unit::None,
                                    Surface(loop).OutConvHnModelEq,
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
            }

            SetupOutputVariable(state, "Surface Inside Face Heat Source Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                SurfQAdditionalHeatSourceInside(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state, "Surface Outside Face Heat Source Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                SurfQAdditionalHeatSourceOutside(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);

            //     ENDIF
            if (DisplayAdvancedReportVariables) {
                SetupOutputVariable(state,
                    "Surface Construction Index", OutputProcessor::Unit::None, Surface(loop).Construction, "Zone", "Average", Surface(loop).Name);
            }
        }

        // unused  ALLOCATE(QBV(NumOfZones))
        // unused  QBV=0.0
        ZoneQC.dimension(NumOfZones, 0.0);
        ZoneQD.dimension(NumOfZones, 0.0);
        ZoneQDforDaylight.dimension(NumOfZones, 0.0);
        ZoneQDV.dimension(NumOfZones, 0.0);
        QL.dimension(NumOfZones, 0.0);

        // UCSD
        MRT.dimension(NumOfZones, 0.0);

        // Allocate Reporting Variables and set up tracking
        ZoneMRT.dimension(NumOfZones, 0.0);

        for (int loop = 1; loop <= NumOfZones; ++loop) {
            // CurrentModuleObject='Zone'
            SetupOutputVariable(state, "Zone Mean Radiant Temperature", OutputProcessor::Unit::C, ZoneMRT(loop), "Zone", "State", Zone(loop).Name);
        }

        SetupOutputVariable(state,
            "Site Total Surface Heat Emission to Air", OutputProcessor::Unit::J, SumSurfaceHeatEmission, "Zone", "Sum", "Environment");
    }

    void InitThermalAndFluxHistories()
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

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // DO loop counter for surfaces
        int OSCMnum; // DO loop counter for Other side conditions modeled (OSCM)
        // FLOW:

        // First do the "bulk" initializations of arrays sized to NumOfZones
        MRT = 23.0; // module level array
        MAT = 23.0; // DataHeatBalFanSys array
        ZT = 23.0;
        ZTAV = 23.0;
        XMAT = 23.0; // DataHeatBalFanSys array
        XM2T = 23.0; // DataHeatBalFanSys array
        XM3T = 23.0; // DataHeatBalFanSys array
        XM4T = 23.0;
        XMPT = 23.0;
        DSXMAT = 23.0; // DataHeatBalFanSys array
        DSXM2T = 23.0; // DataHeatBalFanSys array
        DSXM3T = 23.0; // DataHeatBalFanSys array
        DSXM4T = 23.0;
        ZoneTMX = 23.0; // DataHeatBalFanSys array
        ZoneTM2 = 23.0; // DataHeatBalFanSys array
        // Initialize the Zone Humidity Ratio here so that it is available for EMPD implementations
        ZoneAirHumRatAvg = OutHumRat;
        ZoneAirHumRat = OutHumRat;
        ZoneAirHumRatOld = OutHumRat;
        SumHmAW = 0.0;
        SumHmARa = 0.0;
        SumHmARaW = 0.0;

        // "Bulk" initializations of arrays sized to TotSurfaces
        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            // Loop through zones...
            TempEffBulkAir(zoneNum) = 23.0;
            TempTstatAir(zoneNum) = 23.0;
            int const firstSurf = Zone(zoneNum).SurfaceFirst;
            int const lastSurf = Zone(zoneNum).SurfaceLast;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                SUMH(SurfNum) = 0;             // module level array
                TempSurfIn(SurfNum) = 23.0;    // module level array
                TempSurfInTmp(SurfNum) = 23.0; // module level array
                HConvIn(SurfNum) = 3.076;      // module level array
                HcExtSurf(SurfNum) = 0.0;
                HAirExtSurf(SurfNum) = 0.0;
                HSkyExtSurf(SurfNum) = 0.0;
                HGrdExtSurf(SurfNum) = 0.0;
                SurfTempSurfOut(SurfNum) = 0.0;
                TempSurfInRep(SurfNum) = 0.0;
                TempSurfInMovInsRep(SurfNum) = 0.0;
                QConvInReport(SurfNum) = 0.0;
                QdotConvInRep(SurfNum) = 0.0;
                QdotConvInRepPerArea(SurfNum) = 0.0;
                QRadNetSurfInReport(SurfNum) = 0.0;
                QdotRadNetSurfInRep(SurfNum) = 0.0;
                QdotRadNetSurfInRepPerArea(SurfNum) = 0.0;
                QRadSolarInReport(SurfNum) = 0.0;
                QdotRadSolarInRep(SurfNum) = 0.0;
                QdotRadSolarInRepPerArea(SurfNum) = 0.0;
                QRadLightsInReport(SurfNum) = 0.0;
                QdotRadLightsInRep(SurfNum) = 0.0;
                QdotRadLightsInRepPerArea(SurfNum) = 0.0;
                QRadIntGainsInReport(SurfNum) = 0.0;
                QdotRadIntGainsInRep(SurfNum) = 0.0;
                QdotRadIntGainsInRepPerArea(SurfNum) = 0.0;
                QRadHVACInReport(SurfNum) = 0.0;
                QdotRadHVACInRep(SurfNum) = 0.0;
                QdotRadHVACInRepPerArea(SurfNum) = 0.0;
                QConvOutReport(SurfNum) = 0.0;
                QdotConvOutRep(SurfNum) = 0.0;
                QdotConvOutRepPerArea(SurfNum) = 0.0;
                QRadOutReport(SurfNum) = 0.0;
                QdotRadOutRep(SurfNum) = 0.0;
                QdotRadOutRepPerArea(SurfNum) = 0.0;
                QAirExtReport(SurfNum) = 0.0;
                QHeatEmiReport(SurfNum) = 0.0;
            } // end of  Surf array
            int const firstSurfOpaq = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpaq = Zone(zoneNum).NonWindowSurfaceLast;
            if (firstSurfOpaq >= 0) {
                for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
                    SurfOpaqInsFaceConduction(SurfNum) = 0.0;
                    SurfOpaqInsFaceConductionFlux(SurfNum) = 0.0;
                    SurfOpaqInsFaceConductionEnergy(SurfNum) = 0.0;
                    SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) = 0.0;
                } // end of Zone Surf
            }
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin >= 0) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    // Initialize window frame and divider temperatures
                    SurfWinFrameTempSurfIn(SurfNum) = 23.0;
                    SurfWinFrameTempSurfInOld(SurfNum) = 23.0;
                    SurfWinFrameTempSurfOut(SurfNum) = 23.0;
                    SurfWinDividerTempSurfIn(SurfNum) = 23.0;
                    SurfWinDividerTempSurfInOld(SurfNum) = 23.0;
                    SurfWinDividerTempSurfOut(SurfNum) = 23.0;

                    // Initialize previous-timestep shading indicators
                    SurfWinExtIntShadePrevTS(SurfNum) = 0;
                    SurfWinShadingFlag(SurfNum) = NoShade;
                } // end of Zone Surf
            }
        } // end of Zone

        // "Bulk" initializations of temperature arrays with dimensions (TotSurface,MaxCTFTerms,2)
        TH = 23.0;  // module level array
        THM = 23.0; // module level array
        QH = 0.0;
        QHM = 0.0;
        if (AnyConstructInternalSourceInInput) {
            TsrcHist = 23.0;
            TsrcHistM = 23.0;
            TuserHist = 23.0;
            TuserHistM = 23.0;
            QsrcHist = 0.0;
            QsrcHistM = 0.0;
        }
        CondFDRelaxFactor = CondFDRelaxFactorInput;

        // Perform other initializations that depend on the surface characteristics
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            // Reset outside boundary conditions if necessary
            if ((Surface(SurfNum).ExtBoundCond == ExternalEnvironment) || (Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt)) {

                THM(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = Surface(SurfNum).OutDryBulbTemp;
                TH(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = Surface(SurfNum).OutDryBulbTemp;

            } else if (Surface(SurfNum).ExtBoundCond == Ground) {

                THM(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = GroundTemp;
                TH(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = GroundTemp;

            } else if (Surface(SurfNum).ExtBoundCond == GroundFCfactorMethod) {

                THM(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = GroundTempFC;
                TH(1, {1, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = GroundTempFC;
            }

            if (Surface(SurfNum).ExtCavityPresent) {
                ExtVentedCavity(Surface(SurfNum).ExtCavNum).TbaffleLast = 20.0;
                ExtVentedCavity(Surface(SurfNum).ExtCavNum).TairLast = 20.0;
            }

            // Initialize Kiva convection algorithms
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                SurfaceGeometry::kivaManager.surfaceConvMap[SurfNum].in = KIVA_CONST_CONV(3.076);
                SurfaceGeometry::kivaManager.surfaceConvMap[SurfNum].f = KIVA_HF_DEF;
                SurfaceGeometry::kivaManager.surfaceConvMap[SurfNum].out = KIVA_CONST_CONV(0.0);
            }

            // Initialize the flux histories
            QH(1, {2, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                dataConstruction.Construct(Surface(SurfNum).Construction).UValue * (TH(1, 1, SurfNum) - TH(2, 1, SurfNum));
            QH(2, {2, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = QH(1, 2, SurfNum);
            QHM(1, {2, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = QH(1, 2, SurfNum);
            QHM(2, {2, dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) = QH(1, 2, SurfNum);
        }

        if (TotOSCM >= 1) {
            for (OSCMnum = 1; OSCMnum <= TotOSCM; ++OSCMnum) {
                OSCM(OSCMnum).TConv = 20.0;
                OSCM(OSCMnum).HConv = 4.0;
                OSCM(OSCMnum).TRad = 20.0;
                OSCM(OSCMnum).HRad = 4.0;
            }
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

        // Using/Aliasing
        using SolarShading::CalcInteriorSolarDistribution;
        using namespace HeatBalanceMovableInsulation;
        using General::BlindBeamBeamTrans;
        using General::InterpBlind;
        using General::InterpProfSlatAng;
        using General::InterpSlatAng;
        using General::InterpSw;
        using General::POLYF;
        using namespace DataDaylightingDevices;
        using DaylightingDevices::TransTDD;
        using namespace DataWindowEquivalentLayer;
        using SolarShading::SurfaceScheduledSolarInc;
        using SolarShading::WindowScheduledSolarAbs;

        static Array1D<Real64> AbsDiffWin(CFSMAXNL);    // Diffuse solar absorptance of glass layers //Tuned Made static
        static Array1D<Real64> AbsDiffWinGnd(CFSMAXNL); // Ground diffuse solar absorptance of glass layers //Tuned Made static
        static Array1D<Real64> AbsDiffWinSky(CFSMAXNL); // Sky diffuse solar absorptance of glass layers //Tuned Made static

        Array1D<Real64> currCosInc(TotSurfaces); // Cosine of incidence angle of beam solar on glass
        Array1D<Real64> currBeamSolar(TotSurfaces); // Local variable for BeamSolarRad
        Array1D<Real64> currSkySolarInc(TotSurfaces); // Sky diffuse solar incident on a surface
        Array1D<Real64> currGndSolarInc(TotSurfaces); // Ground diffuse solar incident on a surface
        // Always initialize the shortwave quantities
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            SurfInitialDifSolInAbsReport(SurfNum) = 0.0;
            SurfSWInAbsTotalReport(SurfNum) = 0.0;

            SurfBmIncInsSurfIntensRep(SurfNum) = 0.0;
            SurfBmIncInsSurfAmountRep(SurfNum) = 0.0;
            SurfIntBmIncInsSurfIntensRep(SurfNum) = 0.0;
            SurfIntBmIncInsSurfAmountRep(SurfNum) = 0.0;

            SurfQRadSWOutIncident(SurfNum) = 0.0;
            SurfQRadSWOutIncidentBeam(SurfNum) = 0.0;
            SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = 0.0;
            SurfQRadSWOutIncidentGndDiffuse(SurfNum) = 0.0;

            SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) = 0.0;
            SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) = 0.0;
            SurfQRadSWOutIncBmToBmReflObs(SurfNum) = 0.0;
            SurfQRadSWOutIncBmToDiffReflObs(SurfNum) = 0.0;
            SurfQRadSWOutIncSkyDiffReflObs(SurfNum) = 0.0;
            SurfCosIncidenceAngle(SurfNum) = 0.0;

            SurfBmIncInsSurfIntensRep(SurfNum) = 0.0;
            SurfBmIncInsSurfAmountRep(SurfNum) = 0.0;
            SurfIntBmIncInsSurfIntensRep(SurfNum) = 0.0;
            SurfIntBmIncInsSurfAmountRep(SurfNum) = 0.0;
            SurfBmIncInsSurfAmountRepEnergy(SurfNum) = 0.0;
            SurfIntBmIncInsSurfAmountRepEnergy(SurfNum) = 0.0;

            SurfSkySolarInc(SurfNum) = 0.0;
            SurfGndSolarInc(SurfNum) = 0.0;

        }

        for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
            InitialZoneDifSolReflW(zoneNum) = 0.0;
            ZoneWinHeatGainRepEnergy(zoneNum) = 0.0;
            ZoneWinHeatLossRepEnergy(zoneNum) = 0.0;
            ZnOpqSurfInsFaceCondGnRepEnrg(zoneNum) = 0.0;
            ZnOpqSurfInsFaceCondLsRepEnrg(zoneNum) = 0.0;
            ZnOpqSurfExtFaceCondGnRepEnrg(zoneNum) = 0.0;
            ZnOpqSurfExtFaceCondLsRepEnrg(zoneNum) = 0.0;

            ZoneWinHeatGain(zoneNum) = 0.0;
            ZoneWinHeatGainRep(zoneNum) = 0.0;
            ZoneWinHeatLossRep(zoneNum) = 0.0;
            ZoneOpaqSurfInsFaceCond(zoneNum) = 0.0;
            ZoneOpaqSurfInsFaceCondGainRep(zoneNum) = 0.0;
            ZoneOpaqSurfInsFaceCondLossRep(zoneNum) = 0.0;
            ZoneOpaqSurfExtFaceCond(zoneNum) = 0.0;
            ZoneOpaqSurfExtFaceCondGainRep(zoneNum) = 0.0;
            ZoneOpaqSurfExtFaceCondLossRep(zoneNum) = 0.0;
        }
        for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
            int const firstSurfOpaq = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpaq = Zone(zoneNum).NonWindowSurfaceLast;
            if (firstSurfOpaq == -1) continue;
            for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
                SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) = 0.0;
                SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
                SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
                SurfOpaqQRadSWOutAbs(SurfNum) = 0.0;
                SurfOpaqQRadSWInAbs(SurfNum) = 0.0;
                SurfOpaqQRadSWLightsInAbs(SurfNum) = 0.0;
                SurfOpaqInitialDifSolInAbs(SurfNum) = 0.0;
                SurfOpaqSWOutAbsTotalReport(SurfNum) = 0.0;
                SurfOpaqSWOutAbsEnergyReport(SurfNum) = 0.0;
            } // end of ZoneSurf

            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                // Faster "inline" than calling SurfaceWindow( SurfNum ).InitSolarHeatGains()
                SurfWinFrameQRadOutAbs(SurfNum) = 0.0;
                SurfWinFrameQRadInAbs(SurfNum) = 0.0;
                SurfWinDividerQRadOutAbs(SurfNum) = 0.0;
                SurfWinDividerQRadInAbs(SurfNum) = 0.0;
                SurfWinExtBeamAbsByShade(SurfNum) = 0.0;
                SurfWinExtDiffAbsByShade(SurfNum) = 0.0;
                SurfWinIntBeamAbsByShade(SurfNum) = 0.0;
                SurfWinIntSWAbsByShade(SurfNum) = 0.0;
                SurfWinInitialDifSolAbsByShade(SurfNum) = 0.0;
                SurfWinIntLWAbsByShade(SurfNum) = 0.0;
                SurfWinConvHeatFlowNatural(SurfNum) = 0.0;
                SurfWinConvHeatGainToZoneAir(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinRetHeatGainToZoneAir(SurfNum) = 0.0;
                SurfWinDividerHeatGain(SurfNum) = 0.0;
                SurfWinBlTsolBmBm(SurfNum) = 0.0;
                SurfWinBlTsolBmDif(SurfNum) = 0.0;
                SurfWinBlTsolDifDif(SurfNum) = 0.0;
                SurfWinBlGlSysTsolBmBm(SurfNum) = 0.0;
                SurfWinBlGlSysTsolDifDif(SurfNum) = 0.0;
                SurfWinScTsolBmBm(SurfNum) = 0.0;
                SurfWinScTsolBmDif(SurfNum) = 0.0;
                SurfWinScTsolDifDif(SurfNum) = 0.0;
                SurfWinScGlSysTsolBmBm(SurfNum) = 0.0;
                SurfWinScGlSysTsolDifDif(SurfNum) = 0.0;
                SurfWinGlTsolBmBm(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinGlTsolBmDif(SurfNum) = 0.0;
                SurfWinGlTsolDifDif(SurfNum) = 0.0;
                SurfWinBmSolTransThruIntWinRep(SurfNum) = 0.0;
                SurfWinBmSolAbsdOutsReveal(SurfNum) = 0.0;
                SurfWinBmSolRefldOutsRevealReport(SurfNum) = 0.0;
                SurfWinBmSolAbsdInsReveal(SurfNum) = 0.0;
                SurfWinBmSolRefldInsReveal(SurfNum) = 0.0;
                SurfWinBmSolRefldInsRevealReport(SurfNum) = 0.0;
                SurfWinOutsRevealDiffOntoGlazing(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoGlazing(SurfNum) = 0.0;
                SurfWinInsRevealDiffIntoZone(SurfNum) = 0.0;
                SurfWinOutsRevealDiffOntoFrame(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinInsRevealDiffOntoFrame(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoGlazingReport(SurfNum) = 0.0;
                SurfWinInsRevealDiffIntoZoneReport(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoFrameReport(SurfNum) = 0.0;
                SurfWinBmSolAbsdInsRevealReport(SurfNum) = 0.0;
                SurfWinBmSolTransThruIntWinRepEnergy(SurfNum) = 0.0;
                SurfWinBmSolRefldOutsRevealRepEnergy(SurfNum) = 0.0;
                SurfWinBmSolRefldInsRevealRepEnergy(SurfNum) = 0.0;
                SurfWinProfileAngHor(SurfNum) = 0.0;
                SurfWinProfileAngVert(SurfNum) = 0.0;
                SurfWinHeatGain(SurfNum) = 0.0;
                SurfWinHeatTransfer(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinHeatGainRep(SurfNum) = 0.0;
                SurfWinHeatLossRep(SurfNum) = 0.0;
                SurfWinGainConvGlazToZoneRep(SurfNum) = 0.0;
                SurfWinGainIRGlazToZoneRep(SurfNum) = 0.0;
                SurfWinLossSWZoneToOutWinRep(SurfNum) = 0.0;
                SurfWinGainFrameDividerToZoneRep(SurfNum) = 0.0;
                SurfWinGainConvGlazShadGapToZoneRep(SurfNum) = 0.0;
                SurfWinGainConvShadeToZoneRep(SurfNum) = 0.0;
                SurfWinGainIRShadeToZoneRep(SurfNum) = 0.0;
                SurfWinGapConvHtFlowRep(SurfNum) = 0.0;
                SurfWinShadingAbsorbedSolar(SurfNum) = 0.0;
                SurfWinSysSolTransmittance(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinSysSolReflectance(SurfNum) = 0.0;
                SurfWinSysSolAbsorptance(SurfNum) = 0.0;
                SurfWinHeatGainRepEnergy(SurfNum) = 0.0;
                SurfWinHeatLossRepEnergy(SurfNum) = 0.0;
                SurfWinGapConvHtFlowRepEnergy(SurfNum) = 0.0;
                SurfWinHeatTransferRepEnergy(SurfNum) = 0.0;
                SurfWinShadingAbsorbedSolarEnergy(SurfNum) = 0.0;
                SurfWinOtherConvGainInsideFaceToZoneRep(SurfNum) = 0.0;
                SurfWinBSDFBeamDirectionRep(SurfNum) = 0;
                SurfWinBSDFBeamThetaRep(SurfNum) = 0.0;
                SurfWinBSDFBeamPhiRep(SurfNum) = 0.0;
                SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                SurfWinQRadSWwinAbsTotEnergy(SurfNum) = 0.0;
                SurfWinSWwinAbsTotalReport(SurfNum) = 0.0;
                SurfWinInitialDifSolInTrans(SurfNum) = 0.0;
                SurfWinInitialDifSolInTransReport(SurfNum) = 0.0;

            }

            for (int Lay = 1; Lay <= DataHeatBalance::MaxSolidWinLayers; Lay++) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    SurfWinQRadSWwinAbsLayer(Lay, SurfNum) = 0.0;
                }
            }
            for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL + 1; Lay++) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    SurfWinQRadSWwinAbs(Lay, SurfNum) = 0.0;
                }
            }
            for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL; Lay++) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    SurfWinInitialDifSolwinAbs(Lay, SurfNum) = 0.0;
                }
            }
        }
        if (InitSurfaceHeatBalancefirstTime) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                BmToDiffReflFacGnd(SurfNum) = Surface(SurfNum).ViewFactorGround;
                SkyDiffReflFacGnd(SurfNum) = Surface(SurfNum).ViewFactorGround;
            }
        }

        if (!SunIsUp || (BeamSolarRad + GndSolarRad + DifSolarRad <= 0.0)) { // Sun is down

            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
                ZoneQD(zoneNum) = 0.0;
                ZoneQDforDaylight(zoneNum) = 0.0;
                ZoneQC(zoneNum) = 0.0;
                ZoneQDV(zoneNum) = 0.0;
                ZoneTransSolar(zoneNum) = 0.0;
                ZoneBmSolFrExtWinsRep(zoneNum) = 0.0;
                ZoneBmSolFrIntWinsRep(zoneNum) = 0.0;
                ZoneDifSolFrExtWinsRep(zoneNum) = 0.0;
                ZoneDifSolFrIntWinsRep(zoneNum) = 0.0;
                // energy
                ZoneTransSolarEnergy(zoneNum) = 0.0;
                ZoneBmSolFrExtWinsRepEnergy(zoneNum) = 0.0;
                ZoneBmSolFrIntWinsRepEnergy(zoneNum) = 0.0;
                ZoneDifSolFrExtWinsRepEnergy(zoneNum) = 0.0;
                ZoneDifSolFrIntWinsRepEnergy(zoneNum) = 0.0;
            }
            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
                int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
                int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
                if (firstSurfWin == -1) continue;
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    SurfWinTransSolar(SurfNum) = 0.0;
                    SurfWinBmSolar(SurfNum) = 0.0;
                    SurfWinBmBmSolar(SurfNum) = 0.0;
                    SurfWinBmDifSolar(SurfNum) = 0.0;
                    SurfWinDifSolar(SurfNum) = 0.0;
                    SurfWinDirSolTransAtIncAngle(SurfNum) = 0.0;
                    // energy
                    SurfWinTransSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmBmSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmDifSolarEnergy(SurfNum) = 0.0;
                    SurfWinDifSolarEnergy(SurfNum) = 0.0;
                }
            }

            if (NumOfTDDPipes > 0) {
                for (auto &e : TDDPipe) {
                    e.TransSolBeam = 0.0;
                    e.TransSolDiff = 0.0;
                    e.TransVisBeam = 0.0;
                    e.TransVisDiff = 0.0;
                    e.TransmittedSolar = 0.0;
                    int SurfDome = e.Dome;
                    SurfWinTransSolar(SurfDome) = 0.0;
                    SurfQRadSWOutIncident(SurfDome) = 0.0;
                    SurfWinQRadSWwinAbsTot(SurfDome) = 0.0;
                    for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL + 1; Lay++) {
                        SurfWinQRadSWwinAbs(Lay, SurfDome) = 0.0;
                    }
                }
            }

            if (CalcSolRefl) {
                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                    BmToBmReflFacObs(SurfNum) = 0.0;
                    BmToDiffReflFacObs(SurfNum) = 0.0;
                    BmToDiffReflFacGnd(SurfNum) = 0.0;
                }
            }

        } else { // Sun is up, calculate solar quantities

            assert(equal_dimensions(ReflFacBmToBmSolObs, ReflFacBmToDiffSolObs)); // For linear indexing
            assert(equal_dimensions(ReflFacBmToBmSolObs, ReflFacBmToDiffSolGnd)); // For linear indexing
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                SurfSkySolarInc(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum);
                SurfGndSolarInc(SurfNum) = GndSolarRad * Surface(SurfNum).ViewFactorGround;
                SurfWinSkyGndSolarInc(SurfNum) = SurfGndSolarInc(SurfNum);
                SurfWinBmGndSolarInc(SurfNum) = 0.0;
            }
            if (CalcSolRefl) {
                // [ lSH ] == ( HourOfDay, SurfNum ) // [ lSP ] == ( PreviousHour, SurfNum )
                Array1D<Real64>::size_type lSH = ReflFacBmToBmSolObs.index(HourOfDay, 1) - 1;
                Array1D<Real64>::size_type lSP = ReflFacBmToBmSolObs.index(PreviousHour, 1) - 1;
                // For Complex Fenestrations:
                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                    SurfWinSkyGndSolarInc(SurfNum) = DifSolarRad * GndReflectance * ReflFacSkySolGnd(SurfNum);
                    SurfWinBmGndSolarInc(SurfNum) = BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum);
                    BmToBmReflFacObs(SurfNum) = WeightNow * ReflFacBmToBmSolObs[lSH + SurfNum] +
                                                WeightPreviousHour * ReflFacBmToBmSolObs[lSP + SurfNum];
                    BmToDiffReflFacObs(SurfNum) = WeightNow * ReflFacBmToDiffSolObs[lSH + SurfNum] +
                                                  WeightPreviousHour * ReflFacBmToDiffSolObs[lSP + SurfNum];
                    BmToDiffReflFacGnd(SurfNum) = WeightNow * ReflFacBmToDiffSolGnd[lSH + SurfNum] +
                                                  WeightPreviousHour * ReflFacBmToDiffSolGnd[lSP + SurfNum];
                    // TH2 CR 9056
                    SurfSkySolarInc(SurfNum) +=
                        BeamSolarRad * (BmToBmReflFacObs(SurfNum) + BmToDiffReflFacObs(SurfNum)) + DifSolarRad * ReflFacSkySolObs(SurfNum);
                    SurfGndSolarInc(SurfNum) = BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum) +
                                                         DifSolarRad * GndReflectance * ReflFacSkySolGnd(SurfNum);
                    SkyDiffReflFacGnd(SurfNum) = ReflFacSkySolGnd(SurfNum);
                }
            }

            CalcWindowProfileAngles();

            if (CalcWindowRevealReflection) CalcBeamSolarOnWinRevealSurface();

            if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel() && state.dataWindowManager->winOpticalModel->isSimplifiedModel()) {
                CalcInteriorSolarDistributionWCE(state);
            } else {
                CalcInteriorSolarDistribution(state);
            }

            for (int ZoneNum = 1; ZoneNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++ZoneNum) {

                // TH 3/24/2010 - QBV is not used!
                // unused      QBV(ZoneNum) = (CBZone(ZoneNum) + DBZone(ZoneNum))*BeamSolarRad

                // RJH 08/30/07 - QDV does not seem to ever be used. NOT USED!
                // QDV(ZoneNum) = DSZone(ZoneNum)*DifSolarRad &
                //                +DGZone(ZoneNum)*GndSolarRad

                // Original QD calc used only for QSDifSol and daylighting calcs
                // QDforDaylight(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
                //                          +DSZone(ZoneNum)*DifSolarRad  &
                //                          +DGZone(ZoneNum)*GndSolarRad

                // TH 3/23/2010. CR 7869 and CR 7999. QDforDaylight in W
                //  Beam from interior windows (DBZoneIntWin) reflected from floor is counted in DayltgInterReflIllFrIntWins,
                //  DBZone needs to subtract this part since it is already counted in DBZone.
                //  Use InitialZoneDifSolReflW (Rob's previous work) as it better counts initial distribution of
                //   diffuse solar rather than using weighted area*absorptance
                ZoneQDforDaylight(ZoneNum) =
                    (DBZone(ZoneNum) - DBZoneIntWin(ZoneNum)) * BeamSolarRad + DBZoneSSG(ZoneNum) + InitialZoneDifSolReflW(ZoneNum);

                // RJH 08/30/07 - Substitute InitialZoneDifSolReflW(ZoneNum) for DSZone and DGZone here
                // to exclude diffuse solar now absorbed/transmitted in CalcWinTransDifSolInitialDistribution
                // DBZone(ZoneNum) is Diffuse Solar from beam reflected from interior surfaces
                // and transmitted through interior windows
                // DBZone is a factor that when multiplied by BeamSolarRad [W/m2] gives Watts
                // QD(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
                //                +DSZone(ZoneNum)*DifSolarRad  &
                //                +DGZone(ZoneNum)*GndSolarRad
                ZoneQD(ZoneNum) = DBZone(ZoneNum) * BeamSolarRad + DBZoneSSG(ZoneNum) + InitialZoneDifSolReflW(ZoneNum);
            }

            // Flux of diffuse solar in each zone

            QSDifSol = 0.0;
            for (int enclNum = 1; enclNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclNum) {
                QSDifSol(enclNum) = ZoneQDforDaylight(enclNum);
            }

            if (InterZoneWindow) {
                for (int enclNum = 1; enclNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclNum) {
                    if (RecDifShortFromZ(enclNum)) {
                        Real64 QSDifSol_sum(0.0);                        // Accumulator
                        auto lZone(FractDifShortZtoZ.index(enclNum, 1)); // Tuned Linear indexing
                        for (int otherEnclNum = 1; otherEnclNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++otherEnclNum, ++lZone) {
                            if ((otherEnclNum != enclNum) && (RecDifShortFromZ(otherEnclNum))) {
                                QSDifSol_sum += FractDifShortZtoZ[lZone] * ZoneQDforDaylight(otherEnclNum); // [ lZone ] == ( enclNum, otherEnclNum )
                            }
                        }
                        QSDifSol(enclNum) += QSDifSol_sum;
                    }
                }
            }

            for (int enclNum = 1; enclNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclNum) {
                if (InterZoneWindow)
                    QSDifSol(enclNum) *= FractDifShortZtoZ(enclNum, enclNum) * ZoneVMULT(enclNum);
                else
                    QSDifSol(enclNum) *= ZoneVMULT(enclNum);
            }

            //    RJH - 09-12-07 commented out report varariable calcs here since they refer to old distribution method
            //    DO SurfNum = 1, TotSurfaces
            //      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
            //!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
            //      IF (Surface(SurfNum)%Class == SurfaceClass_Shading) CYCLE
            //      ZoneNum = Surface(SurfNum)%Zone
            // Diffuse solar entering zone through exterior windows is assumed to be uniformly
            // distributed on inside face of surfaces of zone
            //      DifIncInsSurfIntensRep(SurfNum) = (DSZone(ZoneNum)*DifSolarRad + DGZone(ZoneNum)*GndSolarRad) /  &
            //        Zone(ZoneNum)%TotalSurfArea
            //      DifIncInsSurfAmountRep(SurfNum) = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) *  &
            //        DifIncInsSurfIntensRep(SurfNum)
            //      DifIncInsSurfAmountRepEnergy(SurfNum) = DifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
            //    END DO
            if (BuildingShadingCount || FixedShadingCount || AttachedShadingCount) {
                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                    if (!Surface(SurfNum).ShadowingSurf) continue;
                    // Cosine of incidence angle and solar incident on outside of surface, for reporting
                    Real64 CosInc = CosIncAng(TimeStep, HourOfDay, SurfNum);
                    SurfCosIncidenceAngle(SurfNum) = CosInc;
                    // Incident direct (unreflected) beam
                    SurfQRadSWOutIncidentBeam(SurfNum) =
                            BeamSolarRad * SunlitFrac(TimeStep, HourOfDay, SurfNum) * CosInc;
                    // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
                    SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum);
                    // Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
                    SurfQRadSWOutIncidentGndDiffuse(SurfNum) = SurfGndSolarInc(SurfNum);
                    // Incident diffuse solar from beam-to-diffuse reflection from ground
                    SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) =
                            BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum);
                    // Incident diffuse solar from sky diffuse reflection from ground
                    SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) = DifSolarRad * GndReflectance * SkyDiffReflFacGnd(SurfNum);
                    // Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
                    // in SkySolarInc.
                    SurfQRadSWOutIncident(SurfNum) =
                            SurfQRadSWOutIncidentBeam(SurfNum) + SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                            SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);

                }
            }

            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
                int const firstSurf = Zone(zoneNum).SurfaceFirst;
                int const lastSurf = Zone(zoneNum).SurfaceLast;
                for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                    if (Surface(SurfNum).ExtSolar) {
                        // Regular surface
                        currCosInc(SurfNum) = CosIncAng(TimeStep, HourOfDay, SurfNum);
                        currBeamSolar(SurfNum) = BeamSolarRad;
                        currSkySolarInc(SurfNum) = SurfSkySolarInc(SurfNum);
                        currGndSolarInc(SurfNum) = SurfGndSolarInc(SurfNum);
                        // Cosine of incidence angle and solar incident on outside of surface, for reporting
                        SurfCosIncidenceAngle(SurfNum) = currCosInc(SurfNum);
                        // Report variables for various incident solar quantities
                        // Incident direct (unreflected) beam
                        SurfQRadSWOutIncidentBeam(SurfNum) =
                                currBeamSolar(SurfNum) * SunlitFrac(TimeStep, HourOfDay, SurfNum) * currCosInc(SurfNum);

                        // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
                        SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum);
                        // Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
                        SurfQRadSWOutIncidentGndDiffuse(SurfNum) = currGndSolarInc(SurfNum);
                        // Incident diffuse solar from beam-to-diffuse reflection from ground
                        SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) =
                                BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum);

                        // Incident diffuse solar from sky diffuse reflection from ground
                        SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) =
                                DifSolarRad * GndReflectance * SkyDiffReflFacGnd(SurfNum);
                        // Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
                        // in SkySolarInc.
                        // QRadSWOutIncident(SurfNum) = QRadSWOutIncidentBeam(SurfNum) + SkySolarInc + GndSolarInc

                        // TH2 CR 9056
                        SurfQRadSWOutIncident(SurfNum) =
                                SurfQRadSWOutIncidentBeam(SurfNum) + SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                                SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);

                        if (CalcSolRefl) {
                            // Incident beam solar from beam-to-beam (specular) reflection from obstructions
                            SurfQRadSWOutIncBmToBmReflObs(SurfNum) = BmToBmReflFacObs(SurfNum) * BeamSolarRad;
                            // Incident diffuse solar from beam-to-diffuse reflection from obstructions
                            SurfQRadSWOutIncBmToDiffReflObs(SurfNum) = BmToDiffReflFacObs(SurfNum) * BeamSolarRad;
                            // Incident diffuse solar from sky diffuse reflection from obstructions
                            SurfQRadSWOutIncSkyDiffReflObs(SurfNum) = DifSolarRad * ReflFacSkySolObs(SurfNum);
                            // TH2 CR 9056: Add reflections from obstructions to the total incident
                            SurfQRadSWOutIncident(SurfNum) +=
                                    SurfQRadSWOutIncBmToBmReflObs(SurfNum) + SurfQRadSWOutIncBmToDiffReflObs(SurfNum) +
                                    SurfQRadSWOutIncSkyDiffReflObs(SurfNum);
                        }
                    }
                }
            }
            for (int PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum) {
                int SurfNum = TDDPipe(PipeNum).Diffuser; // TDD: Diffuser object number
                int SurfNum2 = TDDPipe(PipeNum).Dome; // TDD: DOME object number
                int ConstrNum = Surface(SurfNum).Construction;
                if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;

                currCosInc(SurfNum) = CosIncAng(TimeStep, HourOfDay, SurfNum2);

                // Reconstruct the beam, sky, and ground radiation transmittance of just the TDD:DOME and TDD pipe
                // by dividing out diffuse solar transmittance of TDD:DIFFUSER
                currBeamSolar(SurfNum) = BeamSolarRad * TransTDD(PipeNum, currCosInc(SurfNum), SolarBeam) /
                                         dataConstruction.Construct(ConstrNum).TransDiff;

                currSkySolarInc(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum2) *
                                           TransTDD(PipeNum, currCosInc(SurfNum), SolarAniso) /
                                           dataConstruction.Construct(ConstrNum).TransDiff;

                currGndSolarInc(SurfNum) =
                        GndSolarRad * Surface(SurfNum2).ViewFactorGround * TDDPipe(PipeNum).TransSolIso /
                        dataConstruction.Construct(ConstrNum).TransDiff;
                // Incident direct (unreflected) beam
                SurfQRadSWOutIncidentBeam(SurfNum) =
                        currBeamSolar(SurfNum) * SunlitFrac(TimeStep, HourOfDay, SurfNum2) *
                        currCosInc(SurfNum); // NOTE: SurfNum2

                // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
                SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = currSkySolarInc(SurfNum);
                SurfQRadSWOutIncident(SurfNum) =
                        SurfQRadSWOutIncidentBeam(SurfNum) + SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                        SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);
            }
            for (int ShelfNum = 1; ShelfNum <= NumOfShelf; ++ShelfNum) {
                int SurfNum = Shelf(ShelfNum).Window; // Daylighting shelf object number
                int OutShelfSurf = Shelf(ShelfNum).OutSurf; // Outside daylighting shelf present if > 0
                currCosInc(SurfNum) = CosIncAng(TimeStep, HourOfDay, SurfNum);
                currBeamSolar(SurfNum) = BeamSolarRad;
                currSkySolarInc(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum);
                // Shelf diffuse solar radiation
                Real64 ShelfSolarRad = (BeamSolarRad * SunlitFrac(TimeStep, HourOfDay, OutShelfSurf) *
                                        CosIncAng(TimeStep, HourOfDay, OutShelfSurf) +
                                        DifSolarRad * AnisoSkyMult(OutShelfSurf)) *
                                       Shelf(ShelfNum).OutReflectSol;

                // Add all reflected solar from the outside shelf to the ground solar
                // NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!
                currGndSolarInc(SurfNum) = GndSolarRad * Surface(SurfNum).ViewFactorGround +
                                           ShelfSolarRad * Shelf(ShelfNum).ViewFactor;
            }

            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
                int const firstSurfOpaq = Zone(zoneNum).NonWindowSurfaceFirst;
                int const lastSurfOpaq = Zone(zoneNum).NonWindowSurfaceLast;
                for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
                    if (Surface(SurfNum).HeatTransSurf) {
                        int ConstrNum = Surface(SurfNum).Construction;
                        if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;
                        if (Surface(SurfNum).ExtSolar) {
                            // Exclude special shading surfaces which required QRadSWOut calculations above
                            int RoughIndexMovInsul = 0; // Roughness index of movable insulation
                            Real64 HMovInsul; // Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
                            Real64 AbsExt; // Absorptivity of outer most layer (or movable insulation if present)
                            if (Surface(SurfNum).MaterialMovInsulExt > 0)
                                EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);

                            if (RoughIndexMovInsul <= 0) { // No movable insulation present

                                AbsExt = dataMaterial.Material(
                                            dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpSolar;
                            }
                            // Opaque heat transfer surface
                            SurfOpaqQRadSWOutAbs(SurfNum) =
                                    AOSurf(SurfNum) * BeamSolarRad + AbsExt * (currSkySolarInc(SurfNum) + currGndSolarInc(SurfNum));
                            SurfOpaqSWOutAbsTotalReport(SurfNum) =
                                    SurfOpaqQRadSWOutAbs(SurfNum) * Surface(SurfNum).Area;
                            SurfOpaqSWOutAbsEnergyReport(SurfNum) =
                                    SurfOpaqSWOutAbsTotalReport(SurfNum) * TimeStepZoneSec;
                        }
                        if (ConstrNum > 0) {
                            int SurfSolIncPtr = SurfaceScheduledSolarInc(SurfNum, ConstrNum);
                            if (SurfSolIncPtr == 0) {
                                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                                    int ShelfNum = Surface(SurfNum).Shelf; // Daylighting shelf object number
                                    int InShelfSurf = 0; // Inside daylighting shelf surface number
                                    if (ShelfNum > 0) {
                                        InShelfSurf = Shelf(ShelfNum).InSurf; // Inside daylighting shelf present if > 0
                                    }
                                    SurfOpaqQRadSWInAbs(SurfNum) += AISurf(SurfNum) * BeamSolarRad;
                                    if (InShelfSurf > 0) { // Inside daylighting shelf
                                        // Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
                                        SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) =
                                                AISurf(SurfNum) * BeamSolarRad * (0.5 * Surface(SurfNum).Area);
                                    } else { // Regular surface
                                        SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) =
                                                AISurf(SurfNum) * BeamSolarRad * Surface(SurfNum).Area;
                                    }
                                }
                            } else {
                                SurfOpaqQRadSWInAbs(SurfNum) += AISurf(SurfNum);
                            }
                        }
                    }
                }
                int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
                int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    if (Surface(SurfNum).ExtSolar || SurfWinOriginalClass(SurfNum) == SurfaceClass_TDD_Diffuser) {
                        if (Surface(SurfNum).HeatTransSurf) {
                            // Exclude special shading surfaces which required QRadSWOut calculations above
                            int RoughIndexMovInsul = 0; // Roughness index of movable insulation
                            Real64 HMovInsul; // Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
                            Real64 AbsExt; // Absorptivity of outer most layer (or movable insulation if present)

                            if (Surface(SurfNum).MaterialMovInsulExt > 0)
                                EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);
                            int ConstrNum = 0 ;
                            if (Surface(SurfNum).HeatTransSurf) {
                                ConstrNum = Surface(SurfNum).Construction;
                                if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;
                            }
                            if (RoughIndexMovInsul <= 0) { // No movable insulation present
                                Real64 CosInc = currCosInc(SurfNum); // Cosine of incidence angle of beam solar on glass
                                Real64 BeamSolar = currBeamSolar(SurfNum); // Local variable for BeamSolarRad
                                Real64 SkySolarInc = currSkySolarInc(SurfNum); // Sky diffuse solar incident on a surface
                                Real64 GndSolarInc = currGndSolarInc(SurfNum); // Ground diffuse solar incident on a surface

                                int ShadeFlag = SurfWinShadingFlag(SurfNum);

                                if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel &&
                                    SurfWinWindowModelType(SurfNum) != WindowEQLModel &&
                                    !state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
                                    int TotGlassLay = dataConstruction.Construct(ConstrNum).TotGlassLayers; // Number of glass layers
                                    for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                        AbsDiffWin(Lay) = dataConstruction.Construct(ConstrNum).AbsDiff(Lay);
                                    }

                                    ShadeFlag = SurfWinShadingFlag(SurfNum);

                                    if (ShadeFlag > 0) { // Shaded window

                                        int ConstrNumSh = Surface(SurfNum).activeShadedConstruction; // Shaded window construction
                                        if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;

                                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn ||
                                            ShadeFlag == ExtScreenOn) { // Shade/screen on

                                            for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                                AbsDiffWin(Lay) = dataConstruction.Construct(ConstrNumSh).AbsDiff(Lay);
                                            }
                                            SurfWinExtDiffAbsByShade(SurfNum) =
                                                    dataConstruction.Construct(ConstrNumSh).AbsDiffShade *
                                                    (SkySolarInc + GndSolarInc);
                                        }

                                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ||
                                            ShadeFlag == BGBlindOn) { // Blind on
                                            for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                                // TODO: InterpSlatAng

                                                AbsDiffWin(Lay) = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                                                SurfWinMovableSlats(SurfNum),
                                                                                dataConstruction.Construct(
                                                                                        ConstrNumSh).BlAbsDiff(
                                                                                        {1, MaxSlatAngs}, Lay));
                                                AbsDiffWinGnd(Lay) = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                                                   SurfWinMovableSlats(SurfNum),
                                                                                   dataConstruction.Construct(
                                                                                           ConstrNumSh).BlAbsDiffGnd(
                                                                                           {1, MaxSlatAngs}, Lay));
                                                AbsDiffWinSky(Lay) = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                                                   SurfWinMovableSlats(SurfNum),
                                                                                   dataConstruction.Construct(
                                                                                           ConstrNumSh).BlAbsDiffSky(
                                                                                           {1, MaxSlatAngs}, Lay));
                                            }
                                            SurfWinExtDiffAbsByShade(SurfNum) =
                                                    InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                    dataConstruction.Construct( ConstrNumSh).AbsDiffBlind) *
                                                    (SkySolarInc + GndSolarInc);
                                            if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                                Real64 ACosTlt = std::abs(Surface(SurfNum).CosTilt);
                                                Real64 AbsDiffBlindGnd = InterpSlatAng(
                                                        SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                        dataConstruction.Construct(ConstrNumSh).AbsDiffBlindGnd);
                                                Real64 AbsDiffBlindSky = InterpSlatAng(
                                                        SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                        dataConstruction.Construct(ConstrNumSh).AbsDiffBlindSky);
                                                SurfWinExtDiffAbsByShade(SurfNum) =
                                                        SkySolarInc * (0.5 * ACosTlt * AbsDiffBlindGnd + (1.0 - 0.5 * ACosTlt) * AbsDiffBlindSky) +
                                                        GndSolarInc * ((1.0 - 0.5 * ACosTlt) * AbsDiffBlindGnd +
                                                        0.5 * ACosTlt * AbsDiffBlindSky);
                                            }
                                        }

                                        // Correct for shadowing of divider onto interior shading device (note that dividers are
                                        // not allowed in windows with between-glass shade/blind)

                                        if ((ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn) &&
                                            SurfWinDividerArea(SurfNum) > 0.0)
                                            SurfWinExtDiffAbsByShade(SurfNum) *= SurfWinGlazedFrac(SurfNum);

                                        if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                                            Real64 SwitchFac = SurfWinSwitchingFactor(
                                                    SurfNum); // Switching factor for switchable glazing
                                            for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                                AbsDiffWin(Lay) = InterpSw(SwitchFac, AbsDiffWin(Lay),
                                                                           dataConstruction.Construct(
                                                                                   ConstrNumSh).AbsDiff(Lay));
                                            }
                                        }

                                    } // End of check if window has shading device on

                                    SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                                    for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                        SurfWinQRadSWwinAbs(Lay, SurfNum) = AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc) +
                                                                            AWinSurf(Lay, SurfNum) * BeamSolar;
                                        // AWinSurf is from InteriorSolarDistribution
                                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ||
                                            ShadeFlag == BGBlindOn) {
                                            int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                            if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                                // AbsDiffGlassLayGnd - System glass layer ground diffuse solar absorptance with blind on
                                                // AbsDiffGlassLaySky - System glass layer sky diffuse solar absorptance with blind on
                                                Real64 ACosTlt = std::abs(Surface(SurfNum).CosTilt);
                                                // Absolute value of cosine of surface tilt angle
                                                Real64 AbsDiffGlassLayGnd =
                                                        InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                        dataConstruction.Construct(ConstrNumSh).BlAbsDiffGnd({1, 19}, Lay));
                                                Real64 AbsDiffGlassLaySky =
                                                        InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                                dataConstruction.Construct(ConstrNumSh).BlAbsDiffSky({1, 19}, Lay));

                                                SurfWinQRadSWwinAbs(Lay, SurfNum) =
                                                        SkySolarInc * (0.5 * ACosTlt * AbsDiffGlassLayGnd + (1.0 - 0.5 * ACosTlt) * AbsDiffGlassLaySky) +
                                                        GndSolarInc * ((1.0 - 0.5 * ACosTlt) * AbsDiffGlassLayGnd + 0.5 * ACosTlt * AbsDiffGlassLaySky) +
                                                        AWinSurf(Lay, SurfNum) * BeamSolar;
                                            }
                                        }

                                        // Total solar absorbed in solid layer (W), for reporting
                                        SurfWinQRadSWwinAbsLayer(Lay, SurfNum) =
                                                SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;

                                        // Total solar absorbed in all glass layers (W), for reporting
                                        SurfWinQRadSWwinAbsTot(SurfNum) += SurfWinQRadSWwinAbsLayer(Lay, SurfNum);
                                    }
                                    SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                                            SurfWinQRadSWwinAbsTot(SurfNum) * TimeStepZoneSec;
                                    // Need to do it this way for now beaucse of scheduled surface gains. They do work only with
                                    // BSDF windows and overwriting absorbtances will work only for ordinary windows
                                    // } else if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel &&
                                    //   SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel &&
                                    //   inExtWindowModel->isExternalLibraryModel() ) {
                                    //   TotSolidLay = Construct( ConstrNum ).TotSolidLayers;
                                    //   for ( Lay = 1; Lay <= TotSolidLay; ++Lay ) {
                                    //     QRadSWwinAbs( Lay, SurfNum ) = AWinSurf( Lay, SurfNum ) *
                                    //       ( QRadSWOutIncident( SurfNum ) + QS( Surface( SurfNum ).Zone ) );
                                    //   }
                                } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                                    int TotSolidLay = dataConstruction.Construct(ConstrNum).TotSolidLayers;
                                    // Number of solid layers in fenestration system (glass + shading)
                                    int CurrentState = SurfaceWindow(SurfNum).ComplexFen.CurrentState;
                                    // Current state for Complex Fenestration
                                    // Examine for schedule surface gain
                                    Real64 SurfSolAbs = WindowScheduledSolarAbs(SurfNum,
                                                                                ConstrNum); // Pointer to scheduled surface gains object for fenestration systems

                                    for (int Lay = 1; Lay <= TotSolidLay; ++Lay) {
                                        if (SurfSolAbs != 0) {
                                            AWinSurf(Lay, SurfNum) = GetCurrentScheduleValue(
                                                    FenLayAbsSSG(SurfSolAbs).SchedPtrs(Lay));
                                            // ABWin(Lay) = AWinSurf(SurfNum,Lay)
                                            SurfWinQRadSWwinAbs(Lay, SurfNum) = AWinSurf(Lay, SurfNum);
                                        } else {
                                            // Several notes about this equation.  First part is accounting for duffuse solar radiation for the ground
                                            // and from the sky.  Second item (AWinSurf(SurfNum,Lay) * BeamSolar) is accounting for absorbed solar
                                            // radiation originating from beam on exterior side.  Third item (AWinCFOverlap(SurfNum,Lay)) is
                                            // accounting for absorptances from beam hitting back of the window which passes through rest of exterior
                                            // windows
                                            SurfWinQRadSWwinAbs(Lay, SurfNum) =
                                                    SurfaceWindow(SurfNum).ComplexFen.State(CurrentState).WinSkyFtAbs(Lay) * SkySolarInc +
                                                    SurfaceWindow(SurfNum).ComplexFen.State(CurrentState).WinSkyGndAbs(Lay) * GndSolarInc +
                                                    AWinSurf(Lay, SurfNum) * BeamSolar +
                                                    AWinCFOverlap(Lay, SurfNum) * BeamSolar;
                                        }
                                        // Total solar absorbed in solid layer (W), for reporting
                                        SurfWinQRadSWwinAbsLayer(Lay, SurfNum) =
                                                SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;

                                        // Total solar absorbed in all glass layers (W), for reporting
                                        SurfWinQRadSWwinAbsTot(SurfNum) += SurfWinQRadSWwinAbsLayer(Lay, SurfNum);
                                    }
                                    SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                                            SurfWinQRadSWwinAbsTot(SurfNum) * TimeStepZoneSec;

                                } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                                    SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                                    // EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr
                                    int TotSolidLay = CFS(dataConstruction.Construct(
                                            Surface(SurfNum).Construction).EQLConsPtr).NL;
                                    for (int Lay = 1; Lay <= TotSolidLay; ++Lay) {
                                        // Absorbed window components include:
                                        // (1) beam solar radiation absorbed by all layers in the fenestration
                                        // (2) sky and ground reflected duffuse solar radiation absorbed by all layers
                                        // (3) diffuse short wave incident on the inside face of the fenestration.  The short wave internal sources
                                        //     include light, ...
                                        AbsDiffWin(Lay) = dataConstruction.Construct(ConstrNum).AbsDiffFrontEQL(
                                                Lay);
                                        SurfWinQRadSWwinAbs(Lay, SurfNum) = AWinSurf(Lay, SurfNum) * BeamSolar +
                                                                            AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc);

                                        // Total solar absorbed in solid layer (W), for reporting
                                        SurfWinQRadSWwinAbsLayer(Lay, SurfNum) =
                                                SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;

                                        // Total solar absorbed in all glass layers (W), for reporting
                                        SurfWinQRadSWwinAbsTot(SurfNum) += SurfWinQRadSWwinAbsLayer(Lay, SurfNum);
                                    }
                                    SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                                            SurfWinQRadSWwinAbsTot(SurfNum) * TimeStepZoneSec;
                                } else if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
                                    int SurfNum2 = SurfNum;
                                    if (SurfWinOriginalClass(SurfNum) == SurfaceClass_TDD_Diffuser) {
                                        SurfNum2 = TDDPipe(SurfWinTDDPipeNum(SurfNum)).Dome;
                                    }
                                    std::pair<Real64, Real64> incomingAngle =
                                            getSunWCEAngles(state, SurfNum2, BSDFHemisphere::Incoming);
                                    Real64 Theta = incomingAngle.first;
                                    Real64 Phi = incomingAngle.second;

                                    std::shared_ptr<CMultiLayerScattered> aLayer = CWindowConstructionsSimplified::instance().getEquivalentLayer(
                                        state, WavelengthRange::Solar, ConstrNum);
                                    size_t totLayers = aLayer->getNumOfLayers();
                                    for (size_t Lay = 1; Lay <= totLayers; ++Lay) {

                                        Real64 AbWinDiff = aLayer->getAbsorptanceLayer(Lay, Side::Front,
                                                                                       ScatteringSimple::Diffuse,
                                                                                       Theta, Phi);

                                        SurfWinQRadSWwinAbs(Lay, SurfNum) =
                                                AbWinDiff * (SkySolarInc + GndSolarInc) +
                                                AWinSurf(Lay, SurfNum) * BeamSolar;

                                        // Total solar absorbed in solid layer (W), for reporting
                                        SurfWinQRadSWwinAbsLayer(Lay, SurfNum) =
                                                SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;

                                        // Total solar absorbed in all glass layers (W), for reporting
                                        SurfWinQRadSWwinAbsTot(SurfNum) += SurfWinQRadSWwinAbsLayer(Lay, SurfNum);
                                    }
                                }

                                // Solar absorbed by window frame and dividers
                                int FrDivNum = Surface(SurfNum).FrameDivider; // Frame/divider number
                                if (FrDivNum > 0) {
                                    Real64 FrArea = SurfWinFrameArea(SurfNum); // Frame, divider area (m2)
                                    Real64 FrProjOut = FrameDivider(FrDivNum).FrameProjectionOut; // Frame, divider outside projection (m)
                                    Real64 FrProjIn = FrameDivider(FrDivNum).FrameProjectionIn;
                                    Real64 DivArea = SurfWinDividerArea(SurfNum);
                                    Real64 DivWidth = FrameDivider(FrDivNum).DividerWidth;
                                    Real64 DivProjOut = FrameDivider(FrDivNum).DividerProjectionOut;
                                    Real64 DivProjIn = FrameDivider(FrDivNum).DividerProjectionIn;
                                    Real64 CosIncAngHorProj = 0.0; // Cosine of incidence angle of sun on horizontal faces of a frame or divider projection
                                    Real64 CosIncAngVertProj = 0.0; // Cosine of incidence angle of sun on vertical faces of a frame or divider projection
                                    Real64 FracSunLit = 0.0; // Fraction of window sunlit this time step
                                    Real64 BeamFaceInc; // Beam solar incident window plane this time step (W/m2)
                                    Real64 DifSolarFaceInc; // Diffuse solar incident on window plane this time step (W/m2)
                                    if (FrArea > 0.0 || DivArea > 0.0) {
                                        FracSunLit = SunlitFrac(TimeStep, HourOfDay, SurfNum);
                                        BeamFaceInc =
                                                BeamSolarRad * SunlitFrac(TimeStep, HourOfDay, SurfNum) * CosInc;
                                        DifSolarFaceInc = SkySolarInc + GndSolarInc;
                                    }
                                    if (FracSunLit > 0.0) {
                                        if ((FrArea > 0.0 && (FrProjOut > 0.0 || FrProjIn > 0.0)) ||
                                            (DivArea > 0.0 && (DivProjOut > 0.0 || DivProjIn > 0.0))) {
                                            // Dot products used to calculate beam solar incident on faces of
                                            // frame and divider perpendicular to the glass surface.
                                            // Note that SOLCOS is the current timestep's solar direction cosines.
                                            //                  PhiWin = ASIN(WALCOS(3,SurfNum))
                                            Real64 PhiWin = std::asin(Surface(SurfNum).OutNormVec(
                                                    3)); // Altitude and azimuth angle of outward window normal (radians)
                                            Real64 ThWin = std::atan2(Surface(SurfNum).OutNormVec(2),
                                                                      Surface(SurfNum).OutNormVec(1));
                                            Real64 PhiSun = std::asin(
                                                    SOLCOS(3)); // Altitude and azimuth angle of sun (radians)
                                            Real64 ThSun = std::atan2(SOLCOS(2), SOLCOS(1));
                                            Real64 const cos_PhiWin(std::cos(PhiWin));
                                            Real64 const cos_PhiSun(std::cos(PhiSun));
                                            CosIncAngHorProj = std::abs(
                                                    std::sin(PhiWin) * cos_PhiSun * std::cos(ThWin - ThSun) -
                                                    cos_PhiWin * std::sin(PhiSun));
                                            CosIncAngVertProj = std::abs(
                                                    cos_PhiWin * cos_PhiSun * std::sin(ThWin - ThSun));
                                        }
                                    }
                                    // Frame solar
                                    // (A window shade or blind, if present, is assumed to not shade the frame, so no special
                                    // treatment of frame solar needed if window has an exterior shade or blind.)
                                    if (FrArea > 0.0) {
                                        Real64 FrIncSolarOut = BeamFaceInc; // Total solar incident on outside offrame including solar
                                        Real64 FrIncSolarIn = 0.0; // Total solar incident on inside offrame including solar on frame projection (W/m2)
                                        Real64 TransDiffGl = 0.0; // Diffuse solar transmittance
                                        if (FrProjOut > 0.0 || FrProjIn > 0.0) {
                                            Real64 BeamFrHorFaceInc = BeamSolarRad * CosIncAngHorProj *
                                                                      (Surface(SurfNum).Width -
                                                                       FrameDivider(FrDivNum).VertDividers *
                                                                       DivWidth) * FracSunLit / FrArea;
                                            Real64 BeamFrVertFaceInc = BeamSolarRad * CosIncAngVertProj *
                                                                       (Surface(SurfNum).Height -
                                                                        FrameDivider(FrDivNum).HorDividers *
                                                                        DivWidth) * FracSunLit / FrArea;
                                            // Beam solar on outside of frame
                                            FrIncSolarOut += (BeamFrHorFaceInc + BeamFrVertFaceInc) * FrProjOut;
                                            if (FrProjIn > 0.0) {
                                                Real64 TransGl = POLYF(CosInc, dataConstruction.Construct(
                                                        ConstrNum).TransSolBeamCoef);
                                                TransDiffGl = dataConstruction.Construct(ConstrNum).TransDiff;
                                                if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                                                    Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum);
                                                    int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                                    Real64 TransGlSh = POLYF(CosInc, dataConstruction.Construct(
                                                            ConstrNumSh).TransSolBeamCoef);
                                                    TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                                    Real64 TransDiffGlSh = dataConstruction.Construct(
                                                            ConstrNumSh).TransDiff;
                                                    TransDiffGl = InterpSw(SwitchFac, TransDiffGl, TransDiffGlSh);
                                                }
                                                // Beam solar on inside of frame
                                                FrIncSolarIn =
                                                        (BeamFrHorFaceInc + BeamFrVertFaceInc) * FrProjIn * TransGl;
                                            }
                                        }
                                        // Beam plus diffuse solar on outside of frame
                                        FrIncSolarOut +=
                                                DifSolarFaceInc * (1.0 + 0.5 * SurfWinProjCorrFrOut(SurfNum));
                                        SurfWinFrameQRadOutAbs(SurfNum) =
                                                FrIncSolarOut * SurfWinFrameSolAbsorp(SurfNum);
                                        // Add diffuse from beam reflected from window outside reveal surfaces
                                        SurfWinFrameQRadOutAbs(SurfNum) +=
                                                BeamSolarRad * SurfWinOutsRevealDiffOntoFrame(SurfNum) *
                                                SurfWinFrameSolAbsorp(SurfNum);

                                        // Beam plus diffuse solar on inside of frame
                                        FrIncSolarIn +=
                                                DifSolarFaceInc * TransDiffGl * 0.5 * SurfWinProjCorrFrIn(SurfNum);
                                        SurfWinFrameQRadInAbs(SurfNum) =
                                                FrIncSolarIn * SurfWinFrameSolAbsorp(SurfNum);
                                        // Add diffuse from beam reflected from window inside reveal surfaces
                                        SurfWinFrameQRadInAbs(SurfNum) +=
                                                BeamSolarRad * SurfWinInsRevealDiffOntoFrame(SurfNum) *
                                                SurfWinFrameSolAbsorp(SurfNum);
                                    }

                                    // Divider solar
                                    // (An exterior shade or blind, when in place, is assumed to completely cover the divider.
                                    // Dividers are not allowed on windows with between-glass shade/blind so DivProjOut and
                                    // DivProjIn will be zero in this case.)
                                    if (DivArea > 0.0) { // Solar absorbed by window divider
                                        Real64 DividerAbs = SurfWinDividerSolAbsorp(
                                                SurfNum); // Window divider solar absorptance
                                        if (SurfWinDividerType(SurfNum) == Suspended) {
                                            // Suspended (between-glass) divider; account for effect glass on outside of divider
                                            // (note that outside and inside projection for this type of divider are both zero)
                                            int MatNumGl = dataConstruction.Construct(ConstrNum).LayerPoint(
                                                    1); // Outer glass layer material number
                                            Real64 TransGl = dataMaterial.Material(
                                                    MatNumGl).Trans; // Outer glass layer material number, switched construction
                                            Real64 ReflGl = dataMaterial.Material(MatNumGl).ReflectSolBeamFront;
                                            Real64 AbsGl = 1.0 - TransGl - ReflGl;
                                            Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum);
                                            int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                            if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                                                Real64 MatNumGlSh = dataConstruction.Construct(
                                                        ConstrNumSh).LayerPoint(1);
                                                Real64 TransGlSh = dataMaterial.Material(MatNumGlSh).Trans;
                                                Real64 ReflGlSh = dataMaterial.Material(
                                                        MatNumGlSh).ReflectSolBeamFront;
                                                Real64 AbsGlSh = 1.0 - TransGlSh - ReflGlSh;
                                                TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                                ReflGl = InterpSw(SwitchFac, ReflGl, ReflGlSh);
                                                AbsGl = InterpSw(SwitchFac, AbsGl, AbsGlSh);
                                            }
                                            Real64 DividerRefl =
                                                    1.0 - DividerAbs; // Window divider solar reflectance
                                            DividerAbs = AbsGl + TransGl * (DividerAbs + DividerRefl * AbsGl) /
                                                                 (1.0 - DividerRefl * ReflGl);
                                        }

                                        Real64 BeamDivHorFaceInc = 0.0; // Beam solar on divider's horizontal outside projection faces (W/m2)
                                        Real64 BeamDivVertFaceInc = 0.0; // Beam solar on divider's vertical outside projection faces (W/m2)
                                        // Beam incident on horizontal and vertical projection faces of divider if no exterior shading
                                        if (DivProjOut > 0.0 && ShadeFlag != ExtShadeOn &&
                                            ShadeFlag != ExtBlindOn && ShadeFlag != ExtScreenOn) {
                                            BeamDivHorFaceInc = BeamSolarRad * CosIncAngHorProj *
                                                                FrameDivider(FrDivNum).HorDividers * DivProjOut *
                                                                (Surface(SurfNum).Width -
                                                                 FrameDivider(FrDivNum).VertDividers * DivWidth) *
                                                                FracSunLit / DivArea;
                                            BeamDivVertFaceInc = BeamSolarRad * CosIncAngVertProj *
                                                                 FrameDivider(FrDivNum).VertDividers * DivProjOut *
                                                                 (Surface(SurfNum).Height -
                                                                  FrameDivider(FrDivNum).HorDividers * DivWidth) *
                                                                 FracSunLit / DivArea;
                                        }
                                        Real64 DivIncSolarOutBm = 0.0; // Diffuse solar incident on outside of divider including beam on divider projection (W/m2)
                                        Real64 DivIncSolarOutDif = 0.0; // Diffuse solar incident on outside of divider including diffuse on divider projection (W/m2)
                                        Real64 DivIncSolarInBm = 0.0; // Diffuse solar incident on inside of divider including beam on divider projection (W/m2)
                                        Real64 DivIncSolarInDif = 0.0; // Diffuse solar incident on inside of divider including diffuse on divider projection (W/m2)
                                        if (ShadeFlag != ExtShadeOn && ShadeFlag != ExtBlindOn &&
                                            ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn &&
                                            ShadeFlag != ExtScreenOn) { // No exterior or between-glass shading
                                            DivIncSolarOutBm = BeamFaceInc + BeamDivHorFaceInc + BeamDivVertFaceInc;
                                            DivIncSolarOutDif =
                                                    DifSolarFaceInc * (1.0 + SurfWinProjCorrDivOut(SurfNum));
                                            if (DivProjIn > 0.0) {
                                                Real64 TransGl = POLYF(CosInc, dataConstruction.Construct(
                                                        ConstrNum).TransSolBeamCoef);
                                                Real64 TransDiffGl = dataConstruction.Construct(
                                                        ConstrNum).TransDiff; // Diffuse solar transmittance
                                                if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                                                    Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum);
                                                    int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                                    Real64 TransGlSh = POLYF(CosInc, dataConstruction.Construct(
                                                            ConstrNumSh).TransSolBeamCoef);
                                                    // Outer glass solar trans, refl, absorptance if switched
                                                    TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                                    Real64 TransDiffGlSh = dataConstruction.Construct(
                                                            ConstrNumSh).TransDiff;
                                                    // Diffuse solar transmittance, switched construction
                                                    TransDiffGl = InterpSw(SwitchFac, TransDiffGl, TransDiffGlSh);
                                                }
                                                // Beam plus diffuse solar on inside of divider
                                                // BeamDivHorFaceIncIn - Beam solar on divider's horizontal inside projection faces (W/m2)
                                                // BeamDivVertFaceIncIn - Beam solar on divider's vertical inside projection faces (W/m2)
                                                Real64 BeamDivHorFaceIncIn = BeamSolarRad * CosIncAngHorProj *
                                                                             FrameDivider(FrDivNum).HorDividers *
                                                                             DivProjIn * (Surface(SurfNum).Width -
                                                                                          FrameDivider(
                                                                                                  FrDivNum).VertDividers *
                                                                                          DivWidth) * FracSunLit /
                                                                             DivArea;
                                                Real64 BeamDivVertFaceIncIn = BeamSolarRad * CosIncAngVertProj *
                                                                              FrameDivider(FrDivNum).VertDividers *
                                                                              DivProjIn * (Surface(SurfNum).Height -
                                                                                           FrameDivider(
                                                                                                   FrDivNum).HorDividers *
                                                                                           DivWidth) * FracSunLit /
                                                                              DivArea;
                                                DivIncSolarInBm =
                                                        TransGl * (BeamDivHorFaceIncIn + BeamDivVertFaceIncIn);
                                                DivIncSolarInDif = TransDiffGl * DifSolarFaceInc *
                                                                   SurfWinProjCorrDivIn(SurfNum);
                                            }
                                        } else { // Exterior shade, screen or blind present

                                            DivIncSolarOutBm = BeamFaceInc * (1.0 + SurfWinProjCorrDivOut(SurfNum));
                                            DivIncSolarOutDif =
                                                    DifSolarFaceInc * (1.0 + SurfWinProjCorrDivOut(SurfNum));
                                            DivIncSolarInBm = BeamFaceInc * SurfWinProjCorrDivIn(SurfNum) *
                                                              dataConstruction.Construct(ConstrNum).TransDiff;
                                            DivIncSolarInDif = DifSolarFaceInc * SurfWinProjCorrDivIn(SurfNum) *
                                                               dataConstruction.Construct(ConstrNum).TransDiff;
                                        }
                                        if (ShadeFlag != ExtShadeOn && ShadeFlag != ExtBlindOn &&
                                            ShadeFlag != ExtScreenOn && ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn) {
                                            // No exterior or between-glass shade, screen or blind
                                            SurfWinDividerQRadOutAbs(SurfNum) =
                                                    DividerAbs * (DivIncSolarOutBm + DivIncSolarOutDif);
                                            SurfWinDividerQRadInAbs(SurfNum) =
                                                    DividerAbs * (DivIncSolarInBm + DivIncSolarInDif);
                                            // Exterior shade, screen or blind
                                        } else if (ShadeFlag == ExtBlindOn) { // Exterior blind
                                            int BlNum = SurfWinBlindNumber(SurfNum);
                                            Real64 ProfAng; // Solar profile angle (rad)
                                            ProfileAngle(SurfNum, SOLCOS, Blind(BlNum).SlatOrientation, ProfAng);
                                            Real64 SlatAng = SurfWinSlatAngThisTS(SurfNum); // Slat angle (rad)
                                            // TBlBmBm - Blind beam-beam solar transmittance
                                            // TBlBmDif - Blind diffuse-diffuse solar transmittance
                                            Real64 TBlBmBm = BlindBeamBeamTrans(ProfAng, SlatAng,
                                                                                Blind(BlNum).SlatWidth,
                                                                                Blind(BlNum).SlatSeparation,
                                                                                Blind(BlNum).SlatThickness);
                                            Real64 TBlBmDif = InterpProfSlatAng(ProfAng, SlatAng,
                                                                                SurfWinMovableSlats(SurfNum),
                                                                                Blind(BlNum).SolFrontBeamDiffTrans);
                                            SurfWinDividerQRadOutAbs(SurfNum) =
                                                    DividerAbs * (DivIncSolarOutBm * (TBlBmBm + TBlBmDif) +
                                                    DivIncSolarOutDif * InterpSlatAng(SlatAng, SurfWinMovableSlats(SurfNum),
                                                            Blind(BlNum).SolFrontDiffDiffTrans));
                                            SurfWinDividerQRadInAbs(SurfNum) =
                                                    DividerAbs * (DivIncSolarInBm * (TBlBmBm + TBlBmDif) +
                                                    DivIncSolarInDif * InterpSlatAng(SlatAng, SurfWinMovableSlats(SurfNum),
                                                            Blind(BlNum).SolFrontDiffDiffTrans));

                                        } else if (ShadeFlag == ExtShadeOn) { // Exterior shade
                                            int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                            SurfWinDividerQRadOutAbs(SurfNum) =
                                                    DividerAbs * dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).Trans *
                                                    (DivIncSolarOutBm +DivIncSolarOutDif);
                                            SurfWinDividerQRadInAbs(SurfNum) =
                                                    DividerAbs * dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).Trans *
                                                    (DivIncSolarInBm +DivIncSolarInDif);

                                        } else if (ShadeFlag == ExtScreenOn) { // Exterior screen
                                            SurfWinDividerQRadOutAbs(SurfNum) = DividerAbs * (SurfaceScreens(
                                                    SurfWinScreenNumber(SurfNum)).BmBmTrans + SurfaceScreens(
                                                    SurfWinScreenNumber(SurfNum)).BmDifTrans) *
                                                                                (DivIncSolarOutBm +
                                                                                 DivIncSolarOutDif);
                                            SurfWinDividerQRadInAbs(SurfNum) = DividerAbs * (SurfaceScreens(
                                                    SurfWinScreenNumber(SurfNum)).BmBmTrans + SurfaceScreens(
                                                    SurfWinScreenNumber(SurfNum)).BmDifTrans) *
                                                                               (DivIncSolarInBm +
                                                                                DivIncSolarInDif);
                                        }
                                    }
                                }

                            } // RoughIndexMovInsul <= 0, no movable insulation
                        }  // Surface(SurfNum)%HeatTransSurf
                    } // Surface(SurfNum)%ExtSolar
                } // end of surface window loop
            } // end of zone loop
            for (int PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum) {
                int SurfNum = TDDPipe(PipeNum).Dome; // TDD: DOME object number
                int ConstrNum = Surface(SurfNum).Construction;
                int TotGlassLay = dataConstruction.Construct(ConstrNum).TotGlassLayers; // Number of glass layers
                SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                    AbsDiffWin(Lay) = dataConstruction.Construct(ConstrNum).AbsDiff(Lay);
                    SurfWinQRadSWwinAbs(Lay, SurfNum) = AbsDiffWin(Lay) * (currSkySolarInc(SurfNum) + currGndSolarInc(SurfNum)) +
                                                        AWinSurf(Lay, SurfNum) * currBeamSolar(SurfNum);
                    SurfWinQRadSWwinAbsLayer(Lay, SurfNum) = SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                    SurfWinQRadSWwinAbsTot(SurfNum) += SurfWinQRadSWwinAbsLayer(Lay, SurfNum);
                }
                SurfWinQRadSWwinAbsTotEnergy(SurfNum) = SurfWinQRadSWwinAbsTot(SurfNum) * TimeStepZoneSec;
            }
        } // End of sun-up check
    }

    void InitIntSolarDistribution()
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
        using General::InterpSlatAng;
        using General::InterpSw;
        using namespace HeatBalanceMovableInsulation;
        using DaylightingDevices::DistributeTDDAbsorbedSolar;
        using namespace DataWindowEquivalentLayer;
        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AbsExt;          // Solar absorptance of outermost layer (or movable insulation if present)
        Real64 AbsInt;          // Inside opaque surface solar absorptance
        Real64 AbsIntSurf;      // Inside opaque surface solar absorptance
        Real64 AbsIntSurfVis;   // Inside opaque surface visible absorptance
        Real64 HMovInsul;       // Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
        int RoughIndexMovInsul; // Roughness index of movable insulation
        int ConstrNum;          // Construction number
        int ConstrNumSh;        // Shaded construction number
        int SurfNumAdjZone;     // Surface number in adjacent zone for interzone surfaces
        int IGlass;             // Glass layer counter
        int ShadeFlag;          // Shading flag
        Real64 DividerThermAbs; // Window divider thermal absorptance
        Real64 DividerSolAbs;   // Window divider solar absorptance
        Real64 DividerSolRefl;  // Window divider solar reflectance
        int MatNumGl;           // Glass layer material number
        int MatNumSh;           // Shade layer material number
        Real64 TransGl;         // Glass layer solar transmittance, reflectance, absorptance
        Real64 ReflGl;
        Real64 AbsGl;
        int BlNum;                // Blind number
        int TotGlassLayers;       // Number of glass layers in a window construction
        Real64 BlAbsDiffBk;       // Glass layer back diffuse solar absorptance when blind in place
        Real64 AbsDiffBkBl;       // Blind diffuse back solar absorptance as part of glazing system
        Real64 EffBlEmiss;        // Blind emissivity (thermal absorptance) as part of glazing system
        Real64 pulseMultipler;    // use to create a pulse for the load component report computations
        static Real64 curQL(0.0); // radiant value prior to adjustment for pulse for load component report
        static Real64 adjQL(0.0); // radiant value including adjustment for pulse for load component report
        int EQLNum;               // equivalent layer fenestration index
        int Lay;                  // equivalent layer fenestration layer index

        // FLOW:

        if (!allocated(QS)) QS.allocate(DataViewFactorInformation::NumOfSolarEnclosures);
        if (!allocated(QSLights)) QSLights.allocate(DataViewFactorInformation::NumOfSolarEnclosures);

        QS = 0.0;
        QSLights = 0.0;

        // COMPUTE TOTAL SHORT-WAVE RADIATION ORIGINATING IN ZONE.
        // Note: If sun is not up, QS is only internal gains
        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            Real64 sumZoneQLTSW = 0.0;
            for (int zoneNum : DataViewFactorInformation::ZoneSolarInfo(enclosureNum).ZoneNums) {
                sumZoneQLTSW += ZoneIntGain(zoneNum).QLTSW;
            }
            QS(enclosureNum) = ZoneQD(enclosureNum) + sumZoneQLTSW;
            QSLights(enclosureNum) = sumZoneQLTSW;
        }

        if (InterZoneWindow) { // DO INTERZONE DISTRIBUTION.

            for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {

                if (RecDifShortFromZ(enclosureNum)) {

                    for (int OtherenclosureNum = 1; OtherenclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++OtherenclosureNum) {

                        if ((OtherenclosureNum != enclosureNum) && (RecDifShortFromZ(OtherenclosureNum))) {
                            Real64 sumZoneQLTSW = 0.0;
                            for (int zoneNum : DataViewFactorInformation::ZoneSolarInfo(OtherenclosureNum).ZoneNums) {
                                sumZoneQLTSW += ZoneIntGain(zoneNum).QLTSW;
                            }
                            QS(enclosureNum) += FractDifShortZtoZ(enclosureNum, OtherenclosureNum) * (ZoneQD(OtherenclosureNum) + sumZoneQLTSW);
                            ZoneDifSolFrIntWinsRep(enclosureNum) += FractDifShortZtoZ(enclosureNum, OtherenclosureNum) * ZoneQD(OtherenclosureNum);
                            ZoneDifSolFrIntWinsRepEnergy(enclosureNum) = ZoneDifSolFrIntWinsRep(enclosureNum) * TimeStepZoneSec;
                        }
                    }
                }
            }
        }

        // Beam and diffuse solar on inside surfaces from interior windows (for reporting)

        if (SunIsUp) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                //!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
                if (Surface(SurfNum).Class == SurfaceClass_Shading) continue;
                int const enclosureNum = Surface(SurfNum).SolarEnclIndex;
                SurfIntBmIncInsSurfIntensRep(SurfNum) =
                    ZoneBmSolFrIntWinsRep(enclosureNum) / DataViewFactorInformation::ZoneSolarInfo(enclosureNum).TotalSurfArea;
                SurfIntBmIncInsSurfAmountRep(SurfNum) = SurfIntBmIncInsSurfIntensRep(SurfNum) * (Surface(SurfNum).Area + SurfWinDividerArea(SurfNum));
                SurfIntBmIncInsSurfAmountRepEnergy(SurfNum) = SurfIntBmIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec;
                //      IntDifIncInsSurfIntensRep(SurfNum) = ZoneDifSolFrIntWinsRep(ZoneNum)/Zone(ZoneNum)%TotalSurfArea
                //      IntDifIncInsSurfAmountRep(SurfNum) = IntDifIncInsSurfIntensRep(SurfNum) *  &
                //                                             (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
                //      IntDifIncInsSurfAmountRepEnergy(SurfNum) = IntDifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
            }
        }

        // COMPUTE CONVECTIVE GAINS AND ZONE FLUX DENSITY.
        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            if (InterZoneWindow) {
                QS(enclosureNum) *= FractDifShortZtoZ(enclosureNum, enclosureNum) * ZoneVMULT(enclosureNum);
                // CR 8695, VMULT not based on visible
                QSLights(enclosureNum) *= FractDifShortZtoZ(enclosureNum, enclosureNum) * ZoneVMULT(enclosureNum);
            } else {
                QS(enclosureNum) *= ZoneVMULT(enclosureNum);
                QSLights(enclosureNum) *= ZoneVMULT(enclosureNum);
            }
        }

        // COMPUTE RADIANT GAINS ON SURFACES
        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfOpague = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpague = Zone(zoneNum).NonWindowSurfaceLast;
            int const radEnclosureNum = Zone(zoneNum).RadiantEnclosureNum;
            int const solEnclosureNum = Zone(zoneNum).SolarEnclosureNum;
            for (int SurfNum = firstSurfOpague; SurfNum <= lastSurfOpague; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf || zoneNum == 0) continue; // Skip non-heat transfer surfaces
                if (Surface(SurfNum).Class == SurfaceClass_TDD_Dome) continue; // Skip tubular daylighting device domes
                ConstrNum = Surface(SurfNum).Construction;

                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                    AbsIntSurf = dataConstruction.Construct(ConstrNum).InsideAbsorpSolar;
                    AbsIntSurfVis = dataConstruction.Construct(
                            ConstrNum).InsideAbsorpSolar; // to fix CR 8695 change to this = Construct(ConstrNum)%InsideAbsorpVis
                    HMovInsul = 0.0;
                    if (Surface(SurfNum).MaterialMovInsulInt > 0)
                        EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
                    if (HMovInsul > 0.0) AbsIntSurf = AbsInt;
                    SurfOpaqQRadSWInAbs(SurfNum) += QS(solEnclosureNum) * AbsIntSurf;
                    SurfOpaqQRadSWLightsInAbs(SurfNum) += QSLights(solEnclosureNum) * AbsIntSurfVis;
                }

                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque exterior surface
                    // Calculate absorbed solar on outside if movable exterior insulation in place
                    HMovInsul = 0.0;
                    if (Surface(SurfNum).MaterialMovInsulExt > 0) EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt);
                    if (HMovInsul > 0) { // Movable outside insulation in place
                        SurfQRadSWOutMvIns(SurfNum) = SurfOpaqQRadSWOutAbs(SurfNum) * AbsExt / dataMaterial.Material(dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpSolar;
                        // For transparent insulation, allow some sunlight to get through the movable insulation.
                        // The equation below is derived by taking what is transmitted through the layer and applying
                        // the fraction that is absorbed plus the back reflected portion (first order reflection only)
                        // to the plane between the transparent insulation and the exterior surface face.
                        SurfOpaqQRadSWOutAbs(SurfNum) = dataMaterial.Material(Surface(SurfNum).MaterialMovInsulExt).Trans * SurfQRadSWOutMvIns(SurfNum) *
                                                ((dataMaterial.Material(dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpSolar / AbsExt) +
                                                 (1 - dataMaterial.Material(dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpSolar));
                        SurfOpaqSWOutAbsTotalReport(SurfNum) = SurfOpaqQRadSWOutAbs(SurfNum) * Surface(SurfNum).Area;
                        SurfOpaqSWOutAbsEnergyReport(SurfNum) = SurfOpaqSWOutAbsTotalReport(SurfNum) * TimeStepZoneSec;
                    }
                }
                // RJH 08/30/07 - Add InitialDifSolInAbs, InitialDifSolwinAbs, and InitialDifSolAbsByShade
                // calced in CalcWinTransDifSolInitialDistribution to QRadSWInAbs, QRadSWwinAbs, and IntSWAbsByShade here
                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                    SurfOpaqQRadSWInAbs(SurfNum) += SurfOpaqInitialDifSolInAbs(SurfNum);
                }

                // Report
//                SWwinAbsTotalReport(SurfNum) = 0.0;
                SurfSWInAbsTotalReport(SurfNum) = 0.0;
                SurfInitialDifSolInAbsReport(SurfNum) = 0.0;
//                SurfWinInitialDifSolInTransReport(SurfNum) = 0.0; //TOCHECK
                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                    // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                    SurfInitialDifSolInAbsReport(SurfNum) = SurfOpaqInitialDifSolInAbs(SurfNum) * Surface(SurfNum).Area;
                    // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                    SurfSWInAbsTotalReport(SurfNum) = SurfOpaqQRadSWInAbs(SurfNum) * Surface(SurfNum).Area;
                }
            } // end of opaque

            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) { // Window
                if (!Surface(SurfNum).HeatTransSurf || zoneNum == 0) continue; // Skip non-heat transfer surfaces
                if (Surface(SurfNum).Class == SurfaceClass_TDD_Dome) continue; // Skip tubular daylighting device domes
                ConstrNum = Surface(SurfNum).Construction;

                if (SurfWinWindowModelType(SurfNum) != WindowEQLModel) {

                    ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                    if (SurfWinStormWinFlag(SurfNum) == 1) {
                        ConstrNum = Surface(SurfNum).StormWinConstruction;
                        ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                    }
                    TotGlassLayers = dataConstruction.Construct(ConstrNum).TotGlassLayers;
                    ShadeFlag = SurfWinShadingFlag(SurfNum);

                    // These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
                    pulseMultipler = 0.01; // the W/sqft pulse for the zone
                    if (!doLoadComponentPulseNow) {
                        SurfQRadThermInAbs(SurfNum) = QL(radEnclosureNum) * TMULT(radEnclosureNum) * ITABSF(SurfNum);
                    } else {
                        curQL = QL(radEnclosureNum);
                        // for the loads component report during the special sizing run increase the radiant portion
                        // a small amount to create a "pulse" of heat that is used for the
                        adjQL = curQL +
                                DataViewFactorInformation::ZoneRadiantInfo(radEnclosureNum).FloorArea * pulseMultipler;
                        // ITABSF is the Inside Thermal Absorptance
                        // TMULT is a multiplier for each zone/enclosure
                        // QRadThermInAbs is the thermal radiation absorbed on inside surfaces
                        SurfQRadThermInAbs(SurfNum) = adjQL * TMULT(radEnclosureNum) * ITABSF(SurfNum);
                    }

                    if (ShadeFlag <= 0) { // No window shading
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                            SurfWinQRadSWwinAbs(IGlass, SurfNum) +=
                                    QS(solEnclosureNum) * dataConstruction.Construct(ConstrNum).AbsDiffBack(IGlass);
                        }
                    } else if (ConstrNumSh != 0 && (ShadeFlag == IntShadeOn || ShadeFlag >= 3)) {
                        // Interior, exterior or between-glass shade, screen or blind in place
                        for (IGlass = 1; IGlass <= dataConstruction.Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                            if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn ||
                                ShadeFlag == ExtScreenOn)
                                SurfWinQRadSWwinAbs(IGlass, SurfNum) += QS(solEnclosureNum) *
                                                                 dataConstruction.Construct(ConstrNumSh).AbsDiffBack(
                                                                         IGlass);
                            if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn) {
                                BlAbsDiffBk = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                            dataConstruction.Construct(ConstrNumSh).BlAbsDiffBack(_,
                                                                                                                  IGlass));
                                SurfWinQRadSWwinAbs(IGlass, SurfNum) += QS(solEnclosureNum) * BlAbsDiffBk;
                            }
                        }
                        BlNum = SurfWinBlindNumber(SurfNum);
                        if (ShadeFlag == IntShadeOn)
                            SurfWinIntLWAbsByShade(SurfNum) =
                                    QL(radEnclosureNum) * dataConstruction.Construct(ConstrNumSh).ShadeAbsorpThermal *
                                    TMULT(radEnclosureNum);
                        if (ShadeFlag == IntBlindOn) {
                            EffBlEmiss = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                       SurfaceWindow(SurfNum).EffShBlindEmiss);
                            SurfWinIntLWAbsByShade(SurfNum) = QL(radEnclosureNum) * EffBlEmiss * TMULT(radEnclosureNum);
                        }
                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn ||
                            ShadeFlag == ExtScreenOn)
                            SurfWinIntSWAbsByShade(SurfNum) =
                                    QS(solEnclosureNum) * dataConstruction.Construct(ConstrNumSh).AbsDiffBackShade;
                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                            AbsDiffBkBl = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                        dataConstruction.Construct(ConstrNumSh).AbsDiffBackBlind);
                            SurfWinIntSWAbsByShade(SurfNum) = QS(solEnclosureNum) * AbsDiffBkBl;

                        }
                        // Correct for divider shadowing
                        if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn)
                            SurfWinIntSWAbsByShade(SurfNum) *= SurfWinGlazedFrac(SurfNum);

                    } else if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {

                            SurfWinQRadSWwinAbs(IGlass, SurfNum) += QS(solEnclosureNum) *
                                                             InterpSw(SurfWinSwitchingFactor(SurfNum),
                                                                      dataConstruction.Construct(ConstrNum).AbsDiffBack(
                                                                              IGlass), dataConstruction.Construct(
                                                                             ConstrNumSh).AbsDiffBack(IGlass));
                        }

                    } // End of shading flag check

                    // Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
                    if (SurfWinFrameArea(SurfNum) > 0.0)
                        SurfWinFrameQRadInAbs(SurfNum) += (QS(solEnclosureNum) * SurfWinFrameSolAbsorp(SurfNum) +
                                                           (QL(radEnclosureNum) * TMULT(radEnclosureNum) +
                                                            QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) +
                                                            QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) +
                                                            QElecBaseboardSurf(SurfNum)) * SurfWinFrameEmis(SurfNum)) *
                                                          (1.0 +
                                                           0.5 * SurfWinProjCorrFrIn(SurfNum)); // Window has a frame
                    if (SurfWinDividerArea(SurfNum) > 0.0) {            // Window has dividers
                        DividerThermAbs = SurfWinDividerEmis(SurfNum);
                        DividerSolAbs = SurfWinDividerSolAbsorp(SurfNum);
                        if (SurfWinDividerType(SurfNum) == Suspended) { // Suspended divider; account for inside glass
                            MatNumGl = dataConstruction.Construct(ConstrNum).LayerPoint(
                                    dataConstruction.Construct(ConstrNum).TotLayers);
                            TransGl = dataMaterial.Material(MatNumGl).Trans;
                            ReflGl = dataMaterial.Material(MatNumGl).ReflectSolBeamBack;
                            AbsGl = 1.0 - TransGl - ReflGl;
                            DividerSolRefl = 1.0 - DividerSolAbs;
                            DividerSolAbs = AbsGl + TransGl * (DividerSolAbs + DividerSolRefl * AbsGl) /
                                                    (1.0 - DividerSolRefl * ReflGl);
                            DividerThermAbs = dataMaterial.Material(MatNumGl).AbsorpThermalBack;
                        }
                        // Correct for interior shade transmittance
                        if (ShadeFlag == IntShadeOn) {
                            MatNumSh = dataConstruction.Construct(ConstrNumSh).LayerPoint(
                                    dataConstruction.Construct(ConstrNumSh).TotLayers);
                            DividerSolAbs *= dataMaterial.Material(MatNumSh).Trans;
                            DividerThermAbs *= dataMaterial.Material(MatNumSh).TransThermal;
                        } else if (ShadeFlag == IntBlindOn) {
                            DividerSolAbs *= InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                           Blind(BlNum).SolBackDiffDiffTrans);
                            DividerThermAbs *= InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                             SurfWinMovableSlats(SurfNum), Blind(BlNum).IRBackTrans);
                        }
                        // Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains
                        SurfWinDividerQRadInAbs(SurfNum) += (QS(solEnclosureNum) * DividerSolAbs +
                                                             (QL(radEnclosureNum) * TMULT(radEnclosureNum) +
                                                              QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) +
                                                              QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) +
                                                              QElecBaseboardSurf(SurfNum)) * DividerThermAbs) *
                                                            (1.0 + SurfWinProjCorrDivIn(SurfNum));
                    }

                } else {

                    ConstrNum = Surface(SurfNum).Construction;

                    // These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
                    pulseMultipler = 0.01; // the W/sqft pulse for the zone
                    if (!doLoadComponentPulseNow) {
                        SurfQRadThermInAbs(SurfNum) = QL(radEnclosureNum) * TMULT(radEnclosureNum) * ITABSF(SurfNum);
                    } else {
                        curQL = QL(radEnclosureNum);
                        // for the loads component report during the special sizing run increase the radiant portion
                        // a small amount to create a "pulse" of heat that is used for the
                        adjQL = curQL +
                                DataViewFactorInformation::ZoneRadiantInfo(radEnclosureNum).FloorArea * pulseMultipler;
                        // ITABSF is the Inside Thermal Absorptance
                        // TMULT is a multiplier for each zone/radiant enclosure
                        // QRadThermInAbs is the thermal radiation absorbed on inside surfaces
                        SurfQRadThermInAbs(SurfNum) = adjQL * TMULT(radEnclosureNum) * ITABSF(SurfNum);
                    }
                    // Radiations absorbed by the window layers coming from zone side
                    EQLNum = dataConstruction.Construct(ConstrNum).EQLConsPtr;
                    for (Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {
                        SurfWinQRadSWwinAbs(Lay, SurfNum) +=
                                QS(solEnclosureNum) * dataConstruction.Construct(ConstrNum).AbsDiffBackEQL(Lay);
                    }
                    // Window frame has not been included for equivalent layer model yet

                } // end if for IF ( SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN


                if (Surface(SurfNum).ExtBoundCond > 0) { // Interzone surface

                    if (dataConstruction.Construct(ConstrNum).TransDiff > 0.0) { // Interzone window

                        // Short-wave radiation absorbed in panes of corresponding window in adjacent zone
                        SurfNumAdjZone = Surface(SurfNum).ExtBoundCond;

                        if (SurfWinWindowModelType(SurfNumAdjZone) != WindowEQLModel) {
                            for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                                SurfWinQRadSWwinAbs(IGlass, SurfNumAdjZone) += QS(solEnclosureNum) *
                                                                                dataConstruction.Construct(
                                                                                Surface(SurfNumAdjZone).Construction).AbsDiff(
                                                                                IGlass);
                                // Note that AbsDiff rather than AbsDiffBack is used in the above since the
                                // radiation from the current zone is incident on the outside of the adjacent
                                // zone's window.
                            }
                        } else { // IF (SurfaceWindow(SurfNumAdjZone)%WindowModelType == WindowEQLModel) THEN
                            ConstrNum = Surface(SurfNumAdjZone).Construction;
                            EQLNum = dataConstruction.Construct(ConstrNum).EQLConsPtr;
                            for (Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {
                                SurfWinQRadSWwinAbs(Lay, SurfNumAdjZone) += QS(solEnclosureNum) * dataConstruction.Construct(
                                        ConstrNum).AbsDiffFrontEQL(Lay);
                                // Note that AbsDiffFrontEQL rather than AbsDiffBackEQL is used in the above
                                // since the radiation from the current zone is incident on the outside of the
                                // adjacent zone's window.
                            }
                        }
                    }
                }

                if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel && SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                    ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                    if (SurfWinStormWinFlag(SurfNum) == 1) {
                        ConstrNum = Surface(SurfNum).StormWinConstruction;
                        ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                    }
                    TotGlassLayers = dataConstruction.Construct(ConstrNum).TotGlassLayers;
                    ShadeFlag = SurfWinShadingFlag(SurfNum);
                    if (ShadeFlag <= 0) { // No window shading
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                            SurfWinQRadSWwinAbs(IGlass, SurfNum) += SurfWinInitialDifSolwinAbs(IGlass, SurfNum);
                        }
                    } else if (ShadeFlag == IntShadeOn || ShadeFlag >= 3) {
                        // Interior, exterior or between-glass shade, screen or blind in place
                        for (IGlass = 1; IGlass <= dataConstruction.Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                            SurfWinQRadSWwinAbs(IGlass, SurfNum) += SurfWinInitialDifSolwinAbs(IGlass, SurfNum);
                        }
                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn ||
                            ShadeFlag == ExtScreenOn)
                            SurfWinIntSWAbsByShade(SurfNum) += SurfWinInitialDifSolAbsByShade(SurfNum);
                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                            SurfWinIntSWAbsByShade(SurfNum) += SurfWinInitialDifSolAbsByShade(SurfNum);
                        }
                    } else if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                            SurfWinQRadSWwinAbs(IGlass, SurfNum) += SurfWinInitialDifSolwinAbs(IGlass, SurfNum);
                        }
                    } // End of shading flag check
                } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                    TotGlassLayers = dataConstruction.Construct(ConstrNum).TotGlassLayers;
                    for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        SurfWinQRadSWwinAbs(IGlass, SurfNum) += SurfWinInitialDifSolwinAbs(IGlass, SurfNum);
                    }
                } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {

                    // ConstrNum   = Surface(SurfNum)%Construction
                    EQLNum = dataConstruction.Construct(ConstrNum).EQLConsPtr;

                    for (Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {
                        SurfWinQRadSWwinAbs(Lay, SurfNum) += SurfWinInitialDifSolwinAbs(Lay, SurfNum);
                    }
                }

                // report variables for surface absorbed short wave radiation
                SurfWinSWwinAbsTotalReport(SurfNum) = 0.0;
                SurfSWInAbsTotalReport(SurfNum) = 0.0;
                SurfInitialDifSolInAbsReport(SurfNum) = 0.0;
                SurfWinInitialDifSolInTransReport(SurfNum) = 0.0;
                SurfWinInitialDifSolInTransReport(SurfNum) += SurfWinInitialDifSolInTrans(SurfNum) * Surface(SurfNum).Area;
                if (SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                    ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                    if (SurfWinStormWinFlag(SurfNum) == 1) {
                        ConstrNum = Surface(SurfNum).StormWinConstruction;
                        ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                    }
                    if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                        TotGlassLayers = dataConstruction.Construct(ConstrNum).TotSolidLayers;
                    } else {
                        TotGlassLayers = dataConstruction.Construct(ConstrNum).TotGlassLayers;
                    }
                    ShadeFlag = SurfWinShadingFlag(SurfNum);
                    if (ShadeFlag <= 0 || SurfWinWindowModelType(SurfNum) == WindowBSDFModel) { // No window shading
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                            // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                            SurfInitialDifSolInAbsReport(SurfNum) +=
                                    SurfWinInitialDifSolwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                            SurfSWInAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Absorbed:All Glass Layers[W]
                            SurfWinSWwinAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                        }
                    } else if (ShadeFlag == IntShadeOn || ShadeFlag >= 3) {
                        // Interior, exterior or between-glass shade, screen or blind in place
                        for (IGlass = 1; IGlass <= dataConstruction.Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                            // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                            SurfInitialDifSolInAbsReport(SurfNum) +=
                                    SurfWinInitialDifSolwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                            SurfSWInAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Absorbed:All Glass Layers[W]
                            SurfWinSWwinAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                        }
                    } else if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                        for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                            // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                            SurfInitialDifSolInAbsReport(SurfNum) +=
                                    SurfWinInitialDifSolwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                            SurfSWInAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                            // Total Shortwave Absorbed:All Glass Layers[W]
                            SurfWinSWwinAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(IGlass, SurfNum) * Surface(SurfNum).Area;
                        }
                    }    // End of shading flag check
                } else { // IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
                    ConstrNum = Surface(SurfNum).Construction;
                    EQLNum = dataConstruction.Construct(ConstrNum).EQLConsPtr;
                    for (Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {

                        // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                        SurfInitialDifSolInAbsReport(SurfNum) += SurfWinInitialDifSolwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                        // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                        SurfSWInAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                        // Total Shortwave Absorbed:All solid Layers[W]
                        SurfWinSWwinAbsTotalReport(SurfNum) += SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                    }
                }
            } // End of window
        }
        DistributeTDDAbsorbedSolar();
    }

    void ComputeIntThermalAbsorpFactors()
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

        using namespace HeatBalanceMovableInsulation;
        using General::InterpSlatAng;
        using General::InterpSw;
        using HeatBalanceMovableInsulation::EvalInsideMovableInsulation;

        if (!allocated(ITABSF)) {
            ITABSF.dimension(TotSurfaces, 0.0);
            TMULT.dimension(NumOfZones, 0.0);
        }

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurf = Zone(zoneNum).SurfaceFirst;
            int const lastSurf = Zone(zoneNum).SurfaceLast;
            if (firstSurf <= 0) continue;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {

                if (!Surface(SurfNum).HeatTransSurf) continue;
                int ConstrNum = Surface(SurfNum).Construction;
                ITABSF(SurfNum) = dataConstruction.Construct(ConstrNum).InsideAbsorpThermal;
                Real64 HMovInsul = 0.0; // Conductance of movable insulation
                Real64 AbsInt; // Solar absorptance of movable insulation
                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                    if (Surface(SurfNum).MaterialMovInsulInt > 0)
                        EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
                    if (HMovInsul > 0.0)
                        ITABSF(SurfNum) = dataMaterial.Material(
                                Surface(SurfNum).MaterialMovInsulInt).AbsorpThermal; // Movable inside insulation present
                }
            }
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                // For window with an interior shade or blind, emissivity is a combination of glass and shade/blind emissivity
                int ShadeFlag = SurfWinShadingFlag(SurfNum);
                if (ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn)
                    ITABSF(SurfNum) = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                    SurfaceWindow(SurfNum).EffShBlindEmiss) +
                                      InterpSlatAng(SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum),
                                                    SurfaceWindow(
                                                            SurfNum).EffGlassEmiss); // For shades, following interpolation just returns value of first element in array
            }
        }

        for (int radEnclosureNum = 1; radEnclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++radEnclosureNum) {

            Real64 SUM1 = 0.0;
            auto &thisEnclosure(DataViewFactorInformation::ZoneRadiantInfo(radEnclosureNum));

            for (int const SurfNum : thisEnclosure.SurfacePtr) {

                if (!Surface(SurfNum).HeatTransSurf) continue;
                int ConstrNum = Surface(SurfNum).Construction;
                int ShadeFlag = SurfWinShadingFlag(SurfNum);
                if (ShadeFlag != SwitchableGlazing) {
                    SUM1 += Surface(SurfNum).Area * ITABSF(SurfNum);
                } else { // Switchable glazing
                    SUM1 += Surface(SurfNum).Area * InterpSw(SurfWinSwitchingFactor(SurfNum),
                                                             dataConstruction.Construct(ConstrNum).InsideAbsorpThermal,
                                                             dataConstruction.Construct(Surface(SurfNum).activeShadedConstruction).InsideAbsorpThermal);
                }

                // Window frame and divider effects
                if (SurfWinFrameArea(SurfNum) > 0.0)
                    SUM1 += SurfWinFrameArea(SurfNum) * (1.0 + 0.5 * SurfWinProjCorrFrIn(SurfNum)) * SurfWinFrameEmis(SurfNum);
                if (SurfWinDividerArea(SurfNum) > 0.0) {
                    Real64 DividerThermAbs = SurfWinDividerEmis(SurfNum); // Window divider thermal absorptance
                    // Suspended (between-glass) divider; relevant emissivity is inner glass emissivity
                    if (SurfWinDividerType(SurfNum) == Suspended) DividerThermAbs = dataConstruction.Construct(ConstrNum).InsideAbsorpThermal;
                    if (ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn) {
                        // Interior shade or blind in place
                        int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                        if (SurfWinHasShadeOrBlindLayer(SurfNum)) {
                            // Shade layer material number
                            int MatNumSh = dataConstruction.Construct(ConstrNumSh).LayerPoint(dataConstruction.Construct(ConstrNumSh).TotLayers);
                            // Shade or blind IR transmittance
                            Real64 TauShIR = dataMaterial.Material(MatNumSh).TransThermal;
                            // Effective emissivity of shade or blind
                            Real64 EffShDevEmiss = SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                            if (ShadeFlag == IntBlindOn) {
                                TauShIR = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                        SurfWinMovableSlats(SurfNum),
                                                        Blind(SurfWinBlindNumber(SurfNum)).IRBackTrans);
                                EffShDevEmiss = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                              SurfWinMovableSlats(SurfNum),
                                                              SurfaceWindow(SurfNum).EffShBlindEmiss);
                            }
                            SUM1 += SurfWinDividerArea(SurfNum) * (EffShDevEmiss + DividerThermAbs * TauShIR);
                        } else {
                            // this is for EMS activated shade/blind but the window construction has no shade/blind layer
                            SUM1 += SurfWinDividerArea(SurfNum) * (1.0 + SurfWinProjCorrDivIn(SurfNum)) * DividerThermAbs;
                        }
                    } else {
                        SUM1 += SurfWinDividerArea(SurfNum) * (1.0 + SurfWinProjCorrDivIn(SurfNum)) * DividerThermAbs;
                    }
                }

            } // End of loop over surfaces in zone/enclosure

            TMULT(radEnclosureNum) = 1.0 / SUM1;

        } // End of loop over zones
    }

    void ComputeIntSWAbsorpFactors()
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
        using namespace HeatBalanceMovableInsulation;
        using General::InterpSlatAng;
        using General::InterpSw;
        using namespace DataWindowEquivalentLayer;

        Real64 const SmallestAreaAbsProductAllowed(0.01); // Avoid a division by zero of the user has entered a bunch
        // of surfaces with zero absorptivity on the inside

        int ConstrNum; // DO loop counter for constructions
        Real64 SUM1;   // Intermediate calculation value for solar absorbed and transmitted
        //   by windows (including shade, blind or insulation, if present)
        int ShadeFlag;        // Shading flag
        int ConstrNumSh;      // Shaded construction number
        Real64 SwitchFac;     // Switching factor
        int Lay;              // Layer number
        Real64 AbsDiffLayWin; // Window layer short-wave absorptance
        Real64 AbsDiffTotWin; // Sum of window layer short-wave absorptances
        Real64 TransDiffWin;  // Window diffuse short-wave transmittance
        Real64 DiffAbsShade;  // Diffuse short-wave shade or blind absorptance
        Real64 AbsIntSurf;    // Inside surface short-wave absorptance
        Real64 AbsInt;
        Real64 HMovInsul;  // Conductance of movable insulation
        Real64 DividerAbs; // Window divider solar absorptance
        int MatNumGl;      // Glass material number
        Real64 TransGl;    // Glass layer short-wave transmittance, reflectance, absorptance
        Real64 ReflGl;
        Real64 AbsGl;
        Real64 DividerRefl; // Window divider short-wave reflectance

        static Array1D_bool FirstCalcZone; // for error message

        // FLOW:

        if (!allocated(ZoneVMULT)) {
            ZoneVMULT.dimension(DataViewFactorInformation::NumOfSolarEnclosures, 0.0);
            ZoneVCONV.dimension(DataViewFactorInformation::NumOfSolarEnclosures, 0.0);
        }
        if (ComputeIntSWAbsorpFactorsfirstTime) {
            FirstCalcZone.dimension(NumOfZones, true);
            ComputeIntSWAbsorpFactorsfirstTime = false;
        }

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {

            SUM1 = 0.0;

            for (int const SurfNum : DataViewFactorInformation::ZoneSolarInfo(enclosureNum).SurfacePtr) {

                ConstrNum = Surface(SurfNum).Construction;

                if (dataConstruction.Construct(ConstrNum).TransDiff <= 0.0) {

                    // Opaque surface

                    AbsIntSurf = dataConstruction.Construct(ConstrNum).InsideAbsorpSolar;
                    HMovInsul = 0.0;
                    if (Surface(SurfNum).MaterialMovInsulInt > 0) EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
                    if (HMovInsul > 0.0) AbsIntSurf = AbsInt;
                    SUM1 += Surface(SurfNum).Area * AbsIntSurf;

                } else {

                    // Window
                    if (!dataConstruction.Construct(Surface(SurfNum).Construction).WindowTypeEQL) {
                        ShadeFlag = SurfWinShadingFlag(SurfNum);
                        AbsDiffTotWin = 0.0;
                        ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                        if (SurfWinStormWinFlag(SurfNum) == 1) {
                            ConstrNum = Surface(SurfNum).StormWinConstruction;
                            ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                        }
                        SwitchFac = SurfWinSwitchingFactor(SurfNum);

                        // Sum of absorptances of glass layers
                        for (Lay = 1; Lay <= dataConstruction.Construct(ConstrNum).TotGlassLayers; ++Lay) {
                            AbsDiffLayWin = dataConstruction.Construct(ConstrNum).AbsDiffBack(Lay);

                            // Window with shade, screen or blind
                            if (ConstrNumSh != 0) {
                                if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                                    AbsDiffLayWin = dataConstruction.Construct(ConstrNumSh).AbsDiffBack(Lay);
                                } else if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                    AbsDiffLayWin = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                                  SurfWinMovableSlats(SurfNum),
                                                                  dataConstruction.Construct(ConstrNumSh).BlAbsDiffBack(_, Lay));
                                }
                            }

                            // Switchable glazing
                            if (ShadeFlag == SwitchableGlazing)
                                AbsDiffLayWin = InterpSw(SwitchFac, AbsDiffLayWin, dataConstruction.Construct(ConstrNumSh).AbsDiffBack(Lay));

                            AbsDiffTotWin += AbsDiffLayWin;
                        }

                        TransDiffWin = dataConstruction.Construct(ConstrNum).TransDiff;
                        DiffAbsShade = 0.0;

                        // Window with shade, screen or blind

                        if (ConstrNumSh != 0) {
                            if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                                TransDiffWin = dataConstruction.Construct(ConstrNumSh).TransDiff;
                                DiffAbsShade = dataConstruction.Construct(ConstrNumSh).AbsDiffBackShade;
                            } else if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                TransDiffWin = InterpSlatAng(
                                    SurfWinSlatAngThisTS(SurfNum), SurfWinMovableSlats(SurfNum), dataConstruction.Construct(ConstrNumSh).BlTransDiff);
                                DiffAbsShade = InterpSlatAng(SurfWinSlatAngThisTS(SurfNum),
                                                             SurfWinMovableSlats(SurfNum),

                                                             dataConstruction.Construct(ConstrNumSh).AbsDiffBackBlind);
                            }
                        }

                        // Switchable glazing

                        if (ShadeFlag == SwitchableGlazing)
                            TransDiffWin = InterpSw(SwitchFac, TransDiffWin, dataConstruction.Construct(ConstrNumSh).TransDiff);

                        SUM1 += Surface(SurfNum).Area * (TransDiffWin + AbsDiffTotWin + DiffAbsShade);

                        // Window frame and divider effects (shade area is glazed area plus divider area)

                        if (SurfWinFrameArea(SurfNum) > 0.0)
                            SUM1 += SurfWinFrameArea(SurfNum) * SurfWinFrameSolAbsorp(SurfNum) *
                                    (1.0 + 0.5 * SurfWinProjCorrFrIn(SurfNum));
                        if (SurfWinDividerArea(SurfNum) > 0.0) {
                            DividerAbs = SurfWinDividerSolAbsorp(SurfNum);
                            if (SurfWinDividerType(SurfNum) == Suspended) {
                                // Suspended (between-glass) divider: account for glass on inside of divider
                                MatNumGl = dataConstruction.Construct(ConstrNum).LayerPoint(dataConstruction.Construct(ConstrNum).TotLayers);
                                TransGl = dataMaterial.Material(MatNumGl).Trans;
                                ReflGl = dataMaterial.Material(MatNumGl).ReflectSolBeamBack;
                                AbsGl = 1.0 - TransGl - ReflGl;
                                DividerRefl = 1.0 - DividerAbs;
                                DividerAbs = AbsGl + TransGl * (DividerAbs + DividerRefl * AbsGl) / (1.0 - DividerRefl * ReflGl);
                            }
                            if (ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn) {
                                SUM1 += SurfWinDividerArea(SurfNum) * (DividerAbs + DiffAbsShade);
                            } else {
                                SUM1 += SurfWinDividerArea(SurfNum) * (1.0 + SurfWinProjCorrDivIn(SurfNum)) * DividerAbs;
                            }
                        }
                    } else { // equivalent layer window
                        // In equivalent layer window solid layers (Glazing and shades) are treated equally
                        // frames and dividers are not supported
                        AbsDiffTotWin = 0.0;
                        AbsDiffLayWin = 0.0;
                        TransDiffWin = dataConstruction.Construct(ConstrNum).TransDiff;
                        for (Lay = 1; Lay <= CFS(dataConstruction.Construct(ConstrNum).EQLConsPtr).NL; ++Lay) {
                            AbsDiffLayWin = dataConstruction.Construct(ConstrNum).AbsDiffBackEQL(Lay);
                            AbsDiffTotWin += AbsDiffLayWin;
                        }
                        SUM1 += Surface(SurfNum).Area * (TransDiffWin + AbsDiffTotWin);
                    }
                } // End of check if opaque surface or window
            }     // End of loop over surfaces in zone

            if (SUM1 > SmallestAreaAbsProductAllowed) { // Everything is okay, proceed with the regular calculation
                ZoneVMULT(enclosureNum) = 1.0 / SUM1;

            } else { // the sum of area*solar absorptance for all surfaces in the zone is zero--either the user screwed up
                // or they really want to disallow any solar from being absorbed on the inside surfaces.  Fire off a
                // nasty warning message and then assume that no solar is ever absorbed (basically everything goes
                // back out whatever window is there.  Note that this also assumes that the shade has no effect.
                // That's probably not correct, but how correct is it to assume that no solar is absorbed anywhere
                // in the zone?
                if (FirstCalcZone(enclosureNum)) {
                    ShowWarningError("ComputeIntSWAbsorbFactors: Sum of area times inside solar absorption for all surfaces is zero in Zone: " +
                                     DataViewFactorInformation::ZoneSolarInfo(enclosureNum).Name);
                    FirstCalcZone(enclosureNum) = false;
                }
                ZoneVMULT(enclosureNum) = 0.0;
            }
        } // End of zone/enclosure loop
    }

    void ComputeDifSolExcZonesWIZWindows(int const NumberOfEnclosures) // Number of solar enclosures
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       Jun 2007 - Lawrie - Speed enhancements.
        //       RE-ENGINEERED  Winkelmann, Lawrie

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes the diffuse solar exchange factors between zones with
        // interzone windows.

        static Array2D<Real64> D;

        if (!allocated(FractDifShortZtoZ)) {
            FractDifShortZtoZ.allocate(NumberOfEnclosures, NumberOfEnclosures);
            RecDifShortFromZ.allocate(NumberOfEnclosures);
            D.allocate(NumberOfEnclosures, NumberOfEnclosures);
        }

        RecDifShortFromZ = false;
        FractDifShortZtoZ.to_identity();
        D.to_identity();

        //      IF (.not. ANY(Zone%HasInterZoneWindow)) RETURN  ! this caused massive diffs
        if (KickOffSimulation || KickOffSizing) return;
        //            Compute fraction transmitted in one pass.

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue;
            if (Surface(SurfNum).ExtBoundCond <= 0) continue;
            if (Surface(SurfNum).ExtBoundCond == SurfNum) continue;
            if (dataConstruction.Construct(Surface(SurfNum).Construction).TransDiff <= 0.0) continue;

            int surfZoneNum = Surface(SurfNum).Zone;
            if (!Zone(surfZoneNum).HasInterZoneWindow) continue;
            int NZ = Surface(SurfNum).SolarEnclIndex;
            int MZ = Surface(Surface(SurfNum).ExtBoundCond).SolarEnclIndex;
            FractDifShortZtoZ(NZ, MZ) += dataConstruction.Construct(Surface(SurfNum).Construction).TransDiff * ZoneVMULT(NZ) * Surface(SurfNum).Area;
            if (ZoneVMULT(NZ) != 0.0) RecDifShortFromZ(NZ) = true;
        }
        //          Compute fractions for multiple passes.

        Array2D<Real64>::size_type l(0u), m(0u), d(0u);
        for (int NZ = 1; NZ <= NumberOfEnclosures; ++NZ, d += NumberOfEnclosures + 1) {
            m = NZ - 1;
            Real64 D_d(0.0); // Local accumulator
            for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ, ++l, m += NumberOfEnclosures) {
                if (MZ == NZ) continue;
                D[l] = FractDifShortZtoZ[l] / (1.0 - FractDifShortZtoZ[l] * FractDifShortZtoZ[m]); // [ l ] == ( MZ, NZ ), [ m ] == ( NZ, MZ )
                D_d += FractDifShortZtoZ[m] * D[l];
            }
            D[d] += D_d; // [ d ] == ( NZ, NZ )
        }

        FractDifShortZtoZ = D;
        // added for CR 7999 & 7869
        assert(FractDifShortZtoZ.isize1() == NumberOfEnclosures);
        assert(FractDifShortZtoZ.isize2() == NumberOfEnclosures);
        l = 0u;
        for (int NZ = 1; NZ <= NumberOfEnclosures; ++NZ) {
            for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ, ++l) {
                if (MZ == NZ) continue;
                if (FractDifShortZtoZ[l] > 0.0) { // [ l ] == ( MZ, NZ )
                    RecDifShortFromZ(NZ) = true;
                    break;
                }
            }
        }

        //           Compute fractions for multiple zones.

        for (int IZ = 1; IZ <= NumberOfEnclosures; ++IZ) {
            if (!RecDifShortFromZ(IZ)) continue;

            for (int JZ = 1; JZ <= NumberOfEnclosures; ++JZ) {
                if (!RecDifShortFromZ(JZ)) continue;
                if (IZ == JZ) continue;
                if (D(IZ, JZ) == 0.0) continue;

                for (int KZ = 1; KZ <= NumberOfEnclosures; ++KZ) {
                    if (!RecDifShortFromZ(KZ)) continue;
                    if (IZ == KZ) continue;
                    if (JZ == KZ) continue;
                    if (D(JZ, KZ) == 0.0) continue;
                    FractDifShortZtoZ(IZ, KZ) += D(JZ, KZ) * D(IZ, JZ);

                    for (int LZ = 1; LZ <= NumberOfEnclosures; ++LZ) {
                        if (!RecDifShortFromZ(LZ)) continue;
                        if (IZ == LZ) continue;
                        if (JZ == LZ) continue;
                        if (KZ == LZ) continue;
                        if (D(KZ, LZ) == 0.0) continue;
                        FractDifShortZtoZ(IZ, LZ) += D(KZ, LZ) * D(JZ, KZ) * D(IZ, JZ);

                        for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ) {
                            if (!RecDifShortFromZ(MZ)) continue;
                            if (IZ == MZ) continue;
                            if (JZ == MZ) continue;
                            if (KZ == MZ) continue;
                            if (LZ == MZ) continue;
                            if (D(LZ, MZ) == 0.0) continue;
                            FractDifShortZtoZ(IZ, MZ) += D(LZ, MZ) * D(KZ, LZ) * D(JZ, KZ) * D(IZ, JZ);
                        } // MZ Loop

                    } // LZ Loop

                } // KZ Loop

            } // JZ Loop

        } // IZ Loop
    }

    void InitEMSControlledSurfaceProperties()
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
        bool SurfPropOverridesPresent(false); // detect if EMS ever used for this and inits need to execute
        int MaterNum;                                // do loop counter over materials
        int ConstrNum;                               // do loop counter over constructions
        int TotLayers;                               // count of material layers in a construction
        int InsideMaterNum;                          // integer pointer for inside face's material layer
        int OutsideMaterNum;                         // integer pointer for outside face's material layer

        // first determine if anything needs to be done, once yes, then always init
        for (auto const &mat : dataMaterial.Material) {
            if ((mat.AbsorpSolarEMSOverrideOn) || (mat.AbsorpThermalEMSOverrideOn) || (mat.AbsorpVisibleEMSOverrideOn)) {
                SurfPropOverridesPresent = true;
                break;
            }
        }

        if (!SurfPropOverridesPresent) return; // quick return if nothing has ever needed to be done

        // first, loop over materials
        for (MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum) {
            if (dataMaterial.Material(MaterNum).AbsorpSolarEMSOverrideOn) {
                dataMaterial.Material(MaterNum).AbsorpSolar = max(min(dataMaterial.Material(MaterNum).AbsorpSolarEMSOverride, 0.9999), 0.0001);
            } else {
                dataMaterial.Material(MaterNum).AbsorpSolar = dataMaterial.Material(MaterNum).AbsorpSolarInput;
            }
            if (dataMaterial.Material(MaterNum).AbsorpThermalEMSOverrideOn) {
                dataMaterial.Material(MaterNum).AbsorpThermal = max(min(dataMaterial.Material(MaterNum).AbsorpThermalEMSOverride, 0.9999), 0.0001);
            } else {
                dataMaterial.Material(MaterNum).AbsorpThermal = dataMaterial.Material(MaterNum).AbsorpThermalInput;
            }
            if (dataMaterial.Material(MaterNum).AbsorpVisibleEMSOverrideOn) {
                dataMaterial.Material(MaterNum).AbsorpVisible = max(min(dataMaterial.Material(MaterNum).AbsorpVisibleEMSOverride, 0.9999), 0.0001);
            } else {
                dataMaterial.Material(MaterNum).AbsorpVisible = dataMaterial.Material(MaterNum).AbsorpVisibleInput;
            }
        } // loop over materials

        // second, loop over constructions
        for (ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum) {
            if (dataConstruction.Construct(ConstrNum).TypeIsWindow) continue; // only override opaque constructions
            TotLayers = dataConstruction.Construct(ConstrNum).TotLayers;
            if (TotLayers == 0) continue; // error condition
            InsideMaterNum = dataConstruction.Construct(ConstrNum).LayerPoint(TotLayers);
            if (InsideMaterNum != 0) {
                dataConstruction.Construct(ConstrNum).InsideAbsorpVis = dataMaterial.Material(InsideMaterNum).AbsorpVisible;
                dataConstruction.Construct(ConstrNum).InsideAbsorpSolar = dataMaterial.Material(InsideMaterNum).AbsorpSolar;
                dataConstruction.Construct(ConstrNum).InsideAbsorpThermal = dataMaterial.Material(InsideMaterNum).AbsorpThermal;
            }

            OutsideMaterNum = dataConstruction.Construct(ConstrNum).LayerPoint(1);
            if (OutsideMaterNum != 0) {
                dataConstruction.Construct(ConstrNum).OutsideAbsorpVis = dataMaterial.Material(OutsideMaterNum).AbsorpVisible;
                dataConstruction.Construct(ConstrNum).OutsideAbsorpSolar = dataMaterial.Material(OutsideMaterNum).AbsorpSolar;
                dataConstruction.Construct(ConstrNum).OutsideAbsorpThermal = dataMaterial.Material(OutsideMaterNum).AbsorpThermal;
            }
        }
    }

    void InitEMSControlledConstructions()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // change construction on surface if overriden by EMS

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataRuntimeLanguage::EMSConstructActuatorChecked;
        using DataRuntimeLanguage::EMSConstructActuatorIsOkay;
        using General::TrimSigDigits;
        using HeatBalFiniteDiffManager::ConstructFD;

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
        bool SurfConstructOverridesPresent(false); // detect if EMS ever used for this and inits need to execute

        if (std::any_of(Surface.begin(), Surface.end(), [](DataSurfaces::SurfaceData const &e) { return e.EMSConstructionOverrideON; }))
            SurfConstructOverridesPresent = true;

        if (!SurfConstructOverridesPresent) return;

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            if (Surface(SurfNum).EMSConstructionOverrideON && (Surface(SurfNum).EMSConstructionOverrideValue > 0)) {

                if (dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).TypeIsWindow) { // okay, allways allow windows
                    EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                    EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                }

                if ((EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum)) &&
                    (EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum))) {

                    Surface(SurfNum).Construction = Surface(SurfNum).EMSConstructionOverrideValue;
                    dataConstruction.Construct(Surface(SurfNum).Construction).IsUsed = true;

                } else { // have not checked yet or is not okay, so see if we need to warn about incompatible
                    if (!EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum)) {
                        // check if constructions appear compatible

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                            // compare old construction to new construction and see if terms match
                            // set as okay and turn false if find a big problem
                            EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            if (dataConstruction.Construct(Surface(SurfNum).Construction).NumHistories !=
                                dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).NumHistories) {
                                // thow warning, but allow
                                ShowWarningError("InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible "
                                                 "CTF timescales are being used.");
                                ShowContinueError(
                                    "Construction named = " + dataConstruction.Construct(Surface(SurfNum).Construction).Name +
                                    " has CTF timesteps = " + TrimSigDigits(dataConstruction.Construct(Surface(SurfNum).Construction).NumHistories));
                                ShowContinueError(
                                    "While construction named = " + dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).Name +
                                    " has CTF timesteps = " +
                                    TrimSigDigits(dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).NumHistories));
                                ShowContinueError("Transient heat transfer modeling may not be valid for surface name = " + Surface(SurfNum).Name +
                                                  ", and the simulation continues");
                            }
                            if (dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms !=
                                dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).NumCTFTerms) {
                                // thow warning, but allow
                                ShowWarningError("InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible "
                                                 "CTF terms are being used.");
                                ShowContinueError("Construction named = " + dataConstruction.Construct(Surface(SurfNum).Construction).Name +
                                                  " has number of CTF terms = " +
                                                  TrimSigDigits(dataConstruction.Construct(Surface(SurfNum).Construction).NumCTFTerms));
                                ShowContinueError(
                                    "While construction named = " + dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).Name +
                                    " has number of CTF terms = " +
                                    TrimSigDigits(dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).NumCTFTerms));
                                ShowContinueError(
                                    "The actuator is allowed but the transient heat transfer modeling may not be valid for surface name = " +
                                    Surface(SurfNum).Name + ", and the simulation continues");
                            }

                            if (dataConstruction.Construct(Surface(SurfNum).Construction).SourceSinkPresent) {
                                if (!dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).SourceSinkPresent) {
                                    // thow warning, and do not allow
                                    ShowSevereError("InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                                    ShowContinueError("Construction named = " + dataConstruction.Construct(Surface(SurfNum).Construction).Name +
                                                      " has internal source/sink");
                                    ShowContinueError("While construction named = " +
                                                      dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).Name +
                                                      " is not an internal source/sink construction");
                                    ShowContinueError("This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                      ", and the simulation continues without the override");

                                    EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = false;
                                }
                            }

                            if (EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum)) {
                                Surface(SurfNum).Construction = Surface(SurfNum).EMSConstructionOverrideValue;
                            }

                        } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                            EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            if (ConstructFD(Surface(SurfNum).Construction).TotNodes !=
                                ConstructFD(Surface(SurfNum).EMSConstructionOverrideValue).TotNodes) {
                                // thow warning, and do not allow
                                ShowSevereError("InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                                ShowContinueError(
                                    "Construction named = " + dataConstruction.Construct(Surface(SurfNum).Construction).Name +
                                    " has number of finite difference nodes =" + TrimSigDigits(ConstructFD(Surface(SurfNum).Construction).TotNodes));
                                ShowContinueError(
                                    "While construction named = " + dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).Name +
                                    "has number of finite difference nodes =" +
                                    TrimSigDigits(ConstructFD(Surface(SurfNum).EMSConstructionOverrideValue).TotNodes));
                                ShowContinueError("This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                  ", and the simulation continues without the override");

                                EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = false;
                            }

                            if (dataConstruction.Construct(Surface(SurfNum).Construction).SourceSinkPresent) {
                                if (!dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).SourceSinkPresent) {
                                    // thow warning, and do not allow
                                    ShowSevereError("InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                                    ShowContinueError("Construction named = " + dataConstruction.Construct(Surface(SurfNum).Construction).Name +
                                                      " has internal source/sink");
                                    ShowContinueError("While construction named = " +
                                                      dataConstruction.Construct(Surface(SurfNum).EMSConstructionOverrideValue).Name +
                                                      " is not an internal source/sink construction");
                                    ShowContinueError("This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                      ", and the simulation continues without the override");

                                    EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = false;
                                }
                            }

                            if (EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum)) {
                                Surface(SurfNum).Construction = Surface(SurfNum).EMSConstructionOverrideValue;
                            }

                        } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) { // don't allow
                            ShowSevereError("InitEMSControlledConstructions: EMS Construction State Actuator not available with Heat transfer "
                                            "algorithm CombinedHeatAndMoistureFiniteElement.");
                            ShowContinueError("This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                              ", and the simulation continues without the override");
                            EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = false;

                        } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_Kiva) { // don't allow
                            ShowSevereError("InitEMSControlledConstructions: EMS Construction State Actuator not available for Surfaces with "
                                            "Foundation Outside Boundary Condition.");
                            ShowContinueError("This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                              ", and the simulation continues without the override");
                            EMSConstructActuatorChecked(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = true;
                            EMSConstructActuatorIsOkay(Surface(SurfNum).EMSConstructionOverrideValue, SurfNum) = false;
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using CoolingPanelSimple::UpdateCoolingPanelSourceValAvg;
        using ElectricBaseboardRadiator::UpdateBBElecRadSourceValAvg;
        using HighTempRadiantSystem::UpdateHTRadSourceValAvg;
        using HWBaseboardRadiator::UpdateBBRadSourceValAvg;
        using LowTempRadiantSystem::UpdateRadSysSourceValAvg;
        using SteamBaseboardRadiator::UpdateBBSteamRadSourceValAvg;
        using SwimmingPool::UpdatePoolSourceValAvg;

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
        bool LowTempRadSysOn;     // .TRUE. if a low temperature radiant system is running
        bool HighTempRadSysOn;    // .TRUE. if a high temperature radiant system is running
        bool HWBaseboardSysOn;    // .TRUE. if a water baseboard heater is running
        bool SteamBaseboardSysOn; // .TRUE. if a steam baseboard heater is running
        bool ElecBaseboardSysOn;  // .TRUE. if a steam baseboard heater is running
        bool CoolingPanelSysOn;   // true if a simple cooling panel is running
        bool SwimmingPoolOn;      // true if a pool is present (running)

        // FLOW:
        UpdateRadSysSourceValAvg(LowTempRadSysOn);
        UpdateHTRadSourceValAvg(HighTempRadSysOn);
        UpdateBBRadSourceValAvg(HWBaseboardSysOn);
        UpdateBBSteamRadSourceValAvg(SteamBaseboardSysOn);
        UpdateBBElecRadSourceValAvg(ElecBaseboardSysOn);
        UpdateCoolingPanelSourceValAvg(state, CoolingPanelSysOn);
        UpdatePoolSourceValAvg(SwimmingPoolOn);

        if (LowTempRadSysOn || HighTempRadSysOn || HWBaseboardSysOn || SteamBaseboardSysOn || ElecBaseboardSysOn || CoolingPanelSysOn ||
            SwimmingPoolOn) {
            // Solve the zone heat balance 'Detailed' solution
            // Call the outside and inside surface heat balances
            CalcHeatBalanceOutsideSurf(state);
            CalcHeatBalanceInsideSurf(state);
        }
    }

    void UpdateThermalHistories()
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

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HistTermNum; // DO loop counter for history terms
        int SideNum;     // DO loop counter for surfaces sides (inside, outside)
        int SurfNum;     // Surface number DO loop counter

        static Array1D<Real64> QExt1;    // Heat flux at the exterior surface during first time step/series
        static Array1D<Real64> QInt1;    // Heat flux at the interior surface during first time step/series
        static Array1D<Real64> TempInt1; // Temperature of interior surface during first time step/series
        static Array1D<Real64> TempExt1; // Temperature of exterior surface during first time step/series
        static Array1D<Real64> Qsrc1;    // Heat source/sink (during first time step/series)
        static Array1D<Real64> Tsrc1;    // Temperature at source/sink (during first time step/series)
        static Array1D<Real64> Tuser1;   // Temperature at the user specified location (during first time step/series)
        static Array1D<Real64> SumTime;  // Amount of time that has elapsed from start of master history to
        // the current time step

        // FLOW:

        // Tuned Assure safe to use shared linear indexing below
        assert(equal_dimensions(TH, THM));
        assert(equal_dimensions(TH, QH));
        assert(equal_dimensions(TH, QHM));
        if (AnyConstructInternalSourceInInput) {
            assert(equal_dimensions(TsrcHist, QsrcHist));
            assert(equal_dimensions(TsrcHist, TsrcHistM));
            assert(equal_dimensions(TsrcHistM, QsrcHistM));
            assert(equal_dimensions(TuserHist, QsrcHist));
            assert(equal_dimensions(TuserHist, TuserHistM));
            assert(equal_dimensions(TuserHistM, QsrcHistM));
        }

        if (UpdateThermalHistoriesFirstTimeFlag) {
            QExt1.dimension(TotSurfaces, 0.0);
            QInt1.dimension(TotSurfaces, 0.0);
            TempInt1.dimension(TotSurfaces, 0.0);
            TempExt1.dimension(TotSurfaces, 0.0);
            SumTime.dimension(TotSurfaces, 0.0);
            if (AnyConstructInternalSourceInInput) {
                Qsrc1.dimension(TotSurfaces, 0.0);
                Tsrc1.dimension(TotSurfaces, 0.0);
                Tuser1.dimension(TotSurfaces, 0.0);
            }
            UpdateThermalHistoriesFirstTimeFlag = false;
        }

        auto const l111(TH.index(1, 1, 1));
        auto const l211(TH.index(2, 1, 1));
        auto l11(l111);
        auto l21(l211);
        for (SurfNum = 1; SurfNum <= TotSurfaces;
             ++SurfNum, ++l11, ++l21) { // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
            auto const &surface(Surface(SurfNum));

            if (surface.Class == SurfaceClass_Window || !surface.HeatTransSurf) continue;

            if ((surface.HeatTransferAlgorithm != HeatTransferModel_CTF) && (surface.HeatTransferAlgorithm != HeatTransferModel_EMPD)) continue;

            int const ConstrNum(surface.Construction);
            auto const &construct(dataConstruction.Construct(ConstrNum));

            if (construct.NumCTFTerms == 0) continue; // Skip surfaces with no history terms

            // Sign convention for the various terms in the following two equations
            // is based on the form of the Conduction Transfer Function equation
            // given by:
            // Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old) + (Sum of)(V Qsrc)
            // Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old) + (Sum of)(W Qsrc)
            // In both equations, flux is positive from outside to inside.  The V and W terms are for radiant systems only.

            // Set current inside flux:
            Real64 const QH_12 = QH[l21] = TH[l11] * construct.CTFCross(0) - TempSurfIn(SurfNum) * construct.CTFInside(0) +
                                           CTFConstInPart(SurfNum); // Heat source/sink term for radiant systems
            if (surface.Class == SurfaceClass_Floor || surface.Class == SurfaceClass_Wall || surface.Class == SurfaceClass_IntMass ||
                surface.Class == SurfaceClass_Roof || surface.Class == SurfaceClass_Door) {
                if (construct.SourceSinkPresent) {
                    Real64 const QH_12s = QH[l21] = QH_12 + QsrcHist(SurfNum, 1) * construct.CTFSourceIn(0);
                    SurfOpaqInsFaceConduction(SurfNum) = surface.Area * QH_12s;
                    SurfOpaqInsFaceConductionFlux(SurfNum) = QH_12s;
                } else {
                    SurfOpaqInsFaceConduction(SurfNum) = surface.Area * QH_12;
                    SurfOpaqInsFaceConductionFlux(SurfNum) = QH_12; // CR 8901
                }
                //      IF (Surface(SurfNum)%Class/=SurfaceClass_IntMass)  &
                //      ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) + &
                //              OpaqSurfInsFaceConduction(SurfNum)
                SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
                SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
                if (SurfOpaqInsFaceConduction(SurfNum) >= 0.0) {
                    SurfOpaqInsFaceCondGainRep(SurfNum) = SurfOpaqInsFaceConduction(SurfNum);
                } else {
                    SurfOpaqInsFaceCondLossRep(SurfNum) = - SurfOpaqInsFaceConduction(SurfNum);
                }
            }

            // Update the temperature at the source/sink location (if one is present)
            if (construct.SourceSinkPresent) {
                TempSource(SurfNum) = TsrcHist(SurfNum, 1) = TH[l11] * construct.CTFTSourceOut(0) + TempSurfIn(SurfNum) * construct.CTFTSourceIn(0) +
                                                             QsrcHist(SurfNum, 1) * construct.CTFTSourceQ(0) + CTFTsrcConstPart(SurfNum);
                TempUserLoc(SurfNum) = TuserHist(SurfNum, 1) = TH[l11] * construct.CTFTUserOut(0) + TempSurfIn(SurfNum) * construct.CTFTUserIn(0) +
                                                               QsrcHist(SurfNum, 1) * construct.CTFTUserSource(0) + CTFTuserConstPart(SurfNum);
            }

            if (surface.ExtBoundCond > 0) continue; // Don't need to evaluate outside for partitions

            // Set current outside flux:
            if (construct.SourceSinkPresent) {
                QH[l11] = TH[l11] * construct.CTFOutside(0) - TempSurfIn(SurfNum) * construct.CTFCross(0) +
                          QsrcHist(SurfNum, 1) * construct.CTFSourceOut(0) + CTFConstOutPart(SurfNum); // Heat source/sink term for radiant systems
            } else {
                QH[l11] = TH[l11] * construct.CTFOutside(0) - TempSurfIn(SurfNum) * construct.CTFCross(0) + CTFConstOutPart(SurfNum);
            }
            if (surface.Class == SurfaceClass_Floor || surface.Class == SurfaceClass_Wall || surface.Class == SurfaceClass_IntMass ||
                surface.Class == SurfaceClass_Roof || surface.Class == SurfaceClass_Door) {
                SurfOpaqOutsideFaceConductionFlux(SurfNum) = -QH[l11]; // switch sign for balance at outside face
                SurfOpaqOutsideFaceConduction(SurfNum) = surface.Area * SurfOpaqOutsideFaceConductionFlux(SurfNum);
            }

        } // ...end of loop over all (heat transfer) surfaces...

        l11 = l111;
        l21 = l211;
        for (SurfNum = 1; SurfNum <= TotSurfaces;
             ++SurfNum, ++l11, ++l21) { // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
            auto const &surface(Surface(SurfNum));

            if (surface.Class == SurfaceClass_Window || !surface.HeatTransSurf) continue;
            if ((surface.HeatTransferAlgorithm != HeatTransferModel_CTF) && (surface.HeatTransferAlgorithm != HeatTransferModel_EMPD) &&
                (surface.HeatTransferAlgorithm != HeatTransferModel_TDD))
                continue;
            if (SUMH(SurfNum) == 0) { // First time step in a block for a surface, update arrays
                TempExt1(SurfNum) = TH[l11];
                TempInt1(SurfNum) = TempSurfIn(SurfNum);
                QExt1(SurfNum) = QH[l11];
                QInt1(SurfNum) = QH[l21];
                if (AnyConstructInternalSourceInInput) {
                    Tsrc1(SurfNum) = TsrcHist(SurfNum, 1);
                    Tuser1(SurfNum) = TuserHist(SurfNum, 1);
                    Qsrc1(SurfNum) = QsrcHist(SurfNum, 1);
                }
            }

        } // ...end of loop over all (heat transfer) surfaces...

        // SHIFT TEMPERATURE AND FLUX HISTORIES:
        // SHIFT AIR TEMP AND FLUX SHIFT VALUES WHEN AT BOTTOM OF ARRAY SPACE.
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) { // Loop through all (heat transfer) surfaces...
            auto const &surface(Surface(SurfNum));

            if (surface.Class == SurfaceClass_Window || surface.Class == SurfaceClass_TDD_Dome || !surface.HeatTransSurf) continue;
            if ((surface.HeatTransferAlgorithm != HeatTransferModel_CTF) && (surface.HeatTransferAlgorithm != HeatTransferModel_EMPD) &&
                (surface.HeatTransferAlgorithm != HeatTransferModel_TDD))
                continue;

            int const ConstrNum(surface.Construction);
            auto const &construct(dataConstruction.Construct(ConstrNum));

            ++SUMH(SurfNum);
            SumTime(SurfNum) = double(SUMH(SurfNum)) * TimeStepZone;

            if (SUMH(SurfNum) == construct.NumHistories) {

                SUMH(SurfNum) = 0;

                if (construct.NumCTFTerms > 1) {
                    int const numCTFTerms(construct.NumCTFTerms);
                    for (SideNum = 1; SideNum <= 2; ++SideNum) { // Tuned Index order switched for cache friendliness
                        auto l(THM.index(SideNum, numCTFTerms, SurfNum));
                        auto const li(THM.size3());
                        auto l1(l + li);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= li) { // Tuned Linear indexing
                            // TH( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum - 1, SurfNum );
                            // QH( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum - 1, SurfNum );
                            TH[l1] = THM[l1] = THM[l];
                            QH[l1] = QHM[l1] = QHM[l];
                        }
                    }
                    if (construct.SourceSinkPresent) {
                        auto m(TsrcHistM.index(SurfNum, numCTFTerms));
                        auto m1(m + 1);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1) { // Tuned Linear indexing
                            // TsrcHist( SurfNum, HistTerm ) = TsrcHistM( SurfNum, HHistTerm ) = TsrcHistM( SurfNum, HistTermNum - 1 );
                            // QsrcHist( SurfNum, HistTerm ) = QsrcHistM( SurfNum, HHistTerm ) = QsrcHistM( SurfNum, HistTermNum - 1 );
                            TsrcHist[m1] = TsrcHistM[m1] = TsrcHistM[m];
                            QsrcHist[m1] = QsrcHistM[m1] = QsrcHistM[m];
                            TuserHist[m1] = TuserHistM[m1] = TuserHistM[m];
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

                auto const l21(TH.index(1, 2, SurfNum)); // Linear index
                auto const l22(TH.index(2, 2, SurfNum)); // Linear index
                THM[l21] = TempExt1(SurfNum);
                THM[l22] = TempInt1(SurfNum);
                QHM[l21] = QExt1(SurfNum);
                QHM[l22] = QInt1(SurfNum);

                TH[l21] = THM[l21];
                TH[l22] = THM(2, 2, SurfNum);
                QH[l21] = QHM[l21];
                QH[l22] = QHM(2, 2, SurfNum);

                if (construct.SourceSinkPresent) {
                    TsrcHistM(SurfNum, 2) = Tsrc1(SurfNum);
                    TuserHistM(SurfNum, 2) = Tuser1(SurfNum);
                    QsrcHistM(SurfNum, 2) = Qsrc1(SurfNum);
                    TsrcHist(SurfNum, 2) = TsrcHistM(SurfNum, 2);
                    TuserHist(SurfNum, 2) = TuserHistM(SurfNum, 2);
                    QsrcHist(SurfNum, 2) = QsrcHistM(SurfNum, 2);
                }

            } else {

                Real64 const sum_steps(SumTime(SurfNum) / construct.CTFTimeStep);
                if (construct.NumCTFTerms > 1) {
                    int const numCTFTerms(construct.NumCTFTerms);
                    for (SideNum = 1; SideNum <= 2; ++SideNum) { // Tuned Index order switched for cache friendliness
                        auto l(THM.index(SideNum, numCTFTerms, SurfNum));
                        auto const s3(THM.size3());
                        auto l1(l + s3);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= s3) { // Tuned Linear indexing
                            // Real64 const THM_l1( THM( SideNum, HistTermNum, SurfNum ) );
                            // TH( SideNum, HistTermNum, SurfNum ) = THM_l1 - ( THM_l1 - THM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
                            // Real64 const QHM_l1( QHM( SideNum, HistTermNum, SurfNum ) );
                            // QH( SideNum, HistTermNum, SurfNum ) = QHM_l1 - ( QHM_l1 - QHM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
                            Real64 const THM_l1(THM[l1]);
                            TH[l1] = THM_l1 - (THM_l1 - THM[l]) * sum_steps;
                            Real64 const QHM_l1(QHM[l1]);
                            QH[l1] = QHM_l1 - (QHM_l1 - QHM[l]) * sum_steps;
                        }
                    }
                    if (construct.SourceSinkPresent) {
                        auto m(TsrcHistM.index(SurfNum, numCTFTerms));
                        auto m1(m + 1);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1) { // Tuned Linear indexing [ l ] == ()
                            // Real64 const TsrcHistM_elem( TsrcHistM( SurfNum, HistTermNum ) );
                            // TsrcHist( SurfNum, HistTermNum ) = TsrcHistM_elem - ( TsrcHistM_elem - TsrcHistM( SurfNum, HistTermNum - 1 ) ) *
                            // sum_steps;  Real64 const QsrcHistM_elem( QsrcHistM( SurfNum, HistTermNum ) );  QsrcHist( SurfNum, HistTermNum ) =
                            // QsrcHistM_elem - ( QsrcHistM_elem - QsrcHistM( SurfNum, HistTermNum - 1 ) ) * sum_steps;
                            Real64 const TsrcHistM_m1(TsrcHistM[m1]);
                            TsrcHist[m1] = TsrcHistM_m1 - (TsrcHistM_m1 - TsrcHistM[m]) * sum_steps;
                            Real64 const QsrcHistM_m1(QsrcHistM[m1]);
                            QsrcHist[m1] = QsrcHistM_m1 - (QsrcHistM_m1 - QsrcHistM[m]) * sum_steps;
                            Real64 const TuserHistM_m1(TuserHistM[m1]);
                            TuserHist[m1] = TuserHistM_m1 - (TuserHistM_m1 - TuserHistM[m]) * sum_steps;
                        }
                    }
                }

                // Tuned Linear indexing
                // TH( 1, 2, SurfNum ) = THM( 1, 2, SurfNum ) - ( THM( 1, 2, SurfNum ) - TempExt1( SurfNum ) ) * sum_steps;
                // TH( 2, 2, SurfNum ) = THM( 2, 2, SurfNum ) - ( THM( 2, 2, SurfNum ) - TempInt1( SurfNum ) ) * sum_steps;
                // QH( 1, 2, SurfNum ) = QHM( 1, 2, SurfNum ) - ( QHM( 1, 2, SurfNum ) - QExt1( SurfNum ) ) * sum_steps;
                // QH( 2, 2, SurfNum ) = QHM( 2, 2, SurfNum ) - ( QHM( 2, 2, SurfNum ) - QInt1( SurfNum ) ) * sum_steps;

                auto const l21(TH.index(1, 2, SurfNum)); // Linear index
                auto const l22(TH.index(2, 2, SurfNum)); // Linear index
                TH[l21] = THM[l21] - (THM[l21] - TempExt1(SurfNum)) * sum_steps;
                TH[l22] = THM[l22] - (THM[l22] - TempInt1(SurfNum)) * sum_steps;
                QH[l21] = QHM[l21] - (QHM[l21] - QExt1(SurfNum)) * sum_steps;
                QH[l22] = QHM[l22] - (QHM[l22] - QInt1(SurfNum)) * sum_steps;

                // Tuned Linear indexing
                // TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 ) - ( TsrcHistM( SurfNum, 2 ) - Tsrc1( SurfNum ) ) * sum_steps;
                // QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 ) - ( QsrcHistM( SurfNum, 2 ) - Qsrc1( SurfNum ) ) * sum_steps;

                if (construct.SourceSinkPresent) {
                    auto const l2(TsrcHist.index(SurfNum, 2));
                    TsrcHist[l2] = TsrcHistM[l2] - (TsrcHistM[l2] - Tsrc1(SurfNum)) * sum_steps;
                    QsrcHist[l2] = QsrcHistM[l2] - (QsrcHistM[l2] - Qsrc1(SurfNum)) * sum_steps;
                    TuserHist[l2] = TuserHistM[l2] - (TuserHistM[l2] - Tuser1(SurfNum)) * sum_steps;
                }
            }

        } // ...end of loop over all (heat transfer) surfaces
    }

    void CalculateZoneMRT(Optional_int_const ZoneToResimulate) // if passed in, then only calculate surfaces that have this zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the current zone MRT for thermal comfort and radiation
        // calculation purposes.

        // METHODOLOGY EMPLOYED:
        // If you have to ask...

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 SumAET;                    // Intermediate calculational variable (area*emissivity*T) sum
        static Array1D<Real64> SurfaceAE; // Product of area and emissivity for each surface
        int SurfNum;                      // Surface number
        static Array1D<Real64> ZoneAESum; // Sum of area times emissivity for all zone surfaces
        int ZoneNum;                      // Zone number

        // FLOW:
        if (CalculateZoneMRTfirstTime) {
            SurfaceAE.allocate(TotSurfaces);
            ZoneAESum.allocate(NumOfZones);
            SurfaceAE = 0.0;
            ZoneAESum = 0.0;
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).HeatTransSurf) {
                    SurfaceAE(SurfNum) = Surface(SurfNum).Area * dataConstruction.Construct(Surface(SurfNum).Construction).InsideAbsorpThermal;
                    ZoneNum = Surface(SurfNum).Zone;
                    if (ZoneNum > 0) ZoneAESum(ZoneNum) += SurfaceAE(SurfNum);
                }
            }
        }

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (present(ZoneToResimulate) && (ZoneNum != ZoneToResimulate)) continue;
            SumAET = 0.0;
            for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                if (Surface(SurfNum).HeatTransSurf) {
                    SumAET += SurfaceAE(SurfNum) * TempSurfIn(SurfNum);
                }
            }
            if (ZoneAESum(ZoneNum) > 0.01) {
                MRT(ZoneNum) = SumAET / ZoneAESum(ZoneNum);
            } else {
                if (CalculateZoneMRTfirstTime) {
                    ShowWarningError("Zone areas*inside surface emissivities are summing to zero, for Zone=\"" + Zone(ZoneNum).Name + "\"");
                    ShowContinueError("As a result, MRT will be set to MAT for that zone");
                }
                MRT(ZoneNum) = MAT(ZoneNum);
            }
        }

        CalculateZoneMRTfirstTime = false;
    }

    // End of Record Keeping subroutines for the HB Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the HB Module
    // *****************************************************************************

    void CalcThermalResilience(EnergyPlusData &state) {
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
        if (ManageSurfaceHeatBalancefirstTime) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                SetupOutputVariable(state, "Zone Heat Index", OutputProcessor::Unit::C, ZoneHeatIndex(ZoneNum), "Zone",
                                    "State", Zone(ZoneNum).Name);
                SetupOutputVariable(state, "Zone Humidity Index", OutputProcessor::Unit::None, ZoneHumidex(ZoneNum), "Zone",
                                    "State", Zone(ZoneNum).Name);
            }
            for (int Loop = 1; Loop <= OutputProcessor::NumOfReqVariables; ++Loop) {
                if (OutputProcessor::ReqRepVars(Loop).VarName == "Zone Heat Index") {
                    reportVarHeatIndex = true;
                }
                if (OutputProcessor::ReqRepVars(Loop).VarName == "Zone Humidity") {
                    reportVarHumidex = true;
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
        if (reportVarHeatIndex || OutputReportTabular::displayThermalResilienceSummary) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                Real64 ZoneT = ZTAV(ZoneNum);
                Real64 ZoneW = ZoneAirHumRatAvg(ZoneNum);
                Real64 ZoneRH = Psychrometrics::PsyRhFnTdbWPb(ZoneT, ZoneW, OutBaroPress) * 100.0;
                Real64 ZoneTF = ZoneT * (9.0 / 5.0) + 32.0;
                Real64 HI;

                if (ZoneTF < 80) {
                    HI = 0.5 * (ZoneTF + 61.0 + (ZoneTF - 68.0) * 1.2 + (ZoneRH * 0.094));
                } else {
                    HI = c1 + c2 * ZoneTF + c3 * ZoneRH + c4 * ZoneTF * ZoneRH + c5 * ZoneTF * ZoneTF +
                         c6 * ZoneRH * ZoneRH + c7 * ZoneTF * ZoneTF * ZoneRH + c8 * ZoneTF * ZoneRH * ZoneRH +
                         c9 * ZoneTF * ZoneTF * ZoneRH * ZoneRH;
                    if (ZoneRH < 13 && ZoneTF < 112) {
                        HI -= (13 - ZoneRH) / 4 * std::sqrt((17 - abs(ZoneTF - 95)) / 17);
                    } else if (ZoneRH > 85 && ZoneTF < 87) {
                        HI += (ZoneRH - 85) / 10 * (87 - ZoneTF) / 5;
                    }
                }
                HI = (HI - 32.0) * (5.0 / 9.0);
                ZoneHeatIndex(ZoneNum) = HI;
            }
        }
        if (reportVarHumidex || OutputReportTabular::displayThermalResilienceSummary) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                Real64 ZoneW = ZoneAirHumRatAvg(ZoneNum);
                Real64 ZoneT = ZTAV(ZoneNum);
                Real64 TDewPointK = Psychrometrics::PsyTdpFnWPb(ZoneW, OutBaroPress) + KelvinConv;
                Real64 e = 6.11 * std::exp(5417.7530 * ((1 / 273.16) - (1 / TDewPointK)));
                Real64 h = 5.0 / 9.0 * (e - 10.0);
                Real64 Humidex = ZoneT + h;
                ZoneHumidex(ZoneNum) = Humidex;
            }
        }
    }

    void ReportThermalResilience() {

        int HINoBins = 5; // Heat Index range - number of bins
        int HumidexNoBins = 5; // Humidex range - number of bins
        int SETNoBins = 4; // SET report column numbers

        if (reportThermalResilienceFirstTime) {
            if (TotPeople == 0) hasPierceSET = false;
            for (int iPeople = 1; iPeople <= TotPeople; ++iPeople) {
                if (!People(iPeople).Pierce) {
                    hasPierceSET = false;
                }
            }
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                ZoneHeatIndexHourBins(ZoneNum).assign(HINoBins, 0.0);
                ZoneHeatIndexOccuHourBins(ZoneNum).assign(HINoBins, 0.0);
                ZoneHumidexHourBins(ZoneNum).assign(HumidexNoBins, 0.0);
                ZoneHumidexOccuHourBins(ZoneNum).assign(HumidexNoBins, 0.0);
                if (hasPierceSET) {
                    ZoneLowSETHours(ZoneNum).assign(SETNoBins, 0.0);
                    ZoneHighSETHours(ZoneNum).assign(SETNoBins, 0.0);
                }
            }
            lowSETLongestHours.assign(NumOfZones, 0.0);
            highSETLongestHours.assign(NumOfZones, 0.0);
            lowSETLongestStart.assign(NumOfZones, 0.0);
            highSETLongestStart.assign(NumOfZones, 0.0);
            reportThermalResilienceFirstTime = false;
        }

        // Count hours only during weather simulation periods
        if (ksRunPeriodWeather == KindOfSim && !WarmupFlag) {
            // Trace current time step Zone Pierce SET; NaN if no occupant or SET not calculated
            // Record last time step SET to trace SET unmet duration;
            for (int iPeople = 1; iPeople <= TotPeople; ++iPeople) {
                int ZoneNum = People(iPeople).ZonePtr;
                ZoneNumOcc(ZoneNum) = People(iPeople).NumberOfPeople * GetCurrentScheduleValue(People(iPeople).NumberOfPeoplePtr);
                ZoneOccPierceSETLastStep(ZoneNum) = ZoneOccPierceSET(ZoneNum);
                if (ZoneNumOcc(ZoneNum) > 0) {
                    if (People(iPeople).Pierce) {
                        ZoneOccPierceSET(ZoneNum) = ThermalComfort::ThermalComfortData(iPeople).PierceSET;
                    } else {
                        ZoneOccPierceSET(ZoneNum) = -1;
                    }
                } else {
                    ZoneOccPierceSET(ZoneNum) = -1;
                }
            }
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                Real64 HI = ZoneHeatIndex(ZoneNum);
                Real64 Humidex = ZoneHumidex(ZoneNum);

                int NumOcc = ZoneNumOcc(ZoneNum);
                if (HI <= 26.7) {
                    ZoneHeatIndexHourBins(ZoneNum)[0] += TimeStepZone;
                    ZoneHeatIndexOccuHourBins(ZoneNum)[0] += NumOcc * TimeStepZone;
                } else if (HI > 26.7 && HI <= 32.2) {
                    ZoneHeatIndexHourBins(ZoneNum)[1] += TimeStepZone;
                    ZoneHeatIndexOccuHourBins(ZoneNum)[1] += NumOcc * TimeStepZone;
                } else if (HI > 32.2 && HI <= 39.4) {
                    ZoneHeatIndexHourBins(ZoneNum)[2] += TimeStepZone;
                    ZoneHeatIndexOccuHourBins(ZoneNum)[2] += NumOcc * TimeStepZone;
                } else if (HI > 39.4 && HI <= 51.7) {
                    ZoneHeatIndexHourBins(ZoneNum)[3] += TimeStepZone;
                    ZoneHeatIndexOccuHourBins(ZoneNum)[3] += NumOcc * TimeStepZone;
                } else {
                    ZoneHeatIndexHourBins(ZoneNum)[4] += TimeStepZone;
                    ZoneHeatIndexOccuHourBins(ZoneNum)[4] += NumOcc * TimeStepZone;
                }

                if (Humidex <= 29) {
                    ZoneHumidexHourBins(ZoneNum)[0] += TimeStepZone;
                    ZoneHumidexOccuHourBins(ZoneNum)[0] += NumOcc * TimeStepZone;
                } else if (Humidex > 29 && Humidex <= 40) {
                    ZoneHumidexHourBins(ZoneNum)[1] += TimeStepZone;
                    ZoneHumidexOccuHourBins(ZoneNum)[1] += NumOcc * TimeStepZone;
                } else if (Humidex > 40 && Humidex <= 45) {
                    ZoneHumidexHourBins(ZoneNum)[2] += TimeStepZone;
                    ZoneHumidexOccuHourBins(ZoneNum)[2] += NumOcc * TimeStepZone;
                } else if (Humidex > 45 && Humidex <= 50) {
                    ZoneHumidexHourBins(ZoneNum)[3] += TimeStepZone;
                    ZoneHumidexOccuHourBins(ZoneNum)[3] += NumOcc * TimeStepZone;
                } else {
                    ZoneHumidexHourBins(ZoneNum)[4] += TimeStepZone;
                    ZoneHumidexOccuHourBins(ZoneNum)[4] += NumOcc * TimeStepZone;
                }

                if (hasPierceSET) {
                    int encodedMonDayHrMin;
                    if (NumOcc > 0) {
                        Real64 PierceSET = ZoneOccPierceSET(ZoneNum);
                        Real64 PierceSETLast = ZoneOccPierceSETLastStep(ZoneNum);

                        if (PierceSET <= 12.2) {
                            ZoneLowSETHours(ZoneNum)[0] += (12.2 - PierceSET) * TimeStepZone;
                            ZoneLowSETHours(ZoneNum)[1] += (12.2 - PierceSET) * NumOcc * TimeStepZone;
                            // Reset duration when last step is out of range.
                            if (PierceSETLast == -1 || PierceSETLast > 12.2) {
                                General::EncodeMonDayHrMin(encodedMonDayHrMin, Month, DayOfMonth, HourOfDay,
                                                           TimeStepZone * (TimeStep - 1) * 60);
                                lowSETLongestHours[ZoneNum - 1] = 0;
                                lowSETLongestStart[ZoneNum - 1] = encodedMonDayHrMin;
                            }
                            // Keep the longest duration record.
                            lowSETLongestHours[ZoneNum - 1] += TimeStepZone;
                            if (lowSETLongestHours[ZoneNum - 1] > ZoneLowSETHours(ZoneNum)[2]) {
                                ZoneLowSETHours(ZoneNum)[2] = lowSETLongestHours[ZoneNum - 1];
                                ZoneLowSETHours(ZoneNum)[3] = lowSETLongestStart[ZoneNum - 1];
                            }
                        } else if (PierceSET > 30) {
                            ZoneHighSETHours(ZoneNum)[0] += (PierceSET - 30) * TimeStepZone;
                            ZoneHighSETHours(ZoneNum)[1] += (PierceSET - 30) * NumOcc * TimeStepZone;
                            if (PierceSETLast == -1 || PierceSETLast <= 30) {
                                General::EncodeMonDayHrMin(encodedMonDayHrMin, Month, DayOfMonth, HourOfDay,
                                                           TimeStepZone * (TimeStep - 1) * 60);
                                highSETLongestHours[ZoneNum - 1] = 0;
                                highSETLongestStart[ZoneNum - 1] = encodedMonDayHrMin;
                            }
                            highSETLongestHours[ZoneNum - 1] += TimeStepZone;
                            if (highSETLongestHours[ZoneNum - 1] > ZoneHighSETHours(ZoneNum)[2]) {
                                ZoneHighSETHours(ZoneNum)[2] = highSETLongestHours[ZoneNum - 1];
                                ZoneHighSETHours(ZoneNum)[3] = highSETLongestStart[ZoneNum - 1];
                            }
                        }
                    } else {
                        // No occupants: record the last time step duration if longer than the record.
                        if (lowSETLongestHours[ZoneNum - 1] > ZoneLowSETHours(ZoneNum)[2]) {
                            ZoneLowSETHours(ZoneNum)[2] = lowSETLongestHours[ZoneNum - 1];
                            ZoneLowSETHours(ZoneNum)[3] = lowSETLongestStart[ZoneNum - 1];
                        }
                        if (highSETLongestHours[ZoneNum - 1] > ZoneHighSETHours(ZoneNum)[2]) {
                            ZoneHighSETHours(ZoneNum)[2] = highSETLongestHours[ZoneNum - 1];
                            ZoneHighSETHours(ZoneNum)[3] = highSETLongestStart[ZoneNum - 1];
                        }
                        lowSETLongestHours[ZoneNum - 1] = 0;
                        highSETLongestHours[ZoneNum - 1] = 0;
                    }
                }
            }
        } // loop over zones
    }

    void ReportCO2Resilience() {
        int NoBins = 3;
        if (reportCO2ResilienceFirstTime) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                ZoneCO2LevelHourBins(ZoneNum).assign(NoBins, 0.0);
                ZoneCO2LevelOccuHourBins(ZoneNum).assign(NoBins, 0.0);
            }
            reportCO2ResilienceFirstTime = false;
            if (!DataContaminantBalance::Contaminant.CO2Simulation) {
                if (OutputReportTabular::displayCO2ResilienceSummaryExplicitly) {
                    ShowWarningError("Writing Annual CO2 Resilience Summary - CO2 Level Hours reports: "
                                     "Zone Air CO2 Concentration output is required, "
                                     "but no ZoneAirContaminantBalance object is defined.");
                }
                OutputReportTabular::displayCO2ResilienceSummary = false;
                return;
            }
        }

        if (ksRunPeriodWeather == KindOfSim && !WarmupFlag) {
            for (int iPeople = 1; iPeople <= TotPeople; ++iPeople) {
                int ZoneNum = People(iPeople).ZonePtr;
                ZoneNumOcc(ZoneNum) = People(iPeople).NumberOfPeople * GetCurrentScheduleValue(People(iPeople).NumberOfPeoplePtr);
            }
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                Real64 ZoneAirCO2 = DataContaminantBalance::ZoneAirCO2Avg(ZoneNum);

                int NumOcc = ZoneNumOcc(ZoneNum);
                if (ZoneAirCO2 <= 1000) {
                    ZoneCO2LevelHourBins(ZoneNum)[0] += TimeStepZone;
                    ZoneCO2LevelOccuHourBins(ZoneNum)[0] += NumOcc * TimeStepZone;
                } else if (ZoneAirCO2 > 1000 && ZoneAirCO2 <= 5000) {
                    ZoneCO2LevelHourBins(ZoneNum)[1] += TimeStepZone;
                    ZoneCO2LevelOccuHourBins(ZoneNum)[1] += NumOcc * TimeStepZone;
                } else {
                    ZoneCO2LevelHourBins(ZoneNum)[2] += TimeStepZone;
                    ZoneCO2LevelOccuHourBins(ZoneNum)[2] += NumOcc * TimeStepZone;
                }
            }
        } // loop over zones
    }

    void ReportVisualResilience() {
        int NoBins = 4;
        if (reportVisualResilienceFirstTime) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                ZoneLightingLevelHourBins(ZoneNum).assign(NoBins, 0.0);
                ZoneLightingLevelOccuHourBins(ZoneNum).assign(NoBins, 0.0);
            }
            reportVisualResilienceFirstTime = false;
            bool hasDayLighting = false;
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                if (DataDaylighting::ZoneDaylight(ZoneNum).DaylightMethod != DataDaylighting::NoDaylighting) {
                    hasDayLighting = true;
                    break;
                }
            }
            if (!hasDayLighting) {
                if (OutputReportTabular::displayVisualResilienceSummaryExplicitly) {
                    ShowWarningError("Writing Annual Visual Resilience Summary - Lighting Level Hours reports: "
                                     "Zone Average Daylighting Reference Point Illuminance output is required, "
                                     "but no Daylighting Control Object is defined.");
                }
                OutputReportTabular::displayVisualResilienceSummary = false;
                return;
            }
        }

        if (ksRunPeriodWeather == KindOfSim && !WarmupFlag) {
            for (int iPeople = 1; iPeople <= TotPeople; ++iPeople) {
                int ZoneNum = People(iPeople).ZonePtr;
                ZoneNumOcc(ZoneNum) = People(iPeople).NumberOfPeople * GetCurrentScheduleValue(People(iPeople).NumberOfPeoplePtr);
            }
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                // Place holder
                if (DataDaylighting::ZoneDaylight(ZoneNum).DaylightMethod == DataDaylighting::NoDaylighting)
                    continue;

                Array1D<Real64> ZoneIllumRef = DataDaylighting::ZoneDaylight(ZoneNum).DaylIllumAtRefPt;
                Real64 ZoneIllum = 0.0;
                for (size_t i = 1; i <= ZoneIllumRef.size(); i++) {
                    ZoneIllum += ZoneIllumRef(i);
                }
                ZoneIllum /= ZoneIllumRef.size();

                if (DataDaylighting::ZoneDaylight(ZoneNum).ZonePowerReductionFactor > 0) {
                    Array1D<Real64> ZoneIllumSetpoint = DataDaylighting::ZoneDaylight(ZoneNum).IllumSetPoint;
                    ZoneIllum = 0.0;
                    for (size_t i = 1; i <= ZoneIllumSetpoint.size(); i++) {
                        ZoneIllum += ZoneIllumSetpoint(i);
                    }
                    ZoneIllum /= ZoneIllumSetpoint.size();
                }

                int NumOcc = ZoneNumOcc(ZoneNum);
                if (ZoneIllum <= 100) {
                    ZoneLightingLevelHourBins(ZoneNum)[0] += TimeStepZone;
                    ZoneLightingLevelOccuHourBins(ZoneNum)[0] += NumOcc * TimeStepZone;
                } else if (ZoneIllum > 100 && ZoneIllum <= 300) {
                    ZoneLightingLevelHourBins(ZoneNum)[1] += TimeStepZone;
                    ZoneLightingLevelOccuHourBins(ZoneNum)[1] += NumOcc * TimeStepZone;
                } else if (ZoneIllum > 300 && ZoneIllum <= 500) {
                    ZoneLightingLevelHourBins(ZoneNum)[2] += TimeStepZone;
                    ZoneLightingLevelOccuHourBins(ZoneNum)[2] += NumOcc * TimeStepZone;
                } else {
                    ZoneLightingLevelHourBins(ZoneNum)[3] += TimeStepZone;
                    ZoneLightingLevelOccuHourBins(ZoneNum)[3] += NumOcc * TimeStepZone;
                }
            }
        } // loop over zones
    }
    void ReportSurfaceHeatBalance()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine puts the reporting part of the HBSurface Module in one area.

        using DataGlobals::CompLoadReportIsReq;
        using DataGlobals::NumOfTimeStepInHour;
        using DataSizing::CurOverallSimDay;
        using OutputReportTabular::feneSolarRadSeq;
        using OutputReportTabular::lightSWRadSeq;
        using SolarShading::ReportSurfaceShading;

        SumSurfaceHeatEmission = 0.0;

        ZoneMRT({1, NumOfZones}) = MRT({1, NumOfZones});

        ReportSurfaceShading();

        // update inside face radiation reports
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            Real64 const surfaceArea(Surface(SurfNum).Area);
            // Tuned Replaced by one line form below for speed
            //			QdotRadNetSurfInRep( SurfNum ) = NetLWRadToSurf( SurfNum ) * surfaceArea;
            //			QdotRadNetSurfInRepPerArea( SurfNum ) = NetLWRadToSurf( SurfNum );
            QdotRadNetSurfInRep(SurfNum) = (QdotRadNetSurfInRepPerArea(SurfNum) = SurfNetLWRadToSurf(SurfNum)) * surfaceArea;
            QRadNetSurfInReport(SurfNum) = QdotRadNetSurfInRep(SurfNum) * TimeStepZoneSec;

            if (Surface(SurfNum).Class != SurfaceClass_Window) { // not a window...
                QdotRadSolarInRepPerArea(SurfNum) = SurfOpaqQRadSWInAbs(SurfNum) - SurfOpaqQRadSWLightsInAbs(SurfNum);
                QdotRadSolarInRep(SurfNum) = QdotRadSolarInRepPerArea(SurfNum) * surfaceArea;
                QRadSolarInReport(SurfNum) = QdotRadSolarInRep(SurfNum) * TimeStepZoneSec;

                QdotRadLightsInRepPerArea(SurfNum) = SurfOpaqQRadSWLightsInAbs(SurfNum);
                QdotRadLightsInRep(SurfNum) = QdotRadLightsInRepPerArea(SurfNum) * surfaceArea;
                QRadLightsInReport(SurfNum) = QdotRadLightsInRep(SurfNum) * TimeStepZoneSec;

                if (ZoneSizingCalc && CompLoadReportIsReq) {
                    int TimeStepInDay = (HourOfDay - 1) * NumOfTimeStepInHour + TimeStep;
                    lightSWRadSeq(CurOverallSimDay, TimeStepInDay, SurfNum) = QdotRadLightsInRep(SurfNum);
                    feneSolarRadSeq(CurOverallSimDay, TimeStepInDay, SurfNum) = QdotRadSolarInRep(SurfNum);
                }
            } else { // can we fill these for windows?
            }

            // Tuned Replaced by one line form below for speed
            //			QdotRadIntGainsInRepPerArea( SurfNum ) = QRadThermInAbs( SurfNum );
            //			QdotRadIntGainsInRep( SurfNum ) = QdotRadIntGainsInRepPerArea( SurfNum ) * surfaceArea;
            QdotRadIntGainsInRep(SurfNum) = (QdotRadIntGainsInRepPerArea(SurfNum) = SurfQRadThermInAbs(SurfNum)) * surfaceArea;
            QRadIntGainsInReport(SurfNum) = QdotRadIntGainsInRep(SurfNum) * TimeStepZoneSec;

            QdotRadHVACInRepPerArea(SurfNum) = QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) + QHWBaseboardSurf(SurfNum) +
                                               QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum);
            QdotRadHVACInRep(SurfNum) = QdotRadHVACInRepPerArea(SurfNum) * Surface(SurfNum).Area;
            // Tuned Replaced by one line form below for speed
            //			QdotRadHVACInRepPerArea( SurfNum ) = QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf(
            // SurfNum
            //)
            //+  QElecBaseboardSurf( SurfNum ); 			QdotRadHVACInRep( SurfNum ) = QdotRadHVACInRepPerArea( SurfNum ) *
            // surfaceArea;
            QdotRadHVACInRep(SurfNum) =
                (QdotRadHVACInRepPerArea(SurfNum) = QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) + QHWBaseboardSurf(SurfNum) +
                                                    QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)) *
                surfaceArea;
            QRadHVACInReport(SurfNum) = QdotRadHVACInRep(SurfNum) * TimeStepZoneSec;

            if (Surface(SurfNum).Class == SurfaceClass_Floor || Surface(SurfNum).Class == SurfaceClass_Wall ||
                Surface(SurfNum).Class == SurfaceClass_IntMass || Surface(SurfNum).Class == SurfaceClass_Roof ||
                Surface(SurfNum).Class == SurfaceClass_Door) {

                // inside face conduction updates
                SurfOpaqInsFaceConductionEnergy(SurfNum) = SurfOpaqInsFaceConduction(SurfNum) * TimeStepZoneSec;
                ZoneOpaqSurfInsFaceCond(Surface(SurfNum).Zone) += SurfOpaqInsFaceConduction(SurfNum);
                SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
                SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
                if (SurfOpaqInsFaceConduction(SurfNum) >= 0.0) {
                    SurfOpaqInsFaceCondGainRep(SurfNum) = SurfOpaqInsFaceConduction(SurfNum);
                } else {
                    SurfOpaqInsFaceCondLossRep(SurfNum) = - SurfOpaqInsFaceConduction(SurfNum);
                }

                // outside face conduction updates
                SurfOpaqOutsideFaceConductionEnergy(SurfNum) = SurfOpaqOutsideFaceConduction(SurfNum) * TimeStepZoneSec;
                ZoneOpaqSurfExtFaceCond(Surface(SurfNum).Zone) += SurfOpaqOutsideFaceConduction(SurfNum);
                SurfOpaqExtFaceCondGainRep(SurfNum) = 0.0;
                SurfOpaqExtFaceCondLossRep(SurfNum) = 0.0;
                if (SurfOpaqOutsideFaceConduction(SurfNum) >= 0.0) {
                    SurfOpaqExtFaceCondGainRep(SurfNum) = SurfOpaqOutsideFaceConduction(SurfNum);
                } else {
                    SurfOpaqExtFaceCondLossRep(SurfNum) = -SurfOpaqOutsideFaceConduction(SurfNum);
                }

                // do average surface conduction updates

                SurfOpaqAvgFaceConduction(SurfNum) = (SurfOpaqInsFaceConduction(SurfNum) - SurfOpaqOutsideFaceConduction(SurfNum)) / 2.0;
                SurfOpaqAvgFaceConductionFlux(SurfNum) = (SurfOpaqInsFaceConductionFlux(SurfNum) - SurfOpaqOutsideFaceConductionFlux(SurfNum)) / 2.0;
                SurfOpaqAvgFaceConductionEnergy(SurfNum) = SurfOpaqAvgFaceConduction(SurfNum) * TimeStepZoneSec;
                SurfOpaqAvgFaceCondGainRep(SurfNum) = 0.0;
                SurfOpaqAvgFaceCondLossRep(SurfNum) = 0.0;
                if (SurfOpaqAvgFaceConduction(SurfNum) >= 0.0) {
                    SurfOpaqAvgFaceCondGainRep(SurfNum) = SurfOpaqAvgFaceConduction(SurfNum);
                } else {
                    SurfOpaqAvgFaceCondLossRep(SurfNum) = - SurfOpaqAvgFaceConduction(SurfNum);
                }

                // do surface storage rate updates
                SurfOpaqStorageConductionFlux(SurfNum) = - (SurfOpaqInsFaceConductionFlux(SurfNum) + SurfOpaqOutsideFaceConductionFlux(SurfNum));
                SurfOpaqStorageConduction(SurfNum) = - (SurfOpaqInsFaceConduction(SurfNum) + SurfOpaqOutsideFaceConduction(SurfNum));
                SurfOpaqStorageConductionEnergy(SurfNum) = SurfOpaqStorageConduction(SurfNum) * TimeStepZoneSec;
                SurfOpaqStorageGainRep(SurfNum) = 0.0;
                SurfOpaqStorageCondLossRep(SurfNum) = 0.0;
                if (SurfOpaqStorageConduction(SurfNum) >= 0.0) {
                    SurfOpaqStorageGainRep(SurfNum) = SurfOpaqStorageConduction(SurfNum);
                } else {
                    SurfOpaqStorageCondLossRep(SurfNum) = - SurfOpaqStorageConduction(SurfNum);
                }

            } // opaque heat transfer surfaces.
            if (Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {
                SumSurfaceHeatEmission += QHeatEmiReport(SurfNum) * TimeStepZoneSec;
            }
        } // loop over surfaces
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (ZoneOpaqSurfInsFaceCond(ZoneNum) >= 0.0) {
                ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) = ZoneOpaqSurfInsFaceCond(ZoneNum);
                ZnOpqSurfInsFaceCondGnRepEnrg(ZoneNum) = ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) * TimeStepZoneSec;
            } else {
                ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) = -ZoneOpaqSurfInsFaceCond(ZoneNum);
                ZnOpqSurfInsFaceCondLsRepEnrg(ZoneNum) = ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) * TimeStepZoneSec;
            }

            if (ZoneOpaqSurfExtFaceCond(ZoneNum) >= 0.0) {
                ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) = ZoneOpaqSurfExtFaceCond(ZoneNum);
                ZnOpqSurfExtFaceCondGnRepEnrg(ZoneNum) = ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) * TimeStepZoneSec;
            } else {
                ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) = -ZoneOpaqSurfExtFaceCond(ZoneNum);
                ZnOpqSurfExtFaceCondLsRepEnrg(ZoneNum) = ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) * TimeStepZoneSec;
            }
        } // loop over zones
    }

    void ReportIntMovInsInsideSurfTemp()
    {
        int SurfNum;
        TempSurfInMovInsRep = TempSurfIn;
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (Surface(SurfNum).MaterialMovInsulInt > 0) {
                if (GetCurrentScheduleValue(Surface(SurfNum).SchedMovInsulInt) > 0.0) {
                    TempSurfInMovInsRep(SurfNum) = TempSurfInTmp(SurfNum);
                }
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
        using namespace DataGlobals;
        using namespace DataEnvironment;
        using namespace DataHeatBalFanSys;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;
        using namespace DataSurfaces;
        using ConvectionCoefficients::InitExteriorConvectionCoeff;
        using ConvectionCoefficients::SetExtConvectionCoeff;
        using ConvectionCoefficients::SetIntConvectionCoeff;
        using DataMoistureBalance::HAirFD;
        using DataMoistureBalance::HConvExtFD;
        using DataMoistureBalance::HConvInFD;
        using DataMoistureBalance::HGrndFD;
        using DataMoistureBalance::HMassConvExtFD;
        using DataMoistureBalance::HMassConvInFD;
        using DataMoistureBalance::HSkyFD;
        using DataMoistureBalance::RhoVaporAirIn;
        using DataMoistureBalance::RhoVaporAirOut;
        using DataMoistureBalance::RhoVaporSurfIn;
        using DataMoistureBalance::TempOutsideAirFD;
        using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
        using HeatBalanceMovableInsulation::EvalOutsideMovableInsulation;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;
        using namespace Psychrometrics;
        using EcoRoofManager::CalcEcoRoof;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcHeatBalanceOutsideSurf");
        static std::string const RoutineNameGroundTemp("CalcHeatBalanceOutsideSurf:GroundTemp");
        static std::string const RoutineNameGroundTempFC("CalcHeatBalanceOutsideSurf:GroundTempFC");
        static std::string const RoutineNameOtherSideCoefNoCalcExt("CalcHeatBalanceOutsideSurf:OtherSideCoefNoCalcExt");
        static std::string const RoutineNameOtherSideCoefCalcExt("CalcHeatBalanceOutsideSurf:OtherSideCoefCalcExt");
        static std::string const RoutineNameOSCM("CalcHeatBalanceOutsideSurf:OSCM");
        static std::string const RoutineNameExtEnvWetSurf("CalcHeatBalanceOutsideSurf:extEnvWetSurf");
        static std::string const RoutineNameExtEnvDrySurf("CalcHeatBalanceOutsideSurf:extEnvDrySurf");
        static std::string const RoutineNameNoWind("CalcHeatBalanceOutsideSurf:nowind");
        static std::string const RoutineNameOther("CalcHeatBalanceOutsideSurf:interior/other");
        static std::string const RoutineNameIZPart("CalcHeatBalanceOutsideSurf:IZPart");
        static std::string const HBSurfManGroundHAMT("HBSurfMan:Ground:HAMT");
        static std::string const HBSurfManRainHAMT("HBSurfMan:Rain:HAMT");
        static std::string const HBSurfManDrySurfCondFD("HBSurfMan:DrySurf:CondFD");
        static std::string const Outside("Outside");
        static std::string const BlankString;

        // FLOW:
        bool MovInsulErrorFlag = false; // Movable Insulation error flag

        if (AnyConstructInternalSourceInInput) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                // Need to transfer any source/sink for a surface to the local array.  Note that
                // the local array is flux (W/m2) while the QRadSysSource is heat transfer (W).
                // This must be done at this location so that this is always updated correctly.
                if (Surface(SurfNum).Area > 0.0)
                    QsrcHist(SurfNum, 1) = QRadSysSource(SurfNum) / Surface(SurfNum).Area; // Make sure we don't divide by zero...

                // next we add source (actually a sink) from any integrated PV
                if (Surface(SurfNum).Area > 0.0)
                    QsrcHist(SurfNum, 1) += QPVSysSource(SurfNum) / Surface(SurfNum).Area; // Make sure we don't divide by zero...
            }
        }

        if (present(ZoneToResimulate)) {
            CalcInteriorRadExchange(TH(2, 1, _), 0, SurfNetLWRadToSurf, ZoneToResimulate, Outside);
        } else {
            CalcInteriorRadExchange(TH(2, 1, _), 0, SurfNetLWRadToSurf, _, Outside);
        }

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {// Loop through all surfaces...
            int const firstSurfOpaque = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpaque = Zone(zoneNum).NonWindowSurfaceLast;
            if (firstSurfOpaque <= 0) continue;
            for (int SurfNum = firstSurfOpaque; SurfNum <= lastSurfOpaque; ++SurfNum) {

                if (present(ZoneToResimulate)) {
                    if ((zoneNum != ZoneToResimulate) && (AdjacentZoneToSurface(SurfNum) != ZoneToResimulate)) {
                        continue; // skip surfaces that are not associated with this zone
                    }
                }
                // Interior windows in partitions use "normal" heat balance calculations
                // For rest, Outside surface temp of windows not needed in Window5 calculation approach.
                // Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

                // Initializations for this surface
                int ConstrNum = Surface(SurfNum).Construction;
                Real64 HMovInsul = 0.0; // "Convection" coefficient of movable insulation
                Real64 HSky = 0.0;      // "Convection" coefficient from sky to surface
                Real64 HGround = 0.0;   // "Convection" coefficient from ground to surface
                Real64 HAir = 0.0;      // "Convection" coefficient from air to surface (radiation)
                HcExtSurf(SurfNum) = 0.0;
                HAirExtSurf(SurfNum) = 0.0;
                HSkyExtSurf(SurfNum) = 0.0;
                HGrdExtSurf(SurfNum) = 0.0;
                SurfQRadLWOutSrdSurfs(SurfNum) = 0.0;

                // Calculate heat extract due to additional heat flux source term as the surface boundary condition

                if (Surface(SurfNum).OutsideHeatSourceTermSchedule) {
                    SurfQAdditionalHeatSourceOutside(SurfNum) = EnergyPlus::ScheduleManager::GetCurrentScheduleValue(
                            Surface(SurfNum).OutsideHeatSourceTermSchedule);
                }

                // Calculate the current outside surface temperature TH(SurfNum,1,1) for the
                // various different boundary conditions
                {
                    auto const SELECT_CASE_var(Surface(SurfNum).ExtBoundCond);

                    if (SELECT_CASE_var == Ground) { // Surface in contact with ground

                        TH(1, 1, SurfNum) = GroundTemp;

                        // Set the only radiant system heat balance coefficient that is non-zero for this case
                        if (dataConstruction.Construct(ConstrNum).SourceSinkPresent)
                            RadSysToHBConstCoef(SurfNum) = TH(1, 1, SurfNum);

                        // start HAMT
                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            // Set variables used in the HAMT moisture balance
                            TempOutsideAirFD(SurfNum) = GroundTemp;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRh(GroundTemp, 1.0, HBSurfManGroundHAMT);
                            HConvExtFD(SurfNum) = HighHConvLimit;

                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, GroundTemp,
                                                                          PsyWFnTdbRhPb(GroundTemp, 1.0, OutBaroPress,
                                                                                        RoutineNameGroundTemp)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));

                            HSkyFD(SurfNum) = HSky;
                            HGrndFD(SurfNum) = HGround;
                            HAirFD(SurfNum) = HAir;
                        }
                        // end HAMT

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                            // Set variables used in the FD moisture balance
                            TempOutsideAirFD(SurfNum) = GroundTemp;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(GroundTemp, 1.0);
                            HConvExtFD(SurfNum) = HighHConvLimit;
                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, GroundTemp,
                                                                          PsyWFnTdbRhPb(GroundTemp, 1.0, OutBaroPress,
                                                                                        RoutineNameGroundTemp)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                            HSkyFD(SurfNum) = HSky;
                            HGrndFD(SurfNum) = HGround;
                            HAirFD(SurfNum) = HAir;
                        }

                        // Added for FCfactor grounds
                    } else if (SELECT_CASE_var == GroundFCfactorMethod) { // Surface in contact with ground

                        TH(1, 1, SurfNum) = GroundTempFC;

                        // Set the only radiant system heat balance coefficient that is non-zero for this case
                        if (dataConstruction.Construct(ConstrNum).SourceSinkPresent)
                            RadSysToHBConstCoef(SurfNum) = TH(1, 1, SurfNum);

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            // Set variables used in the HAMT moisture balance
                            TempOutsideAirFD(SurfNum) = GroundTempFC;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRh(GroundTempFC, 1.0, HBSurfManGroundHAMT);
                            HConvExtFD(SurfNum) = HighHConvLimit;

                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, GroundTempFC,
                                                                          PsyWFnTdbRhPb(GroundTempFC, 1.0, OutBaroPress,
                                                                                        RoutineNameGroundTempFC)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));

                            HSkyFD(SurfNum) = HSky;
                            HGrndFD(SurfNum) = HGround;
                            HAirFD(SurfNum) = HAir;
                        }

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                            // Set variables used in the FD moisture balance
                            TempOutsideAirFD(SurfNum) = GroundTempFC;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(GroundTempFC, 1.0);
                            HConvExtFD(SurfNum) = HighHConvLimit;
                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, GroundTempFC,
                                                                          PsyWFnTdbRhPb(GroundTempFC, 1.0, OutBaroPress,
                                                                                        RoutineNameGroundTempFC)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                            HSkyFD(SurfNum) = HSky;
                            HGrndFD(SurfNum) = HGround;
                            HAirFD(SurfNum) = HAir;
                        }

                    } else if (SELECT_CASE_var == OtherSideCoefNoCalcExt) {
                        // Use Other Side Coefficients to determine the surface film coefficient and
                        // the exterior boundary condition temperature

                        int OPtr = Surface(SurfNum).OSCPtr;
                        // Set surface temp from previous timestep
                        if (BeginTimeStepFlag) {
                            OSC(OPtr).TOutsideSurfPast = TH(1, 1, SurfNum);
                        }

                        if (OSC(OPtr).ConstTempScheduleIndex != 0) { // Determine outside temperature from schedule
                            OSC(OPtr).ConstTemp = GetCurrentScheduleValue(OSC(OPtr).ConstTempScheduleIndex);
                        }

                        //  Allow for modification of TemperatureCoefficient with unitary sine wave.
                        Real64 ConstantTempCoef; // Temperature Coefficient as input or modified using sine wave COP mod
                        if (OSC(OPtr).SinusoidalConstTempCoef) { // Sine wave C4
                            ConstantTempCoef = std::sin(2 * Pi * CurrentTime / OSC(OPtr).SinusoidPeriod);
                        } else {
                            ConstantTempCoef = OSC(OPtr).ConstTempCoef;
                        }

                        OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(zoneNum) +
                                                 OSC(OPtr).ExtDryBulbCoef * Surface(SurfNum).OutDryBulbTemp +
                                                 ConstantTempCoef * OSC(OPtr).ConstTemp +
                                                 OSC(OPtr).GroundTempCoef * GroundTemp +
                                                 OSC(OPtr).WindSpeedCoef * Surface(SurfNum).WindSpeed *
                                                 Surface(SurfNum).OutDryBulbTemp +
                                                 OSC(OPtr).TPreviousCoef * OSC(OPtr).TOutsideSurfPast);

                        // Enforce max/min limits if applicable
                        if (OSC(OPtr).MinLimitPresent)
                            OSC(OPtr).OSCTempCalc = max(OSC(OPtr).MinTempLimit, OSC(OPtr).OSCTempCalc);
                        if (OSC(OPtr).MaxLimitPresent)
                            OSC(OPtr).OSCTempCalc = min(OSC(OPtr).MaxTempLimit, OSC(OPtr).OSCTempCalc);

                        TH(1, 1, SurfNum) = OSC(OPtr).OSCTempCalc;

                        // Set the only radiant system heat balance coefficient that is non-zero for this case
                        if (dataConstruction.Construct(ConstrNum).SourceSinkPresent)
                            RadSysToHBConstCoef(SurfNum) = TH(1, 1, SurfNum);

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            // Set variables used in the FD moisture balance and HAMT
                            TempOutsideAirFD(SurfNum) = TH(1, 1, SurfNum);
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum), OutHumRat,
                                                                      OutBaroPress);
                            HConvExtFD(SurfNum) = HighHConvLimit;
                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                          PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum), 1.0,
                                                                                        OutBaroPress,
                                                                                        RoutineNameOtherSideCoefNoCalcExt)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                            HSkyFD(SurfNum) = HSky;
                            HGrndFD(SurfNum) = HGround;
                            HAirFD(SurfNum) = HAir;
                        }

                        // This ends the calculations for this surface and goes on to the next SurfNum

                    } else if (SELECT_CASE_var ==
                               OtherSideCoefCalcExt) { // A surface with other side coefficients that define the outside environment

                        // First, set up the outside convection coefficient and the exterior temperature
                        // boundary condition for the surface
                        int OPtr = Surface(SurfNum).OSCPtr;
                        // Set surface temp from previous timestep
                        if (BeginTimeStepFlag) {
                            OSC(OPtr).TOutsideSurfPast = TH(1, 1, SurfNum);
                        }

                        if (OSC(OPtr).ConstTempScheduleIndex != 0) { // Determine outside temperature from schedule
                            OSC(OPtr).ConstTemp = GetCurrentScheduleValue(OSC(OPtr).ConstTempScheduleIndex);
                        }

                        HcExtSurf(SurfNum) = OSC(OPtr).SurfFilmCoef;

                        OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(zoneNum) +
                                                 OSC(OPtr).ExtDryBulbCoef * Surface(SurfNum).OutDryBulbTemp +
                                                 OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp +
                                                 OSC(OPtr).GroundTempCoef * GroundTemp +
                                                 OSC(OPtr).WindSpeedCoef * Surface(SurfNum).WindSpeed *
                                                 Surface(SurfNum).OutDryBulbTemp +
                                                 OSC(OPtr).TPreviousCoef * OSC(OPtr).TOutsideSurfPast);

                        // Enforce max/min limits if applicable
                        if (OSC(OPtr).MinLimitPresent)
                            OSC(OPtr).OSCTempCalc = max(OSC(OPtr).MinTempLimit, OSC(OPtr).OSCTempCalc);
                        if (OSC(OPtr).MaxLimitPresent)
                            OSC(OPtr).OSCTempCalc = min(OSC(OPtr).MaxTempLimit, OSC(OPtr).OSCTempCalc);

                        Real64 TempExt = OSC(OPtr).OSCTempCalc;

                        // Set the only radiant system heat balance coefficient that is non-zero for this case
                        if (dataConstruction.Construct(ConstrNum).SourceSinkPresent)
                            RadSysToHBConstCoef(SurfNum) = TH(1, 1, SurfNum);

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            // Set variables used in the FD moisture balance and HAMT
                            TempOutsideAirFD(SurfNum) = TempExt;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum), OutHumRat,
                                                                      OutBaroPress);
                            HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                          PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum), 1.0,
                                                                                        OutBaroPress,
                                                                                        RoutineNameOtherSideCoefCalcExt)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                            HSkyFD(SurfNum) = HSkyExtSurf(SurfNum);
                            HGrndFD(SurfNum) = HGrdExtSurf(SurfNum);
                            HAirFD(SurfNum) = HAirExtSurf(SurfNum);
                        }

                        // Call the outside surface temp calculation and pass the necessary terms
                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                            CalcOutsideSurfTemp(SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                            if (MovInsulErrorFlag)
                                ShowFatalError("CalcOutsideSurfTemp: Program terminates due to preceding conditions.");
                        }

                        // This ends the calculations for this surface and goes on to the next SurfNum

                    } else if (SELECT_CASE_var ==
                               OtherSideCondModeledExt) { // A surface with other side conditions determined from seperate, dynamic component
                        //                               modeling that defines the "outside environment"

                        // First, set up the outside convection coefficient and the exterior temperature
                        // boundary condition for the surface
                        int OPtr = Surface(SurfNum).OSCMPtr;
                        // EMS overrides
                        if (OSCM(OPtr).EMSOverrideOnTConv) OSCM(OPtr).TConv = OSCM(OPtr).EMSOverrideTConvValue;
                        if (OSCM(OPtr).EMSOverrideOnHConv) OSCM(OPtr).HConv = OSCM(OPtr).EMSOverrideHConvValue;
                        if (OSCM(OPtr).EMSOverrideOnTRad) OSCM(OPtr).TRad = OSCM(OPtr).EMSOverrideTRadValue;
                        if (OSCM(OPtr).EMSOverrideOnHrad) OSCM(OPtr).HRad = OSCM(OPtr).EMSOverrideHradValue;
                        HcExtSurf(SurfNum) = OSCM(OPtr).HConv;

                        Real64 TempExt = OSCM(OPtr).TConv;

                        // Set the only radiant system heat balance coefficient that is non-zero for this case
                        if (dataConstruction.Construct(ConstrNum).SourceSinkPresent)
                            RadSysToHBConstCoef(SurfNum) = TH(1, 1, SurfNum);

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            // Set variables used in the FD moisture balance and HAMT
                            TempOutsideAirFD(SurfNum) = TempExt;
                            RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum), OutHumRat,
                                                                      OutBaroPress);
                            HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                            HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                      ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                          PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum), 1.0,
                                                                                        OutBaroPress,
                                                                                        RoutineNameOSCM)) +
                                                        RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                            HSkyFD(SurfNum) = OSCM(OPtr).HRad; // CR 8046, use sky term for surface to baffle IR
                            HGrndFD(SurfNum) = 0.0;            // CR 8046, null out and use only sky term for surface to baffle IR
                            HAirFD(SurfNum) = 0.0;             // CR 8046, null out and use only sky term for surface to baffle IR
                        }

                        // Call the outside surface temp calculation and pass the necessary terms
                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_EMPD) {

                            if (Surface(SurfNum).ExtCavityPresent) {
                                CalcExteriorVentedCavity(state, SurfNum);
                            }

                            CalcOutsideSurfTemp(SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                            if (MovInsulErrorFlag)
                                ShowFatalError("CalcOutsideSurfTemp: Program terminates due to preceding conditions.");

                        } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                   Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                            if (Surface(SurfNum).ExtCavityPresent) {
                                CalcExteriorVentedCavity(state, SurfNum);
                            }
                        }

                        // This ends the calculations for this surface and goes on to the next SurfNum
                    } else if (SELECT_CASE_var == ExternalEnvironment) {

                        // checking the EcoRoof presented in the external environment
                        // recompute each load by calling ecoroof

                        Real64 TempExt;

                        if (Surface(SurfNum).ExtEcoRoof) {
                            CalcEcoRoof(state, SurfNum, zoneNum, ConstrNum, TempExt);
                            continue;
                        }

                        if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;
                        // Roughness index of the exterior surface
                        int RoughSurf = dataMaterial.Material(
                                dataConstruction.Construct(ConstrNum).LayerPoint(1)).Roughness;
                        // Thermal absoptance of the exterior surface
                        Real64 AbsThermSurf = dataMaterial.Material(
                                dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;

                        // Check for outside movable insulation
                        if (Surface(SurfNum).MaterialMovInsulExt > 0) {
                            EvalOutsideMovableInsulation(SurfNum, HMovInsul, RoughSurf, AbsThermSurf);
                            if (HMovInsul > 0)
                                AbsThermSurf = dataMaterial.Material(
                                        Surface(SurfNum).MaterialMovInsulExt).AbsorpThermal; // Movable outside insulation present
                        }

                        // Check for exposure to wind (exterior environment)
                        if (Surface(SurfNum).ExtWind) {

                            // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
                            InitExteriorConvectionCoeff(state, SurfNum, HMovInsul,
                                                        RoughSurf, AbsThermSurf, TH(1, 1, SurfNum), HcExtSurf(SurfNum),
                                                        HSkyExtSurf(SurfNum), HGrdExtSurf(SurfNum),
                                                        HAirExtSurf(SurfNum));

                            if (IsRain) { // Raining: since wind exposed, outside surface gets wet

                                if (Surface(SurfNum).ExtConvCoeff <= 0) { // Reset HcExtSurf because of wetness
                                    HcExtSurf(SurfNum) = 1000.0;
                                } else { // User set
                                    HcExtSurf(SurfNum) = SetExtConvectionCoeff(state, SurfNum);
                                }

                                TempExt = Surface(SurfNum).OutWetBulbTemp;

                                // start HAMT
                                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                    // Set variables used in the HAMT moisture balance
                                    TempOutsideAirFD(SurfNum) = TempExt;
                                    RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRh(TempOutsideAirFD(SurfNum), 1.0,
                                                                             HBSurfManRainHAMT);
                                    HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                                    HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                            ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                    PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0, OutBaroPress,
                                                            RoutineNameExtEnvWetSurf)) +
                                                            RhoVaporAirOut(SurfNum)) *  PsyCpAirFnW(OutHumRat));
                                    HSkyFD(SurfNum) = HSkyExtSurf(SurfNum);
                                    HGrndFD(SurfNum) = HGrdExtSurf(SurfNum);
                                    HAirFD(SurfNum) = HAirExtSurf(SurfNum);
                                }
                                // end HAMT

                                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                                    // Set variables used in the FD moisture balance
                                    TempOutsideAirFD(SurfNum) = TempExt;
                                    RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(TempOutsideAirFD(SurfNum), 1.0);
                                    HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                                    HMassConvExtFD(SurfNum) =
                                            HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                            PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum), 1.0, OutBaroPress, RoutineNameExtEnvWetSurf)) +
                                            RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                                    HSkyFD(SurfNum) = HSkyExtSurf(SurfNum);
                                    HGrndFD(SurfNum) = HGrdExtSurf(SurfNum);
                                    HAirFD(SurfNum) = HAirExtSurf(SurfNum);
                                }

                            } else { // Surface is dry, use the normal correlation

                                TempExt = Surface(SurfNum).OutDryBulbTemp;

                                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                    Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                    // Set variables used in the FD moisture balance and HAMT
                                    TempOutsideAirFD(SurfNum) = TempExt;
                                    RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum), OutHumRat,
                                                                              OutBaroPress);
                                    HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                                    HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(OutBaroPress,
                                                                                                        TempOutsideAirFD(
                                                                                                                SurfNum),
                                                                                                        PsyWFnTdbRhPb(
                                                                                                                TempOutsideAirFD(
                                                                                                                        SurfNum),
                                                                                                                1.0,
                                                                                                                OutBaroPress,
                                                                                                                RoutineNameExtEnvDrySurf)) +
                                                                                      RhoVaporAirOut(SurfNum)) *
                                                                                     PsyCpAirFnW(OutHumRat));
                                    //  check for saturation conditions of air
                                    // Local temporary saturated vapor density for checking
                                    Real64 RhoVaporSat = PsyRhovFnTdbRh(TempOutsideAirFD(SurfNum), 1.0,
                                                                 HBSurfManDrySurfCondFD);
                                    if (RhoVaporAirOut(SurfNum) > RhoVaporSat) RhoVaporAirOut(SurfNum) = RhoVaporSat;
                                    HSkyFD(SurfNum) = HSkyExtSurf(SurfNum);
                                    HGrndFD(SurfNum) = HGrdExtSurf(SurfNum);
                                    HAirFD(SurfNum) = HAirExtSurf(SurfNum);
                                }
                            }

                        } else { // No wind

                            // Calculate exterior heat transfer coefficients for windspeed = 0
                            InitExteriorConvectionCoeff(state, SurfNum, HMovInsul,
                                                        RoughSurf, AbsThermSurf, TH(1, 1, SurfNum), HcExtSurf(SurfNum),
                                                        HSkyExtSurf(SurfNum), HGrdExtSurf(SurfNum),
                                                        HAirExtSurf(SurfNum));

                            TempExt = Surface(SurfNum).OutDryBulbTemp;

                            if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                // Set variables used in the FD moisture balance and HAMT
                                TempOutsideAirFD(SurfNum) = TempExt;
                                RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum), OutHumRat,
                                                                          OutBaroPress);
                                HConvExtFD(SurfNum) = HcExtSurf(SurfNum);
                                HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                          ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),
                                                                                            1.0, OutBaroPress,
                                                                                            RoutineNameNoWind)) +
                                                            RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                                HSkyFD(SurfNum) = HSkyExtSurf(SurfNum);
                                HGrndFD(SurfNum) = HGrdExtSurf(SurfNum);
                                HAirFD(SurfNum) = HAirExtSurf(SurfNum);
                            }
                        }
                        // Calculate LWR from surrounding surfaces if defined for an exterior surface
                        SurfQRadLWOutSrdSurfs(SurfNum) = 0;
                        if (Surface(SurfNum).HasSurroundingSurfProperties) {
                            int SrdSurfsNum = Surface(SurfNum).SurroundingSurfacesNum;
                            // Absolute temperature of the outside surface of an exterior surface
                            Real64 TSurf = TH(1, 1, SurfNum) + KelvinConv;
                            for (int SrdSurfNum = 1; SrdSurfNum <= SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
                                // View factor of a surrounding surface
                                Real64 SrdSurfViewFac = SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                                // Absolute temperature of a surrounding surface
                                Real64 SrdSurfTempAbs = GetCurrentScheduleValue(SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).TempSchNum) + KelvinConv;
                                SurfQRadLWOutSrdSurfs(SurfNum) += StefanBoltzmann * AbsThermSurf * SrdSurfViewFac * (pow_4(SrdSurfTempAbs) - pow_4(TSurf));
                            }
                        }

                        if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF ||
                            Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_EMPD) {

                            CalcOutsideSurfTemp(SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                            if (MovInsulErrorFlag)
                                ShowFatalError("CalcOutsideSurfTemp: Program terminates due to preceding conditions.");
                        }

                    } else if (SELECT_CASE_var == KivaFoundation) {
                        int RoughSurf = dataMaterial.Material(
                                dataConstruction.Construct(ConstrNum).LayerPoint(1)).Roughness;
                        Real64 AbsThermSurf = dataMaterial.Material(
                                dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;

                        // Set Kiva exterior convection algorithms
                        InitExteriorConvectionCoeff(state, SurfNum, HMovInsul, RoughSurf,
                                                    AbsThermSurf, TH(1, 1, SurfNum), HcExtSurf(SurfNum),
                                                    HSkyExtSurf(SurfNum), HGrdExtSurf(SurfNum), HAirExtSurf(SurfNum));

                    } else { // for interior or other zone surfaces

                        if (Surface(SurfNum).ExtBoundCond == SurfNum) { // Regular partition/internal mass

                            TH(1, 1, SurfNum) = TempSurfIn(SurfNum);

                            // No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

                            if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                // Set variables used in the FD moisture balance HAMT
                                TempOutsideAirFD(SurfNum) = TempSurfIn(SurfNum);
                                RhoVaporAirOut(SurfNum) = RhoVaporAirIn(SurfNum);
                                HConvExtFD(SurfNum) = HConvIn(SurfNum);
                                HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                          ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),
                                                                                            1.0, OutBaroPress,
                                                                                            RoutineNameOther)) +
                                                            RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                                HSkyFD(SurfNum) = 0.0;
                                HGrndFD(SurfNum) = 0.0;
                                HAirFD(SurfNum) = 0.0;
                            }

                        } else { // Interzone partition

                            TH(1, 1, SurfNum) = TH(2, 1, Surface(SurfNum).ExtBoundCond);

                            // No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

                            if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                // Set variables used in the FD moisture balance and HAMT
                                TempOutsideAirFD(SurfNum) = TH(2, 1, Surface(SurfNum).ExtBoundCond);
                                RhoVaporAirOut(SurfNum) = RhoVaporAirIn(Surface(SurfNum).ExtBoundCond);
                                HConvExtFD(SurfNum) = HConvIn(Surface(SurfNum).ExtBoundCond);
                                HMassConvExtFD(SurfNum) = HConvExtFD(SurfNum) /
                                                          ((PsyRhoAirFnPbTdbW(OutBaroPress, TempOutsideAirFD(SurfNum),
                                                                              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),
                                                                                            1.0, OutBaroPress,
                                                                                            RoutineNameIZPart)) +
                                                            RhoVaporAirOut(SurfNum)) * PsyCpAirFnW(OutHumRat));
                                HSkyFD(SurfNum) = 0.0;
                                HGrndFD(SurfNum) = 0.0;
                                HAirFD(SurfNum) = 0.0;
                            }
                        }

                        // This ends the calculations for this surface and goes on to the next SurfNum
                    }
                }

                // fill in reporting values for outside face

                QdotConvOutRepPerArea(SurfNum) = GetQdotConvOutRepPerArea(SurfNum);

                QdotConvOutRep(SurfNum) = QdotConvOutRepPerArea(SurfNum) * Surface(SurfNum).Area;

                QConvOutReport(SurfNum) = QdotConvOutRep(SurfNum) * TimeStepZoneSec;

                QHeatEmiReport(SurfNum) = QAirExtReport(SurfNum) - QdotConvOutRep(SurfNum);
            }
        } // ...end of DO loop over all surface (actually heat transfer surfaces)
    }

    Real64 GetQdotConvOutRepPerArea(int const SurfNum)
    {
        int OPtr = Surface(SurfNum).OSCMPtr;
        if (Surface(SurfNum).OSCMPtr > 0) { // Optr is set above in this case, use OSCM boundary data
            return -OSCM(OPtr).HConv * (TH(1, 1, SurfNum) - OSCM(OPtr).TConv);
        } else {
            if (IsRain) {
                return -HcExtSurf(SurfNum) * (TH(1, 1, SurfNum) - Surface(SurfNum).OutWetBulbTemp);
            } else {
                return -HcExtSurf(SurfNum) * (TH(1, 1, SurfNum) - Surface(SurfNum).OutDryBulbTemp);
            }
        }
    }

    void CalcHeatBalanceInsideSurf(EnergyPlusData &state,
                                   Optional_int_const ZoneToResimulate) // if passed in, then only calculate surfaces that have this zone
    {

        if (calcHeatBalInsideSurfFirstTime) {
            if (DataHeatBalance::AnyEMPD) {
                MinIterations = MinEMPDIterations;
            }
            if (DisplayAdvancedReportVariables) {
                SetupOutputVariable(state, "Surface Inside Face Heat Balance Calculation Iteration Count",
                                    OutputProcessor::Unit::None,
                                    InsideSurfIterations,
                                    "ZONE",
                                    "Sum",
                                    "Simulation");
            }
            // Precompute whether CTF temperature limits will be needed
            DataHeatBalSurface::Zone_has_mixed_HT_models.resize(NumOfZones + 1, false);
            for (int iZone = 1; iZone <= NumOfZones; ++iZone) {
                auto const &zone(Zone(iZone));
                for (int iSurf = zone.SurfaceFirst, eSurf = zone.SurfaceLast; iSurf <= eSurf; ++iSurf) {
                    auto const alg(Surface(iSurf).HeatTransferAlgorithm);
                    if ((alg == HeatTransferModel_CondFD) || (alg == HeatTransferModel_HAMT) || (alg == HeatTransferModel_Kiva)) {
                        DataHeatBalSurface::Zone_has_mixed_HT_models[iZone] = true;
                        break;
                    }
                }
            }
            calcHeatBalInsideSurfFirstTime = false;
        }

        if (BeginEnvrnFlag && calcHeatBalInsideSurEnvrnFlag) {
            TempInsOld = 23.0;
            RefAirTemp = 23.0;
            TempEffBulkAir = 23.0;
            calcHeatBalInsideSurfWarmupErrCount = 0;
            calcHeatBalInsideSurEnvrnFlag = false;

            // Initialize Kiva instances ground temperatures
            if (DataHeatBalance::AnyKiva) {
                SurfaceGeometry::kivaManager.initKivaInstances(state);
            }
        }
        if (!BeginEnvrnFlag) {
            calcHeatBalInsideSurEnvrnFlag = true;
        }

        // Pass correct list of surfaces to CalcHeatBalanceInsideSurf2
        bool const PartialResimulate(present(ZoneToResimulate));

        if (!PartialResimulate) {
            // Zero window heat gains for all zones
            ZoneWinHeatGain = 0.0;
            ZoneWinHeatGainRep = 0.0;
            ZoneWinHeatGainRepEnergy = 0.0;
            ZoneWinHeatLossRep = 0.0;
            ZoneWinHeatLossRepEnergy = 0.0;

            if (AllCTF) {
                CalcHeatBalanceInsideSurf2CTFOnly(state, 1, NumOfZones, DataSurfaces::AllIZSurfaceList);
            } else {
                CalcHeatBalanceInsideSurf2(state,
                                           DataSurfaces::AllHTSurfaceList,
                                           DataSurfaces::AllIZSurfaceList,
                                           DataSurfaces::AllHTNonWindowSurfaceList,
                                           DataSurfaces::AllHTWindowSurfaceList);
            }
            // Sort window heat gain/loss
            for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
                if (ZoneWinHeatGain(zoneNum) >= 0.0) {
                    ZoneWinHeatGainRep(zoneNum) = ZoneWinHeatGain(zoneNum);
                    ZoneWinHeatGainRepEnergy(zoneNum) = ZoneWinHeatGainRep(zoneNum) * DataGlobals::TimeStepZoneSec;
                } else {
                    ZoneWinHeatLossRep(zoneNum) = -ZoneWinHeatGain(zoneNum);
                    ZoneWinHeatLossRepEnergy(zoneNum) = ZoneWinHeatLossRep(zoneNum) * DataGlobals::TimeStepZoneSec;
                }
            }
        } else {
            // Zero window heat gains for resimulate zone
            ZoneWinHeatGain(ZoneToResimulate) = 0.0;
            ZoneWinHeatGainRep(ZoneToResimulate) = 0.0;
            ZoneWinHeatGainRepEnergy(ZoneToResimulate) = 0.0;
            ZoneWinHeatLossRep(ZoneToResimulate) = 0.0;
            ZoneWinHeatLossRepEnergy(ZoneToResimulate) = 0.0;

            auto const &zoneHTSurfList(Zone(ZoneToResimulate).ZoneHTSurfaceList);
            auto const &zoneIZSurfList(Zone(ZoneToResimulate).ZoneIZSurfaceList);
            auto const &zoneHTNonWindowSurfList(Zone(ZoneToResimulate).ZoneHTNonWindowSurfaceList);
            auto const &zoneHTWindowSurfList(Zone(ZoneToResimulate).ZoneHTWindowSurfaceList);
            // Cannot use CalcHeatBalanceInsideSurf2CTFOnly because resimulated zone includes adjacent interzone surfaces
            CalcHeatBalanceInsideSurf2(state, zoneHTSurfList, zoneIZSurfList, zoneHTNonWindowSurfList, zoneHTWindowSurfList, ZoneToResimulate);
            // Sort window heat gain/loss
            if (ZoneWinHeatGain(ZoneToResimulate) >= 0.0) {
                ZoneWinHeatGainRep(ZoneToResimulate) = ZoneWinHeatGain(ZoneToResimulate);
                ZoneWinHeatGainRepEnergy(ZoneToResimulate) = ZoneWinHeatGainRep(ZoneToResimulate) * DataGlobals::TimeStepZoneSec;
            } else {
                ZoneWinHeatLossRep(ZoneToResimulate) = -ZoneWinHeatGain(ZoneToResimulate);
                ZoneWinHeatLossRepEnergy(ZoneToResimulate) = ZoneWinHeatLossRep(ZoneToResimulate) * DataGlobals::TimeStepZoneSec;
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

        static std::string const rhoAirZone("RhoAirZone");
        static std::string const wsurf("Wsurf");
        static std::string const HBSurfManInsideSurf("HB,SurfMan:InsideSurf");
        static std::string const Inside("Inside");
        static std::string const BlankString;

        Real64 TempSurfOutTmp; // Local Temporary Surface temperature for the outside surface face
        Real64 TempSurfInSat;  // Local temporary surface dew point temperature

        Real64 Wsurf;         // Moisture ratio for HAMT
        Real64 RhoAirZone;    // Zone moisture density for HAMT
        int OtherSideZoneNum; // Zone Number index for other side of an interzone partition HAMT

        // determine reference air temperatures
        for (int SurfNum : HTSurfs) {
            int ZoneNum = Surface(SurfNum).Zone;

            // These conditions are not used in every SurfNum loop here so we don't use them to skip surfaces
            if (Surface(SurfNum).Class == SurfaceClass_TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

            {
                auto const SELECT_CASE_var(Surface(SurfNum).TAirRef);
                if (SELECT_CASE_var == ZoneMeanAirTemp) {
                    RefAirTemp(SurfNum) = MAT(ZoneNum);
                    TempEffBulkAir(SurfNum) = MAT(ZoneNum); // for reporting surf adjacent air temp
                } else if (SELECT_CASE_var == AdjacentAirTemp) {
                    RefAirTemp(SurfNum) = TempEffBulkAir(SurfNum);
                } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                    // determine ZoneEquipConfigNum for this zone
                    int ZoneEquipConfigNum = ZoneNum;
                    // check whether this zone is a controlled zone or not
                    if (!Zone(ZoneNum).IsControlled) {
                        ShowFatalError("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone(ZoneNum).Name);
                        return;
                    }
                    // determine supply air conditions
                    Real64 SumSysMCp = 0.0;
                    Real64 SumSysMCpT = 0.0;
                    Real64 const CpAir = Psychrometrics::PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                    for (int NodeNum = 1; NodeNum <= DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                        Real64 NodeTemp = DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                        Real64 MassFlowRate =
                            DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                        // Real64 CpAir2 = PsyCpAirFnW(ZoneAirHumRat(ZoneNum), NodeTemp);
                        SumSysMCp += MassFlowRate * CpAir;
                        SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                    }
                    // a weighted average of the inlet temperatures.
                    if (SumSysMCp > 0.0) {                            // protect div by zero
                        RefAirTemp(SurfNum) = SumSysMCpT / SumSysMCp; // BG changed 02-16-2005 to add index (SurfNum)
                    } else {
                        RefAirTemp(SurfNum) = MAT(ZoneNum);
                    }
                    TempEffBulkAir(SurfNum) = RefAirTemp(SurfNum); // for reporting surf adjacent air temp
                } else {
                    // currently set to mean air temp but should add error warning here
                    RefAirTemp(SurfNum) = MAT(ZoneNum);
                    TempEffBulkAir(SurfNum) = MAT(ZoneNum); // for reporting surf adjacent air temp
                }
            }
        }

        // Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
        // CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
        // CalcWindowHeatBalance.
        // Only for Surface(SurfNum).Class == DataSurfaces::SurfaceClass_Window
        for (int surfNum : HTWindowSurfs) {
            SurfWinHeatGain(surfNum) = 0.0;
            SurfWinHeatTransfer(surfNum) = 0.0;
            SurfWinHeatGainRep(surfNum) = 0.0;
            SurfWinHeatLossRep(surfNum) = 0.0;
            SurfWinGainConvGlazToZoneRep(surfNum) = 0.0;
            SurfWinGainIRGlazToZoneRep(surfNum) = 0.0;
            SurfWinLossSWZoneToOutWinRep(surfNum) = 0.0;
            SurfWinGainFrameDividerToZoneRep(surfNum) = 0.0;
            SurfWinGainConvGlazShadGapToZoneRep(surfNum) = 0.0;
            SurfWinGainConvShadeToZoneRep(surfNum) = 0.0;
            SurfWinOtherConvGainInsideFaceToZoneRep(surfNum) = 0.0;
            SurfWinGainIRShadeToZoneRep(surfNum) = 0.0;
            SurfWinFrameQRadOutAbs(surfNum) = 0.0;
            SurfWinFrameQRadInAbs(surfNum) = 0.0;
            SurfWinDividerQRadOutAbs(surfNum) = 0.0;
            SurfWinDividerQRadInAbs(surfNum) = 0.0;
        }

        InsideSurfIterations = 0;

        // Calculate heat extract due to additional heat flux source term as the surface boundary condition
        if (DataSurfaces::AnyHeatBalanceInsideSourceTerm) {
            for (int SurfNum : HTSurfs) {
                if (Surface(SurfNum).InsideHeatSourceTermSchedule) {
                    SurfQAdditionalHeatSourceInside(SurfNum) =
                        EnergyPlus::ScheduleManager::GetCurrentScheduleValue(Surface(SurfNum).InsideHeatSourceTermSchedule);
                }
            }
        }

        // Calculate Kiva instances
        if (DataHeatBalance::AnyKiva) {
            if (((SurfaceGeometry::kivaManager.settings.timestepType == HeatBalanceKivaManager::KivaManager::Settings::HOURLY && TimeStep == 1) ||
                 SurfaceGeometry::kivaManager.settings.timestepType == HeatBalanceKivaManager::KivaManager::Settings::TIMESTEP) &&
                !WarmupFlag) {
                SurfaceGeometry::kivaManager.calcKivaInstances();
            }
        }

        bool Converged = false; // .TRUE. if inside heat balance has converged
        while (!Converged) {    // Start of main inside heat balance DO loop...

            TempInsOld = TempSurfIn; // Keep track of last iteration's temperature values

            if (DataHeatBalance::AnyKiva) {
                for (auto &kivaSurf : SurfaceGeometry::kivaManager.surfaceMap) {
                    TempSurfIn(kivaSurf.first) = kivaSurf.second.results.Tavg - DataGlobals::KelvinConv; // TODO: Use average radiant temp? Trad?
                }
            }

            HeatBalanceIntRadExchange::CalcInteriorRadExchange(
                TempSurfIn, InsideSurfIterations, SurfNetLWRadToSurf, ZoneToResimulate, Inside); // Update the radiation balance

            if (DataHeatBalance::AnyKiva) {
                for (auto &kivaSurf : SurfaceGeometry::kivaManager.surfaceMap) {
                    TempSurfIn(kivaSurf.first) = TempInsOld(kivaSurf.first);
                }
            }

            // Every 30 iterations, recalculate the inside convection coefficients in case
            // there has been a significant drift in the surface temperatures predicted.
            // This is not fool-proof and it basically means that the outside surface
            // heat balance is in error (potentially) once HConvIn is re-evaluated.
            // The choice of 30 is not significant--just want to do this a couple of
            // times before the iteration limit is hit.
            if ((InsideSurfIterations > 0) && (mod(InsideSurfIterations, ItersReevalConvCoeff) == 0)) {
                ConvectionCoefficients::InitInteriorConvectionCoeffs(state, TempSurfIn, ZoneToResimulate);
            }

            if (DataHeatBalance::AnyEMPD || DataHeatBalance::AnyHAMT) {
                for (int SurfNum : HTSurfs) {
                    auto &surface(Surface(SurfNum));
                    if (surface.Class == SurfaceClass_TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

                    // Calculate the inside surface moisture quantities
                    // calculate the inside surface moisture transfer conditions
                    // check for saturation conditions of air
                    if ((surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) || (surface.HeatTransferAlgorithm == HeatTransferModel_HAMT)) {
                        int ZoneNum = Surface(SurfNum).Zone;
                        Real64 const MAT_zone(MAT(ZoneNum));
                        Real64 const ZoneAirHumRat_zone(max(ZoneAirHumRat(ZoneNum), 1.0e-5));
                        Real64 const HConvIn_surf(HConvInFD(SurfNum) = HConvIn(SurfNum));

                        RhoVaporAirIn(SurfNum) = min(Psychrometrics::PsyRhovFnTdbWPb_fast(MAT_zone, ZoneAirHumRat_zone, OutBaroPress),
                                                     Psychrometrics::PsyRhovFnTdbRh(MAT_zone, 1.0, HBSurfManInsideSurf));
                        HMassConvInFD(SurfNum) = HConvIn_surf / (Psychrometrics::PsyRhoAirFnPbTdbW_fast(OutBaroPress, MAT_zone, ZoneAirHumRat_zone) *
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
                Real64 &TH11(TH(1, 1, SurfNum));
                int const ConstrNum = surface.Construction;
                auto const &construct(dataConstruction.Construct(ConstrNum));
                Real64 const MAT_zone(MAT(ZoneNum));
                Real64 const HConvIn_surf(HConvInFD(SurfNum) = HConvIn(SurfNum));

                if (surface.ExtBoundCond == SurfNum) {
                    // CR6869 -- let Window HB take care of it      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
                    // Surface is adiabatic
                    if (surface.HeatTransferAlgorithm == HeatTransferModel_CTF ||
                        surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) { // Regular CTF Surface and/or EMPD surface

                        if (surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                            MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(state, SurfNum, TempSurfInTmp(SurfNum), MAT_zone, TempSurfInSat);
                        }
                        // Pre-calculate a few terms
                        //
                        Real64 const TempTerm(CTFConstInPart(SurfNum) + SurfQRadThermInAbs(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) +
                                              SurfQAdditionalHeatSourceInside(SurfNum) + HConvIn_surf * RefAirTemp(SurfNum) + QHTRadSysSurf(SurfNum) +
                                              QCoolingPanelSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) +
                                              QElecBaseboardSurf(SurfNum) + SurfNetLWRadToSurf(SurfNum) + (QRadSurfAFNDuct(SurfNum) / TimeStepZoneSec));
                        Real64 const TempDiv(1.0 / (construct.CTFInside(0) - construct.CTFCross(0) + HConvIn_surf + IterDampConst));
                        // Calculate the current inside surface temperature
                        if ((!surface.IsPool) || ((surface.IsPool) && (std::abs(QPoolSurfNumerator(SurfNum)) < PoolIsOperatingLimit) &&
                                                  (std::abs(PoolHeatTransCoefs(SurfNum)) < PoolIsOperatingLimit))) {
                            if (construct.SourceSinkPresent) {
                                TempSurfInTmp(SurfNum) =
                                    (TempTerm + construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) + IterDampConst * TempInsOld(SurfNum)) *
                                    TempDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation
                                             // from internal sources | Convection from surface to zone air | Net radiant exchange with other zone
                                             // surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from a
                                             // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a
                                             // steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for
                                             // stability) | Conduction term (both partition sides same temp) | Conduction term (both partition sides
                                             // same temp) | Convection and damping term | Radiation from AFN ducts
                            } else {
                                TempSurfInTmp(SurfNum) =
                                    (TempTerm + IterDampConst * TempInsOld(SurfNum)) *
                                    TempDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation
                                             // from internal sources | Convection from surface to zone air | Net radiant exchange with other zone
                                             // surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from a
                                             // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a
                                             // steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for
                                             // stability) | Conduction term (both partition sides same temp) | Conduction term (both partition sides
                                             // same temp) | Convection and damping term | Radiation from AFN ducts
                            }
                        } else { // this is a pool and it has been simulated this time step
                            TempSurfInTmp(SurfNum) = (CTFConstInPart(SurfNum) + QPoolSurfNumerator(SurfNum) + IterDampConst * TempInsOld(SurfNum)) /
                                                     (construct.CTFInside(0) - construct.CTFCross(0) + PoolHeatTransCoefs(SurfNum) +
                                                      IterDampConst); // Constant part of conduction eq (history terms) | Pool modified terms (see
                                                                      // non-pool equation for details) | Iterative damping term (for stability) |
                                                                      // Conduction term (both partition sides same temp) | Pool and damping term
                        }
                        if (surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                            TempSurfInTmp(SurfNum) -=
                                DataMoistureBalanceEMPD::HeatFluxLatent(SurfNum) * TempDiv; // Conduction term (both partition sides same temp) |
                                                                                            // Conduction term (both partition sides same temp) |
                                                                                            // Convection and damping term
                            if (TempSurfInSat > TempSurfInTmp(SurfNum)) {
                                TempSurfInTmp(SurfNum) = TempSurfInSat; // Surface temp cannot be below dew point
                            }
                        }
                        // if any mixed heat transfer models in zone, apply limits to CTF result
                        if (DataHeatBalSurface::Zone_has_mixed_HT_models[ZoneNum])
                            TempSurfInTmp(SurfNum) =
                                max(MinSurfaceTempLimit,
                                    min(MaxSurfaceTempLimit, TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop

                        if (construct.SourceSinkPresent) { // Set the appropriate parameters for the radiant system

                            // Radiant system does not need the damping coefficient terms (hopefully) // Partitions are assumed to be symmetric
                            Real64 const RadSysDiv(1.0 / (construct.CTFInside(0) - construct.CTFCross(0) + HConvIn_surf));
                            RadSysToHBConstCoef(SurfNum) = RadSysTiHBConstCoef(SurfNum) =
                                TempTerm * RadSysDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW
                                                      // radiation from internal sources | Convection from surface to zone air | Radiant flux from
                                                      // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant
                                                      // flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Net
                                                      // radiant exchange with other zone surfaces | Cond term (both partition sides same temp) | Cond
                                                      // term (both partition sides same temp) | Convection and damping term
                            RadSysToHBTinCoef(SurfNum) = RadSysTiHBToutCoef(SurfNum) =
                                0.0; // The outside temp is assumed to be equal to the inside temp for a partition
                            RadSysToHBQsrcCoef(SurfNum) = RadSysTiHBQsrcCoef(SurfNum) =
                                construct.CTFSourceIn(0) * RadSysDiv; // QTF term for the source | Cond term (both partition sides same temp) | Cond
                                                                      // term (both partition sides same temp) | Convection and damping term
                        }

                    } else if (surface.HeatTransferAlgorithm == HeatTransferModel_CondFD || surface.HeatTransferAlgorithm == HeatTransferModel_HAMT) {

                        if (surface.HeatTransferAlgorithm == HeatTransferModel_HAMT)
                            HeatBalanceHAMTManager::ManageHeatBalHAMT(state, SurfNum, TempSurfInTmp(SurfNum), TempSurfOutTmp); // HAMT

                        if (surface.HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                            HeatBalFiniteDiffManager::ManageHeatBalFiniteDiff(state, SurfNum, TempSurfInTmp(SurfNum), TempSurfOutTmp);
                        }

                        TH11 = TempSurfOutTmp;
                    }

                    TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum);

                } else { // Standard surface or interzone surface

                    Real64 HMovInsul = 0.0; // "Convection" coefficient of movable insulation
                    if (surface.MaterialMovInsulInt > 0) {
                        Real64 AbsInt = 0.0; // Solar absorptance of inside movable insulation (not used here)
                        HeatBalanceMovableInsulation::EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
                    }

                    if (HMovInsul <= 0.0) { // No movable insulation present, normal heat balance equation

                        if (surface.HeatTransferAlgorithm == HeatTransferModel_CTF ||
                            surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) { // Regular CTF Surface and/or EMPD surface

                            if (surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                                MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(state, SurfNum, TempSurfInTmp(SurfNum), MAT_zone, TempSurfInSat);
                            }
                            // Pre-calculate a few terms
                            Real64 const TempTerm(CTFConstInPart(SurfNum) + SurfQRadThermInAbs(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) +
                                                  SurfQAdditionalHeatSourceInside(SurfNum) + HConvIn_surf * RefAirTemp(SurfNum) + QHTRadSysSurf(SurfNum) +
                                                  QCoolingPanelSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) +
                                                  QElecBaseboardSurf(SurfNum) + SurfNetLWRadToSurf(SurfNum) +
                                                  (QRadSurfAFNDuct(SurfNum) / TimeStepZoneSec));
                            Real64 const TempDiv(1.0 / (construct.CTFInside(0) + HConvIn_surf + IterDampConst));
                            // Calculate the current inside surface temperature
                            if ((!surface.IsPool) || ((surface.IsPool) && (std::abs(QPoolSurfNumerator(SurfNum)) < PoolIsOperatingLimit) &&
                                                      (std::abs(PoolHeatTransCoefs(SurfNum)) < PoolIsOperatingLimit))) {
                                if (construct.SourceSinkPresent) {
                                    TempSurfInTmp(SurfNum) =
                                        (TempTerm + construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) + IterDampConst * TempInsOld(SurfNum) +
                                         construct.CTFCross(0) * TH11) *
                                        TempDiv; // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW
                                                 // radiation from internal sources | Convection from surface to zone air | Net radiant exchange
                                                 // with other zone surfaces | Heat source/sink term for radiant systems | (if there is one
                                                 // present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water
                                                 // baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric
                                                 // baseboard heater | Iterative damping term (for stability) | Current conduction from | the
                                                 // outside surface | Coefficient for conduction (current time) | Convection and damping term |
                                                 // Radiation from AFN ducts
                                } else {
                                    TempSurfInTmp(SurfNum) =
                                        (TempTerm + IterDampConst * TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) *
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
                                TempSurfInTmp(SurfNum) = (CTFConstInPart(SurfNum) + QPoolSurfNumerator(SurfNum) +
                                                          IterDampConst * TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) /
                                                         (construct.CTFInside(0) + PoolHeatTransCoefs(SurfNum) +
                                                          IterDampConst); // Constant part of conduction eq (history terms) | Pool modified terms
                                                                          // (see non-pool equation for details) | Iterative damping term (for
                                                                          // stability) | Current conduction from | the outside surface |
                                                                          // Coefficient for conduction (current time) | Pool and damping term
                            }
                            if (surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                                TempSurfInTmp(SurfNum) -= DataMoistureBalanceEMPD::HeatFluxLatent(SurfNum) *
                                                          TempDiv; // Coefficient for conduction (current time) | Convection and damping term
                                if (TempSurfInSat > TempSurfInTmp(SurfNum)) {
                                    TempSurfInTmp(SurfNum) = TempSurfInSat; // Surface temp cannot be below dew point
                                }
                            }
                            // if any mixed heat transfer models in zone, apply limits to CTF result
                            if (DataHeatBalSurface::Zone_has_mixed_HT_models[ZoneNum])
                                TempSurfInTmp(SurfNum) =
                                    max(MinSurfaceTempLimit,
                                        min(MaxSurfaceTempLimit,
                                            TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop

                            if (construct.SourceSinkPresent) { // Set the appropriate parameters for the radiant system

                                // Radiant system does not need the damping coefficient terms (hopefully)
                                Real64 const RadSysDiv(1.0 / (construct.CTFInside(0) + HConvIn_surf));
                                RadSysTiHBConstCoef(SurfNum) =
                                    TempTerm * RadSysDiv; // Constant portion of cond eq (history terms) | LW radiation from internal sources | SW
                                                          // radiation from internal sources | Convection from surface to zone air | Radiant flux
                                                          // from high temp radiant heater | Radiant flux from a hot water baseboard heater |
                                                          // Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard
                                                          // heater | Net radiant exchange with other zone surfaces | Cond term (both partition
                                                          // sides same temp) | Convection and damping term
                                RadSysTiHBToutCoef(SurfNum) = construct.CTFCross(0) * RadSysDiv;    // Outside temp=inside temp for a partition |
                                                                                                    // Cond term (both partition sides same temp) |
                                                                                                    // Convection and damping term
                                RadSysTiHBQsrcCoef(SurfNum) = construct.CTFSourceIn(0) * RadSysDiv; // QTF term for the source | Cond term (both
                                                                                                    // partition sides same temp) | Convection and
                                                                                                    // damping term

                                if (surface.ExtBoundCond > 0) { // This is an interzone partition and we need to set outside params
                                    // The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
                                    // the inside coefficients are set up once the heat balance equation for that side has been calculated.
                                    // For both sides to actually have been set, we have to wait until we get to the second side in the surface
                                    // derived type.  At that point, both inside coefficient sets have been evaluated.
                                    if (surface.ExtBoundCond < SurfNum) { // Both of the inside coefficients have now been set
                                        int OtherSideSurfNum = surface.ExtBoundCond;
                                        RadSysToHBConstCoef(OtherSideSurfNum) = RadSysTiHBConstCoef(SurfNum);
                                        RadSysToHBTinCoef(OtherSideSurfNum) = RadSysTiHBToutCoef(SurfNum);
                                        RadSysToHBQsrcCoef(OtherSideSurfNum) = RadSysTiHBQsrcCoef(SurfNum);
                                        RadSysToHBConstCoef(SurfNum) = RadSysTiHBConstCoef(OtherSideSurfNum);
                                        RadSysToHBTinCoef(SurfNum) = RadSysTiHBToutCoef(OtherSideSurfNum);
                                        RadSysToHBQsrcCoef(SurfNum) = RadSysTiHBQsrcCoef(OtherSideSurfNum);
                                    }
                                }
                            }

                        } else if (surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ||
                                   surface.HeatTransferAlgorithm == HeatTransferModel_HAMT) {

                            if (surface.HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                                if (surface.ExtBoundCond > 0) {
                                    // HAMT get the correct other side zone zone air temperature --
                                    int OtherSideSurfNum = surface.ExtBoundCond;
                                    // ZoneNum = surface.Zone;
                                    OtherSideZoneNum = Surface(OtherSideSurfNum).Zone;
                                    TempOutsideAirFD(SurfNum) = MAT(OtherSideZoneNum);
                                }
                                HeatBalanceHAMTManager::ManageHeatBalHAMT(state, SurfNum, TempSurfInTmp(SurfNum), TempSurfOutTmp);
                            }

                            if (surface.HeatTransferAlgorithm == HeatTransferModel_CondFD)
                                HeatBalFiniteDiffManager::ManageHeatBalFiniteDiff(state, SurfNum, TempSurfInTmp(SurfNum), TempSurfOutTmp);

                            TH11 = TempSurfOutTmp;

                        } else if (surface.HeatTransferAlgorithm == HeatTransferModel_Kiva) {
                            // Read Kiva results for each surface
                            TempSurfInTmp(SurfNum) = SurfaceGeometry::kivaManager.surfaceMap[SurfNum].results.Tconv - DataGlobals::KelvinConv;
                            SurfOpaqInsFaceConductionFlux(SurfNum) = SurfaceGeometry::kivaManager.surfaceMap[SurfNum].results.qtot;
                            SurfOpaqInsFaceConduction(SurfNum) = SurfOpaqInsFaceConductionFlux(SurfNum) * DataSurfaces::Surface(SurfNum).Area;

                            TH11 = 0.0;
                        }

                        TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum);

                    } else { // Movable insulation present

                        if (construct.SourceSinkPresent) {

                            ShowSevereError("Interior movable insulation is not valid with embedded sources/sinks");
                            ShowContinueError("Construction " + construct.Name + " contains an internal source or sink but also uses");
                            ShowContinueError("interior movable insulation " + dataMaterial.Material(Surface(SurfNum).MaterialMovInsulInt).Name +
                                              " for a surface with that construction.");
                            ShowContinueError("This is not currently allowed because the heat balance equations do not currently accommodate "
                                              "this combination.");
                            ShowFatalError("CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
                        }

                        Real64 F1 = HMovInsul / (HMovInsul + HConvIn_surf + IterDampConst);

                        TempSurfIn(SurfNum) =
                            (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + construct.CTFCross(0) * TH11 +
                             F1 * (SurfQRadThermInAbs(SurfNum) + HConvIn_surf * RefAirTemp(SurfNum) + SurfNetLWRadToSurf(SurfNum) + QHTRadSysSurf(SurfNum) +
                                   QCoolingPanelSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) +
                                   QElecBaseboardSurf(SurfNum) + SurfQAdditionalHeatSourceInside(SurfNum) + IterDampConst * TempInsOld(SurfNum))) /
                            (construct.CTFInside(0) + HMovInsul - F1 * HMovInsul); // Convection from surface to zone air

                        TempSurfInTmp(SurfNum) = (construct.CTFInside(0) * TempSurfIn(SurfNum) + HMovInsul * TempSurfIn(SurfNum) -
                                SurfOpaqQRadSWInAbs(SurfNum) - CTFConstInPart(SurfNum) - construct.CTFCross(0) * TH11) /
                                (HMovInsul);
                        // if any mixed heat transfer models in zone, apply limits to CTF result
                        if (DataHeatBalSurface::Zone_has_mixed_HT_models[ZoneNum])
                            TempSurfInTmp(SurfNum) =
                                max(MinSurfaceTempLimit,
                                    min(MaxSurfaceTempLimit, TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop
                    }
                }
            }
            for (int SurfNum : HTWindowSurfs) {
                auto &surface(Surface(SurfNum));
                Real64 &TH11(TH(1, 1, SurfNum));
                int ConstrNum = surface.Construction; // Not const, because storm window may change this
                auto const &construct(dataConstruction.Construct(ConstrNum));
                if (SurfWinOriginalClass(SurfNum) == SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                    // Lookup up the TDD:DOME object
                    int const pipeNum = SurfWinTDDPipeNum(SurfNum);
                    int const domeNum = DataDaylightingDevices::TDDPipe(pipeNum).Dome;
                    // Ueff = 1 / effective R value between TDD:DOME and TDD:DIFFUSER
                    Real64 Ueff = 1.0 / DataDaylightingDevices::TDDPipe(pipeNum).Reff;

                    // Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
                    // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
                    //   = QRadSWwinAbs(SurfNum,1)/2.0
                    Real64 const HConvIn_surf(HConvInFD(SurfNum) = HConvIn(SurfNum));
                    TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum) =
                        (SurfQRadThermInAbs(SurfNum) + SurfWinQRadSWwinAbs(1, SurfNum) / 2.0 + SurfQAdditionalHeatSourceInside(SurfNum) +
                         HConvIn_surf * RefAirTemp(SurfNum) + SurfNetLWRadToSurf(SurfNum) + IterDampConst * TempInsOld(SurfNum) +
                         Ueff * TH(1, 1, domeNum)) /
                        (Ueff + HConvIn_surf + IterDampConst); // LW radiation from internal sources | SW radiation from internal sources and
                                                               // solar | Convection from surface to zone air | Net radiant exchange with
                                                               // other zone surfaces | Iterative damping term (for stability) | Current
                                                               // conduction from the outside surface | Coefficient for conduction (current
                                                               // time) | Convection and damping term

                    Real64 const Sigma_Temp_4(DataGlobals::StefanBoltzmann * pow_4(TempSurfIn(SurfNum)));

                    // Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
                    SurfWinHeatGain(SurfNum) =
                            SurfWinTransSolar(SurfNum) + HConvIn_surf * surface.Area * (TempSurfIn(SurfNum) - RefAirTemp(SurfNum)) +
                            dataConstruction.Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                            (Sigma_Temp_4 - (SurfWinIRfromParentZone(SurfNum) + QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) +
                                             QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum))) -
                                             QS(surface.SolarEnclIndex) * surface.Area * dataConstruction.Construct(surface.Construction).TransDiff;
                    // Transmitted solar | Convection | IR exchange | IR
                    // Zone diffuse interior shortwave reflected back into the TDD
                    SurfWinHeatTransfer(SurfNum) = SurfWinHeatGain(SurfNum);

                    // fill out report vars for components of Window Heat Gain
                    SurfWinGainConvGlazToZoneRep(SurfNum) = HConvIn_surf * surface.Area * (TempSurfIn(SurfNum) - RefAirTemp(SurfNum));
                    SurfWinGainIRGlazToZoneRep(SurfNum) =
                        dataConstruction.Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                        (Sigma_Temp_4 - (SurfWinIRfromParentZone(SurfNum) + QHTRadSysSurf(SurfNum) + QCoolingPanelSurf(SurfNum) +
                                         QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)));
                    SurfWinLossSWZoneToOutWinRep(SurfNum) =
                        QS(surface.SolarEnclIndex) * surface.Area * dataConstruction.Construct(surface.Construction).TransDiff;
                } else {                             // Regular window
                    if (InsideSurfIterations == 0) { // Do windows only once
                        if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = surface.StormWinConstruction;
                        // Get outside convection coeff for exterior window here to avoid calling
                        // InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
                        // (HeatBalanceSurfaceManager USEing and WindowManager and
                        // WindowManager USEing HeatBalanceSurfaceManager)
                        if (surface.ExtBoundCond == ExternalEnvironment) {
                            int RoughSurf = dataMaterial.Material(construct.LayerPoint(1)).Roughness;           // Outside surface roughness
                            Real64 EmisOut = dataMaterial.Material(construct.LayerPoint(1)).AbsorpThermalFront; // Glass outside surface emissivity
                            auto const shading_flag(SurfWinShadingFlag(SurfNum));
                            if (shading_flag == ExtShadeOn || shading_flag == ExtBlindOn || shading_flag == ExtScreenOn) {
                                // Exterior shade in place
                                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                if (ConstrNumSh != 0) {
                                    RoughSurf = dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).Roughness;
                                    EmisOut = dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).AbsorpThermal;
                                }
                            }

                            // Get the outside effective emissivity for Equivalent layer model
                            if (construct.WindowTypeEQL) {
                                EmisOut = WindowEquivalentLayer::EQLWindowOutsideEffectiveEmiss(ConstrNum);
                            }
                            // Set Exterior Convection Coefficient...
                            if (surface.ExtConvCoeff > 0) {

                                HcExtSurf(SurfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, SurfNum);

                            } else if (surface.ExtWind) { // Window is exposed to wind (and possibly rain)

                                // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in
                                // subroutine)
                                ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                    SurfNum,
                                                                                    0.0,
                                                                                    RoughSurf,
                                                                                    EmisOut,
                                                                                    TH11,
                                                                                    HcExtSurf(SurfNum),
                                                                                    HSkyExtSurf(SurfNum),
                                                                                    HGrdExtSurf(SurfNum),
                                                                                    HAirExtSurf(SurfNum));

                                if (IsRain) {                    // Raining: since wind exposed, outside window surface gets wet
                                    HcExtSurf(SurfNum) = 1000.0; // Reset HcExtSurf because of wetness
                                }

                            } else { // Not Wind exposed

                                // Calculate exterior heat transfer coefficients for windspeed = 0
                                ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                    SurfNum,
                                                                                    0.0,
                                                                                    RoughSurf,
                                                                                    EmisOut,
                                                                                    TH11,
                                                                                    HcExtSurf(SurfNum),
                                                                                    HSkyExtSurf(SurfNum),
                                                                                    HGrdExtSurf(SurfNum),
                                                                                    HAirExtSurf(SurfNum));
                            }
                        } else { // Interior Surface

                            if (surface.ExtConvCoeff > 0) {
                                HcExtSurf(SurfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, SurfNum);
                            } else {
                                // Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of
                                // same
                                HcExtSurf(SurfNum) = HConvIn(surface.ExtBoundCond);
                            }
                        }

                        // Following call determines inside surface temperature of glazing, and of
                        // frame and/or divider, if present
                        CalcWindowHeatBalance(state,
                                              SurfNum,
                                              HcExtSurf(SurfNum),
                                              TempSurfInTmp(SurfNum),
                                              TH11);

                        TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum);
                    }
                }
            } // ...end of inside surface heat balance equation selection

            for (int SurfNum : HTSurfs) {
                int const ZoneNum = Surface(SurfNum).Zone;
                auto &surface(Surface(SurfNum));
                auto &zone(Zone(ZoneNum));
                Real64 &TH11(TH(1, 1, SurfNum));
                Real64 &TH12(TH(2, 1, SurfNum));
                TH12 = TempSurfInRep(SurfNum) = TempSurfIn(SurfNum);
                SurfTempSurfOut(SurfNum) = TH11; // For reporting

                if (SurfWinOriginalClass(SurfNum) == SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                    // Tubular daylighting devices are treated as one big object with an effective R value.
                    // The outside face temperature of the TDD:DOME and the inside face temperature of the
                    // TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
                    // Below, the resulting temperatures are copied to the inside face of the TDD:DOME
                    // and the outside face of the TDD:DIFFUSER for reporting.

                    // Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
                    int domeNum = DataDaylightingDevices::TDDPipe(SurfWinTDDPipeNum(SurfNum)).Dome;
                    TH(2, 1, domeNum) = TempSurfIn(domeNum) = TempSurfInTmp(domeNum) = TempSurfInRep(domeNum) = TempSurfIn(SurfNum);

                    // Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
                    // Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
                    TH11 = SurfTempSurfOut(SurfNum) = SurfTempSurfOut(domeNum) = TH(1, 1, domeNum);
                }

                if ((TH12 > MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
                    TestSurfTempCalcHeatBalanceInsideSurf(TH12, surface, zone, calcHeatBalInsideSurfWarmupErrCount);
                }

            } // ...end of main loops over all surfaces for inside heat balances

            // Interzone surface updating: interzone surfaces have other side temperatures
            // which can vary as the simulation iterates through the inside heat
            // balance.  This block is intended to "lock" the opposite side (outside)
            // temperatures to the correct value, namely the value calculated by the
            // inside surface heat balance for the other side.
            assert(TH.index(1, 1, 1) == 0u); // Assumed for linear indexing below
            auto const l211(TH.index(2, 1, 1) - 1);
            for (int SurfNum : IZSurfs) {
                int const surfExtBoundCond(Surface(SurfNum).ExtBoundCond);
                // Set the outside surface temperature to the inside surface temperature of the interzone pair.
                // By going through all of the surfaces, this should pick up the other side as well as affect the next iteration.
                // [ SurfNum - 1 ] == ( 1, 1, SurfNum )
                // [ l211 + surfExtBoundCond ] == ( 2, 1, surfExtBoundCond )
                SurfTempSurfOut(SurfNum) = TH[SurfNum - 1] = TH[l211 + surfExtBoundCond];
            }

            ++InsideSurfIterations;

            // Convergence check - Loop through all relevant non-window surfaces to check for convergence...
            Real64 MaxDelTemp = 0.0; // Maximum change in surface temperature for any opaque surface from one iteration to the next
            for (int SurfNum : HTNonWindowSurfs) {
                MaxDelTemp = max(std::abs(TempSurfIn(SurfNum) - TempInsOld(SurfNum)), MaxDelTemp);
                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                    // also check all internal nodes as well as surface faces
                    MaxDelTemp = max(MaxDelTemp, HeatBalFiniteDiffManager::SurfaceFD(SurfNum).MaxNodeDelTemp);
                }
            } // ...end of loop to check for convergence

            if (!DataHeatBalance::AnyCondFD) {
                if (MaxDelTemp <= MaxAllowedDelTemp) Converged = true;
            } else {
                if (MaxDelTemp <= MaxAllowedDelTempCondFD) Converged = true;

                // resets relaxation factor to speed up iterations when under-relaxation is not needed.
                if (InsideSurfIterations <= 1) {
                    CondFDRelaxFactor = CondFDRelaxFactorInput;
                }
                if ((InsideSurfIterations > IterationsForCondFDRelaxChange) && !Converged) {
                    // adjust relaxation factor down, assume large number of iterations is result of instability
                    CondFDRelaxFactor *= 0.9;
                    if (CondFDRelaxFactor < 0.1) CondFDRelaxFactor = 0.1;
                }
            }

#ifdef EP_Count_Calls
            NumMaxInsideSurfIterations = max(NumMaxInsideSurfIterations, InsideSurfIterations);
#endif

            if (InsideSurfIterations < MinIterations) Converged = false;

            if (InsideSurfIterations > MaxIterations) {
                if (!WarmupFlag) {
                    ++calcHeatBalInsideSurfErrCount;
                    if (calcHeatBalInsideSurfErrCount < 16) {
                        if (!DataHeatBalance::AnyCondFD) {
                            ShowWarningError("Inside surface heat balance did not converge with Max Temp Difference [C] =" +
                                             General::RoundSigDigits(MaxDelTemp, 3) +
                                             " vs Max Allowed Temp Diff [C] =" + General::RoundSigDigits(MaxAllowedDelTemp, 3));
                            ShowContinueErrorTimeStamp("");
                        } else {
                            ShowWarningError("Inside surface heat balance did not converge with Max Temp Difference [C] =" +
                                             General::RoundSigDigits(MaxDelTemp, 3) +
                                             " vs Max Allowed Temp Diff [C] =" + General::RoundSigDigits(MaxAllowedDelTempCondFD, 6));
                            ShowContinueErrorTimeStamp("");
                        }
                    } else {
                        ShowRecurringWarningErrorAtEnd("Inside surface heat balance convergence problem continues",
                                                       calcHeatBalInsideSurfErrPointer,
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
            if (Surface(surfNum).Class == SurfaceClass_TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

            // Inside Face Convection - sign convention is positive means energy going into inside face from the air.
            auto const HConvInTemp_fac(-HConvIn(surfNum) * (TempSurfIn(surfNum) - RefAirTemp(surfNum)));
            QdotConvInRep(surfNum) = Surface(surfNum).Area * HConvInTemp_fac;
            QdotConvInRepPerArea(surfNum) = HConvInTemp_fac;
            QConvInReport(surfNum) = QdotConvInRep(surfNum) * TimeStepZoneSec;

            // The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
            // sizing for both the normal and pulse cases so that load components can be derived later.
            if (ZoneSizingCalc && CompLoadReportIsReq) {
                if (!WarmupFlag) {
                    int TimeStepInDay = (HourOfDay - 1) * NumOfTimeStepInHour + TimeStep;
                    if (isPulseZoneSizing) {
                        OutputReportTabular::loadConvectedWithPulse(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) = QdotConvInRep(surfNum);
                    } else {
                        OutputReportTabular::loadConvectedNormal(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) = QdotConvInRep(surfNum);
                        OutputReportTabular::netSurfRadSeq(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) =
                                SurfNetLWRadToSurf(surfNum) * Surface(surfNum).Area;
                    }
                }
            }

            // Window heat gain/loss
            if (DataSurfaces::Surface(surfNum).Class == DataSurfaces::SurfaceClass_Window) {
                if (DataSurfaces::SurfWinHeatGain(surfNum) >= 0.0) {
                    DataSurfaces::SurfWinHeatGainRep(surfNum) = DataSurfaces::SurfWinHeatGain(surfNum);
                    DataSurfaces::SurfWinHeatGainRepEnergy(surfNum) = DataSurfaces::SurfWinHeatGainRep(surfNum) * DataGlobals::TimeStepZoneSec;
                } else {
                    DataSurfaces::SurfWinHeatLossRep(surfNum) = -DataSurfaces::SurfWinHeatGain(surfNum);
                    DataSurfaces::SurfWinHeatLossRepEnergy(surfNum) = DataSurfaces::SurfWinHeatLossRep(surfNum) * DataGlobals::TimeStepZoneSec;
                }

                DataSurfaces::SurfWinHeatTransferRepEnergy(surfNum) = DataSurfaces::SurfWinHeatGain(surfNum) * DataGlobals::TimeStepZoneSec;
                if (DataSurfaces::SurfWinOriginalClass(surfNum) == DataSurfaces::SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                    int pipeNum = DataSurfaces::SurfWinTDDPipeNum(surfNum);
                    DataDaylightingDevices::TDDPipe(pipeNum).HeatGain = DataSurfaces::SurfWinHeatGainRep(surfNum);
                    DataDaylightingDevices::TDDPipe(pipeNum).HeatLoss = DataSurfaces::SurfWinHeatLossRep(surfNum);
                }
                if (DataSurfaces::Surface(surfNum).ExtSolar) { // WindowManager's definition of ZoneWinHeatGain/Loss
                    int zoneNum = DataSurfaces::Surface(surfNum).Zone;
                    DataHeatBalance::ZoneWinHeatGain(zoneNum) += DataSurfaces::SurfWinHeatGain(surfNum);
                }
            }
        }

        // Update SumHmXXXX for non-window EMPD or HAMT surfaces
        if (DataHeatBalance::AnyEMPD || DataHeatBalance::AnyHAMT) {
            for (int SurfNum : HTNonWindowSurfs) {
                auto const &surface(Surface(SurfNum));
                int ZoneNum = surface.Zone;

                if (surface.HeatTransferAlgorithm == HeatTransferModel_HAMT) {
                    HeatBalanceHAMTManager::UpdateHeatBalHAMT(SurfNum);

                    Real64 const FD_Area_fac(HMassConvInFD(SurfNum) * surface.Area);

                    SumHmAW(ZoneNum) += FD_Area_fac * (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));

                    Real64 const MAT_zone(MAT(surface.Zone));
                    RhoAirZone = Psychrometrics::PsyRhoAirFnPbTdbW(
                        OutBaroPress,
                        MAT_zone,
                        Psychrometrics::PsyWFnTdbRhPb(
                            MAT_zone, Psychrometrics::PsyRhFnTdbRhov(MAT_zone, RhoVaporAirIn(SurfNum), rhoAirZone), OutBaroPress));

                    Real64 const surfInTemp(TempSurfInTmp(SurfNum));
                    Wsurf = Psychrometrics::PsyWFnTdbRhPb(
                        surfInTemp, Psychrometrics::PsyRhFnTdbRhov(surfInTemp, RhoVaporSurfIn(SurfNum), wsurf), OutBaroPress);

                    SumHmARa(ZoneNum) += FD_Area_fac * RhoAirZone;

                    SumHmARaW(ZoneNum) += FD_Area_fac * RhoVaporSurfIn(SurfNum); // old eq'n: FD_Area_fac * RhoAirZone * Wsurf;

                } else if (surface.HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                    // need to calculate the amount of moisture that is entering or
                    // leaving the zone  Qm [kg/sec] = hmi * Area * (Del Rhov)
                    // {Hmi [m/sec];     Area [m2];    Rhov [kg moist/m3]  }
                    // Positive values are into the zone and negative values are
                    // leaving the zone.  SumHmAw is the sum of the moisture entering or
                    // leaving the zone from all of the surfaces and is a rate.  Multiply
                    // by time to get the actual amount affecting the zone volume of air.

                    MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD(SurfNum);
                    RhoVaporSurfIn(SurfNum) = DataMoistureBalanceEMPD::RVSurface(SurfNum);
                    Real64 const FD_Area_fac(HMassConvInFD(SurfNum) * surface.Area);
                    SumHmAW(ZoneNum) += FD_Area_fac * (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));
                    Real64 const MAT_zone(MAT(ZoneNum));
                    SumHmARa(ZoneNum) +=
                        FD_Area_fac *
                        Psychrometrics::PsyRhoAirFnPbTdbW(
                            OutBaroPress,
                            MAT_zone,
                            Psychrometrics::PsyWFnTdbRhPb(MAT_zone,
                                                          Psychrometrics::PsyRhFnTdbRhovLBnd0C(MAT_zone, RhoVaporAirIn(SurfNum)),
                                                          OutBaroPress)); // surfInTemp, PsyWFnTdbRhPb( surfInTemp, PsyRhFnTdbRhovLBnd0C(
                                                                          // surfInTemp, RhoVaporAirIn( SurfNum ) ), OutBaroPress ) );
                    SumHmARaW(ZoneNum) += FD_Area_fac * RhoVaporSurfIn(SurfNum);
                }
            }
        }

        ReportIntMovInsInsideSurfTemp();

        CalculateZoneMRT(ZoneToResimulate); // Update here so that the proper value of MRT is available to radiant systems
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

        static std::string const Inside("Inside");

        if (calcHeatBalInsideSurfCTFOnlyFirstTime) {
            // Set up coefficient arrays that never change - loop over non-window HT surfaces
            for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
                int const firstSurf = Zone(zoneNum).NonWindowSurfaceFirst;
                int const lastSurf = Zone(zoneNum).NonWindowSurfaceLast;
                for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                    int const ConstrNum = Surface(surfNum).Construction;
                    auto const &construct(dataConstruction.Construct(ConstrNum));
                    if (Surface(surfNum).ExtBoundCond == surfNum) {
                        IsAdiabatic(surfNum) = 1;
                        IsNotAdiabatic(surfNum) = 0;
                    } else {
                        IsAdiabatic(surfNum) = 0;
                        IsNotAdiabatic(surfNum) = 1;
                    }
                    if (construct.SourceSinkPresent) {
                        IsSource(surfNum) = 1;
                        IsNotSource(surfNum) = 0;
                    } else {
                        QsrcHistSurf1(surfNum) = 0.0;
                        IsSource(surfNum) = 0;
                        IsNotSource(surfNum) = 1;
                    }
                    if (!Surface(surfNum).IsPool) {
                        IsPoolSurf(surfNum) = 0;
                        IsNotPoolSurf(surfNum) = 1;
                    }
                }
            }

            calcHeatBalInsideSurfCTFOnlyFirstTime = false;
        }

        for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
            int const firstSurf = Zone(zoneNum).SurfaceFirst;
            int const lastSurf = Zone(zoneNum).SurfaceLast;

            // determine reference air temperatures and other variable terms - loop over all surfaces
            for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {

                int const ConstrNum = Surface(surfNum).Construction;
                auto const &construct(dataConstruction.Construct(ConstrNum));
                CTFCross0(surfNum) = construct.CTFCross(0);
                CTFInside0(surfNum) = construct.CTFInside(0);
                CTFSourceIn0(surfNum) = construct.CTFSourceIn(0);
                TH11Surf(surfNum) = TH(1, 1, surfNum);
                if (construct.SourceSinkPresent) {
                    QsrcHistSurf1(surfNum) = QsrcHist(surfNum, 1);
                }

                // The special heat balance terms for pools are used only when the pool is operating, so IsPool can change
                if (Surface(surfNum).IsPool) {
                    if ((std::abs(QPoolSurfNumerator(surfNum)) >= PoolIsOperatingLimit) ||
                        (std::abs(PoolHeatTransCoefs(surfNum)) >= PoolIsOperatingLimit)) {
                        IsPoolSurf(surfNum) = 1;
                        IsNotPoolSurf(surfNum) = 0;
                    } else {
                        IsPoolSurf(surfNum) = 0;
                        IsNotPoolSurf(surfNum) = 1;
                    }
                }

                // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.
                if (Surface(surfNum).Class == SurfaceClass_TDD_Dome) continue;

                {
                    auto const SELECT_CASE_var(Surface(surfNum).TAirRef);
                    if (SELECT_CASE_var == ZoneMeanAirTemp) {
                        RefAirTemp(surfNum) = MAT(zoneNum);
                        TempEffBulkAir(surfNum) = MAT(zoneNum); // for reporting surf adjacent air temp
                    } else if (SELECT_CASE_var == AdjacentAirTemp) {
                        RefAirTemp(surfNum) = TempEffBulkAir(surfNum);
                    } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                        // determine ZoneEquipConfigNum for this zone
                        int ZoneEquipConfigNum = zoneNum;
                        // check whether this zone is a controlled zone or not
                        if (!Zone(zoneNum).IsControlled) {
                            ShowFatalError("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " +
                                           Zone(zoneNum).Name);
                            return;
                        }
                        // determine supply air conditions
                        Real64 SumSysMCp = 0.0;
                        Real64 SumSysMCpT = 0.0;
                        Real64 const CpAir = Psychrometrics::PsyCpAirFnW(ZoneAirHumRat(zoneNum));
                        for (int NodeNum = 1; NodeNum <= DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                            Real64 NodeTemp = DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                            Real64 MassFlowRate =
                                DataLoopNode::Node(DataZoneEquipment::ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                            SumSysMCp += MassFlowRate * CpAir;
                            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                        }
                        // a weighted average of the inlet temperatures.
                        if (SumSysMCp > 0.0) {                            // protect div by zero
                            RefAirTemp(surfNum) = SumSysMCpT / SumSysMCp; // BG changed 02-16-2005 to add index (SurfNum)
                        } else {
                            RefAirTemp(surfNum) = MAT(zoneNum);
                        }
                        TempEffBulkAir(surfNum) = RefAirTemp(surfNum); // for reporting surf adjacent air temp
                    } else {
                        // currently set to mean air temp but should add error warning here
                        RefAirTemp(surfNum) = MAT(zoneNum);
                        TempEffBulkAir(surfNum) = MAT(zoneNum); // for reporting surf adjacent air temp
                    }
                }
            }

            // Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
            // CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
            // CalcWindowHeatBalance.
            // Only for Surface(SurfNum).Class == DataSurfaces::SurfaceClass_Window
            int const firstWindowSurf = Zone(zoneNum).WindowSurfaceFirst;
            int const lastWindowSurf = Zone(zoneNum).WindowSurfaceLast;
            for (int surfNum = firstWindowSurf; surfNum <= lastWindowSurf; ++surfNum) {
                SurfWinHeatGain(surfNum) = 0.0;
                SurfWinHeatTransfer(surfNum) = 0.0;
                SurfWinHeatGainRep(surfNum) = 0.0;
                SurfWinHeatLossRep(surfNum) = 0.0;
                SurfWinGainConvGlazToZoneRep(surfNum) = 0.0;
                SurfWinGainIRGlazToZoneRep(surfNum) = 0.0;
                SurfWinLossSWZoneToOutWinRep(surfNum) = 0.0;
                SurfWinGainFrameDividerToZoneRep(surfNum) = 0.0;
                SurfWinGainConvGlazShadGapToZoneRep(surfNum) = 0.0;
                SurfWinGainConvShadeToZoneRep(surfNum) = 0.0;
                SurfWinOtherConvGainInsideFaceToZoneRep(surfNum) = 0.0;
                SurfWinGainIRShadeToZoneRep(surfNum) = 0.0;
                SurfWinFrameQRadOutAbs(surfNum) = 0.0;
                SurfWinFrameQRadInAbs(surfNum) = 0.0;
                SurfWinDividerQRadOutAbs(surfNum) = 0.0;
                SurfWinDividerQRadInAbs(surfNum) = 0.0;
            }

            // Calculate heat extract due to additional heat flux source term as the surface boundary condition - all HT surfaces
            if (DataSurfaces::AnyHeatBalanceInsideSourceTerm) {
                for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                    if (Surface(surfNum).InsideHeatSourceTermSchedule) {
                        SurfQAdditionalHeatSourceInside(surfNum) =
                            EnergyPlus::ScheduleManager::GetCurrentScheduleValue(Surface(surfNum).InsideHeatSourceTermSchedule);
                    }
                }
            }

            // Set up coefficient arrays prior to calculations and precalc terms that do no change during iteration - non-window surfaces
            int const firstNonWinSurf = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastNonWinSurf = Zone(zoneNum).NonWindowSurfaceLast;
            Real64 const timeStepZoneSeconds = TimeStepZoneSec; // local for vectorization
            Real64 const iterDampConstant = IterDampConst;      // local for vectorization
            // this loop auto-vectorizes
            for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {

                // Pre-calculate a few terms before the iteration loop
                TempTermSurf(surfNum) = CTFConstInPart(surfNum) + SurfQRadThermInAbs(surfNum) + SurfOpaqQRadSWInAbs(surfNum) +
                        SurfQAdditionalHeatSourceInside(surfNum) + HConvIn(surfNum) * RefAirTemp(surfNum) + QHTRadSysSurf(surfNum) +
                        QCoolingPanelSurf(surfNum) + QHWBaseboardSurf(surfNum) + QSteamBaseboardSurf(surfNum) +
                        QElecBaseboardSurf(surfNum) + (QRadSurfAFNDuct(surfNum) / timeStepZoneSeconds);
                TempDivSurf(surfNum) =
                    1.0 / (CTFInside0(surfNum) - IsAdiabatic(surfNum) * CTFCross0(surfNum) + IsPoolSurf(surfNum) * PoolHeatTransCoefs(surfNum) +
                           IsNotPoolSurf(surfNum) * HConvIn(surfNum) + iterDampConstant);
            }
        }

        InsideSurfIterations = 0;
        bool Converged = false; // .TRUE. if inside heat balance has converged
        while (!Converged) {    // Start of main inside heat balance iteration loop...

            TempInsOld = TempSurfIn; // Keep track of last iteration's temperature values

            HeatBalanceIntRadExchange::CalcInteriorRadExchange(
                TempSurfIn, InsideSurfIterations, SurfNetLWRadToSurf, ZoneToResimulate, Inside); // Update the radiation balance

            // Every 30 iterations, recalculate the inside convection coefficients in case
            // there has been a significant drift in the surface temperatures predicted.
            // This is not fool-proof and it basically means that the outside surface
            // heat balance is in error (potentially) once HConvIn is re-evaluated.
            // The choice of 30 is not significant--just want to do this a couple of
            // times before the iteration limit is hit.
            if ((InsideSurfIterations > 0) && (mod(InsideSurfIterations, ItersReevalConvCoeff) == 0)) {
                ConvectionCoefficients::InitInteriorConvectionCoeffs(state, TempSurfIn, ZoneToResimulate);
                // Since HConvIn has changed re-calculate a few terms - non-window surfaces
                for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
                    int const firstSurf = Zone(zoneNum).NonWindowSurfaceFirst;
                    int const lastSurf = Zone(zoneNum).NonWindowSurfaceLast;
                    Real64 const timeStepZoneSeconds = TimeStepZoneSec; // local for vectorization
                    Real64 const iterDampConstant = IterDampConst;      // local for vectorization
                    // this loop auto-vectorizes
                    for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                        TempTermSurf(surfNum) =
                            CTFConstInPart(surfNum) + SurfQRadThermInAbs(surfNum) + SurfOpaqQRadSWInAbs(surfNum) + SurfQAdditionalHeatSourceInside(surfNum) +
                            HConvIn(surfNum) * RefAirTemp(surfNum) + QHTRadSysSurf(surfNum) + QCoolingPanelSurf(surfNum) + QHWBaseboardSurf(surfNum) +
                            QSteamBaseboardSurf(surfNum) + QElecBaseboardSurf(surfNum) + (QRadSurfAFNDuct(surfNum) / timeStepZoneSeconds);
                        TempDivSurf(surfNum) =
                            1.0 / (CTFInside0(surfNum) - IsAdiabatic(surfNum) * CTFCross0(surfNum) +
                                   IsPoolSurf(surfNum) * PoolHeatTransCoefs(surfNum) + IsNotPoolSurf(surfNum) * HConvIn(surfNum) + iterDampConstant);
                    }
                }
            }

            // Loop over non-window surfaces
            for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
                int const firstNonWinSurf = Zone(zoneNum).NonWindowSurfaceFirst;
                int const lastNonWinSurf = Zone(zoneNum).NonWindowSurfaceLast;
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
                    TempSurfInTmp(surfNum) =
                        (IsNotPoolSurf(surfNum) * (TempTermSurf(surfNum) + SurfNetLWRadToSurf(surfNum)) +
                         IsSource(surfNum) * CTFSourceIn0(surfNum) * QsrcHistSurf1(surfNum) + IsPoolSurf(surfNum) * CTFConstInPart(surfNum) +
                         IsPoolSurf(surfNum) * QPoolSurfNumerator(surfNum) + iterDampConstant * TempInsOld(surfNum) +
                         IsNotAdiabatic(surfNum) * CTFCross0(surfNum) * TH11Surf(surfNum)) *
                        TempDivSurf(surfNum); // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW
                                              // radiation from internal sources | Convection from surface to zone air | Net radiant
                                              // exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there
                                              // is one present) | Radiant flux from high temp radiant heater | Radiant flux from a hot
                                              // water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from
                                              // an electric baseboard heater | Iterative damping term (for stability) | Current
                                              // conduction from | the outside surface | Coefficient for conduction (current time) |
                                              // Convection and damping term | Radiation from AFN ducts

                    TempSurfIn(surfNum) = TempSurfInTmp(surfNum);
                }

                // Loop over non-window surfaces (includes TubularDaylightingDomes)
                for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {
                    if (Surface(surfNum).HeatTransferAlgorithm == HeatTransferModel_AirBoundaryNoHT) {
                        // This is a temporary band-aid to get the same results with airboundary surfaces
                        // 1. Air boundary surfaces shouldn't be part of HT surf lists, I don't think
                        // 2. And even if it is, it shouldn't be impacting other surface temps, but it is somehow
                        // 3. So, once that's fixed in develop, this can go away here.
                        TempSurfIn(surfNum) = TempSurfInTmp(surfNum) = 23.0;
                        continue;
                    }
                    Real64 HMovInsul = 0.0; // "Convection" coefficient of movable insulation
                    if (Surface(surfNum).MaterialMovInsulInt > 0) {
                        Real64 AbsInt = 0.0; // Solar absorptance of inside movable insulation (not used here)
                        HeatBalanceMovableInsulation::EvalInsideMovableInsulation(surfNum, HMovInsul, AbsInt);
                    }

                    if (HMovInsul > 0.0) { // Movable insulation present, recalc surface temps
                        Real64 F1 = HMovInsul / (HMovInsul + HConvIn(surfNum) + IterDampConst);

                        TempSurfIn(surfNum) =
                            (CTFConstInPart(surfNum) + SurfOpaqQRadSWInAbs(surfNum) + CTFCross0(surfNum) * TH11Surf(surfNum) +
                             F1 * (SurfQRadThermInAbs(surfNum) + HConvIn(surfNum) * RefAirTemp(surfNum) + SurfNetLWRadToSurf(surfNum) +
                                   QHTRadSysSurf(surfNum) + QCoolingPanelSurf(surfNum) + QHWBaseboardSurf(surfNum) + QSteamBaseboardSurf(surfNum) +
                                   QElecBaseboardSurf(surfNum) + SurfQAdditionalHeatSourceInside(surfNum) + IterDampConst * TempInsOld(surfNum))) /
                            (CTFInside0(surfNum) + HMovInsul - F1 * HMovInsul); // Convection from surface to zone air

                        TempSurfInTmp(surfNum) = (CTFInside0(surfNum) * TempSurfIn(surfNum) + HMovInsul * TempSurfIn(surfNum) - SurfOpaqQRadSWInAbs(surfNum) -
                                                  CTFConstInPart(surfNum) - CTFCross0(surfNum) * TH11Surf(surfNum)) /
                                                 (HMovInsul);
                    }

                    if (AnyConstructInternalSourceInInput) {
                        if (dataConstruction.Construct(Surface(surfNum).Construction).SourceSinkPresent) {
                            // Set the appropriate parameters for the radiant system
                            // Radiant system does not need the damping coefficient terms (hopefully)
                            Real64 const RadSysDiv(1.0 / (CTFInside0(surfNum) + HConvIn(surfNum)));
                            Real64 const TempTerm(CTFConstInPart(surfNum) + SurfQRadThermInAbs(surfNum) + SurfOpaqQRadSWInAbs(surfNum) +
                                                          SurfQAdditionalHeatSourceInside(surfNum) + HConvIn(surfNum) * RefAirTemp(surfNum) +
                                                  QHTRadSysSurf(surfNum) + QCoolingPanelSurf(surfNum) + QHWBaseboardSurf(surfNum) +
                                                  QSteamBaseboardSurf(surfNum) + QElecBaseboardSurf(surfNum) + SurfNetLWRadToSurf(surfNum) +
                                                  (QRadSurfAFNDuct(surfNum) / TimeStepZoneSec));
                            RadSysTiHBConstCoef(surfNum) =
                                TempTerm * RadSysDiv; // Constant portion of cond eq (history terms) | LW radiation from internal sources | SW
                                                      // radiation from internal sources | Convection from surface to zone air | Radiant flux
                                                      // from high temp radiant heater | Radiant flux from a hot water baseboard heater |
                                                      // Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard
                                                      // heater | Net radiant exchange with other zone surfaces | Cond term (both partition
                                                      // sides same temp) | Convection and damping term
                            RadSysTiHBToutCoef(surfNum) = CTFCross0(surfNum) * RadSysDiv;    // Outside temp=inside temp for a partition |
                                                                                             // Cond term (both partition sides same temp) |
                                                                                             // Convection and damping term
                            RadSysTiHBQsrcCoef(surfNum) = CTFSourceIn0(surfNum) * RadSysDiv; // QTF term for the source | Cond term (both
                                                                                             // partition sides same temp) | Convection and
                                                                                             // damping term

                            if (Surface(surfNum).ExtBoundCond > 0) { // This is an interzone partition and we need to set outside params
                                // The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
                                // the inside coefficients are set up once the heat balance equation for that side has been calculated.
                                // For both sides to actually have been set, we have to wait until we get to the second side in the surface
                                // derived type.  At that point, both inside coefficient sets have been evaluated.
                                if (Surface(surfNum).ExtBoundCond <= surfNum) { // Both of the inside coefficients have now been set
                                    int OtherSideSurfNum = Surface(surfNum).ExtBoundCond;
                                    RadSysToHBConstCoef(OtherSideSurfNum) = RadSysTiHBConstCoef(surfNum);
                                    RadSysToHBTinCoef(OtherSideSurfNum) = RadSysTiHBToutCoef(surfNum);
                                    RadSysToHBQsrcCoef(OtherSideSurfNum) = RadSysTiHBQsrcCoef(surfNum);
                                    RadSysToHBConstCoef(surfNum) = RadSysTiHBConstCoef(OtherSideSurfNum);
                                    RadSysToHBTinCoef(surfNum) = RadSysTiHBToutCoef(OtherSideSurfNum);
                                    RadSysToHBQsrcCoef(surfNum) = RadSysTiHBQsrcCoef(OtherSideSurfNum);
                                }
                            }
                        }
                    }
                }

                // Loop over window surfaces
                int const firstWindowSurf = Zone(zoneNum).WindowSurfaceFirst;
                int const lastWindowSurf = Zone(zoneNum).WindowSurfaceLast;
                for (int surfNum = firstWindowSurf; surfNum <= lastWindowSurf; ++surfNum) {
                    auto &surface(Surface(surfNum));
                    Real64 &TH11(TH(1, 1, surfNum));
                    int ConstrNum = surface.Construction; // Not const, because storm window may change this
                    auto const &construct(dataConstruction.Construct(ConstrNum));
                    if (SurfWinOriginalClass(surfNum) == SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                        // Lookup up the TDD:DOME object
                        int const pipeNum = SurfWinTDDPipeNum(surfNum);
                        int const domeNum = DataDaylightingDevices::TDDPipe(pipeNum).Dome;
                        // Ueff = 1 / effective R value between TDD:DOME and TDD:DIFFUSER
                        Real64 Ueff = 1.0 / DataDaylightingDevices::TDDPipe(pipeNum).Reff;

                        // Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
                        // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
                        //   = QRadSWwinAbs(surfNum,1)/2.0
                        Real64 const HConvIn_surf(HConvInFD(surfNum) = HConvIn(surfNum));
                        TempSurfIn(surfNum) = TempSurfInTmp(surfNum) =
                            (SurfQRadThermInAbs(surfNum) + SurfWinQRadSWwinAbs(1, surfNum) / 2.0 + SurfQAdditionalHeatSourceInside(surfNum) +
                             HConvIn_surf * RefAirTemp(surfNum) + SurfNetLWRadToSurf(surfNum) + IterDampConst * TempInsOld(surfNum) +
                             Ueff * TH(1, 1, domeNum)) /
                            (Ueff + HConvIn_surf + IterDampConst); // LW radiation from internal sources | SW radiation from internal sources and
                                                                   // solar | Convection from surface to zone air | Net radiant exchange with
                                                                   // other zone surfaces | Iterative damping term (for stability) | Current
                                                                   // conduction from the outside surface | Coefficient for conduction (current
                                                                   // time) | Convection and damping term

                        Real64 const Sigma_Temp_4(DataGlobals::StefanBoltzmann * pow_4(TempSurfIn(surfNum)));

                        // Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
                        SurfWinHeatGain(surfNum) =
                                SurfWinTransSolar(surfNum) + HConvIn_surf * surface.Area * (TempSurfIn(surfNum) - RefAirTemp(surfNum)) +
                                dataConstruction.Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                                (Sigma_Temp_4 - (SurfWinIRfromParentZone(surfNum) + QHTRadSysSurf(surfNum) + QCoolingPanelSurf(surfNum) +
                                                 QHWBaseboardSurf(surfNum) + QSteamBaseboardSurf(surfNum) + QElecBaseboardSurf(surfNum))) - QS(surface.SolarEnclIndex) * surface.Area *
                                dataConstruction.Construct(surface.Construction)
                                    .TransDiff; // Transmitted solar | Convection | IR exchange | IR
                                                // Zone diffuse interior shortwave reflected back into the TDD
                        SurfWinHeatTransfer(surfNum) = SurfWinHeatGain(surfNum);

                        // fill out report vars for components of Window Heat Gain
                        SurfWinGainConvGlazToZoneRep(surfNum) = HConvIn_surf * surface.Area * (TempSurfIn(surfNum) - RefAirTemp(surfNum));
                        SurfWinGainIRGlazToZoneRep(surfNum) =
                            dataConstruction.Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                            (Sigma_Temp_4 - (SurfWinIRfromParentZone(surfNum) + QHTRadSysSurf(surfNum) + QCoolingPanelSurf(surfNum) +
                                             QHWBaseboardSurf(surfNum) + QSteamBaseboardSurf(surfNum) + QElecBaseboardSurf(surfNum)));
                        SurfWinLossSWZoneToOutWinRep(surfNum) =
                            QS(surface.SolarEnclIndex) * surface.Area * dataConstruction.Construct(surface.Construction).TransDiff;
                    } else {                             // Regular window
                        if (InsideSurfIterations == 0) { // Do windows only once
                            if (SurfWinStormWinFlag(surfNum) == 1) ConstrNum = surface.StormWinConstruction;
                            // Get outside convection coeff for exterior window here to avoid calling
                            // InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
                            // (HeatBalanceSurfaceManager USEing and WindowManager and
                            // WindowManager USEing HeatBalanceSurfaceManager)
                            if (surface.ExtBoundCond == ExternalEnvironment) {
                                int RoughSurf = dataMaterial.Material(construct.LayerPoint(1)).Roughness; // Outside surface roughness
                                Real64 EmisOut =
                                    dataMaterial.Material(construct.LayerPoint(1)).AbsorpThermalFront; // Glass outside surface emissivity
                                auto const shading_flag(SurfWinShadingFlag(surfNum));
                                if (shading_flag == ExtShadeOn || shading_flag == ExtBlindOn || shading_flag == ExtScreenOn) {
                                    // Exterior shade in place
                                    int ConstrNumSh = Surface(surfNum).activeShadedConstruction;
                                    if (ConstrNumSh != 0) {
                                        RoughSurf = dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).Roughness;
                                        EmisOut = dataMaterial.Material(dataConstruction.Construct(ConstrNumSh).LayerPoint(1)).AbsorpThermal;
                                    }
                                }

                                // Get the outside effective emissivity for Equivalent layer model
                                if (construct.WindowTypeEQL) {
                                    EmisOut = WindowEquivalentLayer::EQLWindowOutsideEffectiveEmiss(ConstrNum);
                                }
                                // Set Exterior Convection Coefficient...
                                if (surface.ExtConvCoeff > 0) {

                                    HcExtSurf(surfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, surfNum);

                                } else if (surface.ExtWind) { // Window is exposed to wind (and possibly rain)

                                    // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in
                                    // subroutine)
                                    ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                        surfNum,
                                                                                        0.0,
                                                                                        RoughSurf,
                                                                                        EmisOut,
                                                                                        TH11,
                                                                                        HcExtSurf(surfNum),
                                                                                        HSkyExtSurf(surfNum),
                                                                                        HGrdExtSurf(surfNum),
                                                                                        HAirExtSurf(surfNum));

                                    if (IsRain) {                    // Raining: since wind exposed, outside window surface gets wet
                                        HcExtSurf(surfNum) = 1000.0; // Reset HcExtSurf because of wetness
                                    }

                                } else { // Not Wind exposed

                                    // Calculate exterior heat transfer coefficients for windspeed = 0
                                    ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                        surfNum,
                                                                                        0.0,
                                                                                        RoughSurf,
                                                                                        EmisOut,
                                                                                        TH11,
                                                                                        HcExtSurf(surfNum),
                                                                                        HSkyExtSurf(surfNum),
                                                                                        HGrdExtSurf(surfNum),
                                                                                        HAirExtSurf(surfNum));
                                }
                            } else { // Interior Surface

                                if (surface.ExtConvCoeff > 0) {
                                    HcExtSurf(surfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, surfNum);
                                } else {
                                    // Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of
                                    // same
                                    HcExtSurf(surfNum) = HConvIn(surface.ExtBoundCond);
                                }
                            }

                            // Following call determines inside surface temperature of glazing, and of
                            // frame and/or divider, if present
                            CalcWindowHeatBalance(state,
                                                  surfNum,
                                                  HcExtSurf(surfNum),
                                                  TempSurfInTmp(surfNum),
                                                  TH11);

                            TempSurfIn(surfNum) = TempSurfInTmp(surfNum);
                        }
                    }
                }

                // Loop over all HT surfaces
                int const firstSurf = Zone(zoneNum).SurfaceFirst;
                int const lastSurf = Zone(zoneNum).SurfaceLast;
                for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                    auto &surface(Surface(surfNum));
                    auto &zone(Zone(zoneNum));
                    Real64 &TH11(TH(1, 1, surfNum));
                    Real64 &TH12(TH(2, 1, surfNum));
                    TH12 = TempSurfInRep(surfNum) = TempSurfIn(surfNum);
                    SurfTempSurfOut(surfNum) = TH11; // For reporting

                    if (SurfWinOriginalClass(surfNum) == SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                        // Tubular daylighting devices are treated as one big object with an effective R value.
                        // The outside face temperature of the TDD:DOME and the inside face temperature of the
                        // TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
                        // Below, the resulting temperatures are copied to the inside face of the TDD:DOME
                        // and the outside face of the TDD:DIFFUSER for reporting.

                        // Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
                        int domeNum = DataDaylightingDevices::TDDPipe(SurfWinTDDPipeNum(surfNum)).Dome;
                        TH(2, 1, domeNum) = TempSurfIn(domeNum) = TempSurfInTmp(domeNum) = TempSurfInRep(domeNum) = TempSurfIn(surfNum);

                        // Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
                        // Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
                        TH11 = SurfTempSurfOut(surfNum) = SurfTempSurfOut(domeNum) = TH(1, 1, domeNum);
                    }

                    if ((TH12 > MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
                        TestSurfTempCalcHeatBalanceInsideSurf(TH12, surface, zone, calcHeatBalInsideSurfWarmupErrCount);
                    }
                }
            } // ...end of main loops over all surfaces for inside heat balances

            // Interzone surface updating: interzone surfaces have other side temperatures
            // which can vary as the simulation iterates through the inside heat
            // balance.  This block is intended to "lock" the opposite side (outside)
            // temperatures to the correct value, namely the value calculated by the
            // inside surface heat balance for the other side.
            assert(TH.index(1, 1, 1) == 0u); // Assumed for linear indexing below
            auto const l211(TH.index(2, 1, 1) - 1);
            for (int SurfNum : IZSurfs) {
                int const surfExtBoundCond(Surface(SurfNum).ExtBoundCond);
                // Set the outside surface temperature to the inside surface temperature of the interzone pair.
                // By going through all of the surfaces, this should pick up the other side as well as affect the next iteration.
                // [ SurfNum - 1 ] == ( 1, 1, SurfNum )
                // [ l211 + surfExtBoundCond ] == ( 2, 1, surfExtBoundCond )
                SurfTempSurfOut(SurfNum) = TH[SurfNum - 1] = TH[l211 + surfExtBoundCond];
                TH11Surf(SurfNum) = SurfTempSurfOut(SurfNum);
            }

            ++InsideSurfIterations;

            // Convergence check - Loop through all relevant non-window surfaces to check for convergence...
            Real64 MaxDelTemp = 0.0; // Maximum change in surface temperature for any opaque surface from one iteration to the next
            for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
                int const firstNonWinSurf = Zone(zoneNum).NonWindowSurfaceFirst;
                int const lastNonWinSurf = Zone(zoneNum).NonWindowSurfaceLast;
                for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {
                    Real64 delta = TempSurfIn(surfNum) - TempInsOld(surfNum);
                    Real64 absDif = std::abs(delta);
                    MaxDelTemp = std::max(absDif, MaxDelTemp);
                }
            } // ...end of loop to check for convergence

            if (MaxDelTemp <= MaxAllowedDelTemp) Converged = true;

#ifdef EP_Count_Calls
            NumMaxInsideSurfIterations = max(NumMaxInsideSurfIterations, InsideSurfIterations);
#endif

            if (InsideSurfIterations < MinIterations) Converged = false;

            if (InsideSurfIterations > MaxIterations) {
                if (!WarmupFlag) {
                    ++calcHeatBalInsideSurfErrCount;
                    if (calcHeatBalInsideSurfErrCount < 16) {
                        ShowWarningError(
                            "Inside surface heat balance did not converge with Max Temp Difference [C] =" + General::RoundSigDigits(MaxDelTemp, 3) +
                            " vs Max Allowed Temp Diff [C] =" + General::RoundSigDigits(MaxAllowedDelTempCondFD, 6));
                        ShowContinueErrorTimeStamp("");
                    } else {
                        ShowRecurringWarningErrorAtEnd("Inside surface heat balance convergence problem continues",
                                                       calcHeatBalInsideSurfErrPointer,
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
            int const firstSurf = Zone(zoneNum).SurfaceFirst;
            int const lastSurf = Zone(zoneNum).SurfaceLast;
            for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                if (Surface(surfNum).Class == SurfaceClass_TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

                // Inside Face Convection - sign convention is positive means energy going into inside face from the air.
                auto const HConvInTemp_fac(-HConvIn(surfNum) * (TempSurfIn(surfNum) - RefAirTemp(surfNum)));
                QdotConvInRep(surfNum) = Surface(surfNum).Area * HConvInTemp_fac;
                QdotConvInRepPerArea(surfNum) = HConvInTemp_fac;
                QConvInReport(surfNum) = QdotConvInRep(surfNum) * TimeStepZoneSec;

                // The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
                // sizing for both the normal and pulse cases so that load components can be derived later.
                if (ZoneSizingCalc && CompLoadReportIsReq) {
                    if (!WarmupFlag) {
                        int TimeStepInDay = (HourOfDay - 1) * NumOfTimeStepInHour + TimeStep;
                        if (isPulseZoneSizing) {
                            OutputReportTabular::loadConvectedWithPulse(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) =
                                QdotConvInRep(surfNum);
                        } else {
                            OutputReportTabular::loadConvectedNormal(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) = QdotConvInRep(surfNum);
                            OutputReportTabular::netSurfRadSeq(DataSizing::CurOverallSimDay, TimeStepInDay, surfNum) =
                                    SurfNetLWRadToSurf(surfNum) * Surface(surfNum).Area;
                        }
                    }
                }

                // Window heat gain/loss
                if (DataSurfaces::Surface(surfNum).Class == DataSurfaces::SurfaceClass_Window) {
                    if (DataSurfaces::SurfWinHeatGain(surfNum) >= 0.0) {
                        DataSurfaces::SurfWinHeatGainRep(surfNum) = DataSurfaces::SurfWinHeatGain(surfNum);
                        DataSurfaces::SurfWinHeatGainRepEnergy(surfNum) = DataSurfaces::SurfWinHeatGainRep(surfNum) * DataGlobals::TimeStepZoneSec;
                    } else {
                        DataSurfaces::SurfWinHeatLossRep(surfNum) = -DataSurfaces::SurfWinHeatGain(surfNum);
                        DataSurfaces::SurfWinHeatLossRepEnergy(surfNum) = DataSurfaces::SurfWinHeatLossRep(surfNum) * DataGlobals::TimeStepZoneSec;
                    }

                    DataSurfaces::SurfWinHeatTransferRepEnergy(surfNum) = DataSurfaces::SurfWinHeatGain(surfNum) * DataGlobals::TimeStepZoneSec;
                    if (DataSurfaces::SurfWinOriginalClass(surfNum) == DataSurfaces::SurfaceClass_TDD_Diffuser) { // Tubular daylighting device
                        int pipeNum = DataSurfaces::SurfWinTDDPipeNum(surfNum);
                        DataDaylightingDevices::TDDPipe(pipeNum).HeatGain = DataSurfaces::SurfWinHeatGainRep(surfNum);
                        DataDaylightingDevices::TDDPipe(pipeNum).HeatLoss = DataSurfaces::SurfWinHeatLossRep(surfNum);
                    }
                    if (DataSurfaces::Surface(surfNum).ExtSolar) { // WindowManager's definition of ZoneWinHeatGain/Loss
                        int zoneNum = DataSurfaces::Surface(surfNum).Zone;
                        DataHeatBalance::ZoneWinHeatGain(zoneNum) += DataSurfaces::SurfWinHeatGain(surfNum);
                    }
                }
            }
        }

        ReportIntMovInsInsideSurfTemp();

        CalculateZoneMRT(ZoneToResimulate); // Update here so that the proper value of MRT is available to radiant systems
    }

    void TestSurfTempCalcHeatBalanceInsideSurf(Real64 TH12, SurfaceData &surface, ZoneData &zone, int WarmupSurfTemp)
    {
        using General::RoundSigDigits;

        if ((TH12 > MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
            if (WarmupFlag) ++WarmupSurfTemp;
            if (!WarmupFlag || WarmupSurfTemp > 10 || DisplayExtraWarnings) {
                if (TH12 < MinSurfaceTempLimit) {
                    if (surface.LowTempErrCount == 0) {
                        ShowSevereMessage("Temperature (low) out of bounds [" + RoundSigDigits(TH12, 2) + "] for zone=\"" + zone.Name +
                                          "\", for surface=\"" + surface.Name + "\"");
                        ShowContinueErrorTimeStamp("");
                        if (!zone.TempOutOfBoundsReported) {
                            ShowContinueError("Zone=\"" + zone.Name + "\", Diagnostic Details:");
                            if (zone.FloorArea > 0.0) {
                                ShowContinueError("...Internal Heat Gain [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) + "] W/m2");
                            } else {
                                ShowContinueError("...Internal Heat Gain (no floor) [" + RoundSigDigits(zone.InternalHeatGains, 3) + "] W");
                            }
                            if (AirflowNetwork::SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                                ShowContinueError("...Infiltration/Ventilation [" + RoundSigDigits(zone.NominalInfilVent, 3) + "] m3/s");
                                ShowContinueError("...Mixing/Cross Mixing [" + RoundSigDigits(zone.NominalMixing, 3) + "] m3/s");
                            } else {
                                ShowContinueError("...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                            }
                            if (zone.IsControlled) {
                                ShowContinueError("...Zone is part of HVAC controlled system.");
                            } else {
                                ShowContinueError("...Zone is not part of HVAC controlled system.");
                            }
                            zone.TempOutOfBoundsReported = true;
                        }
                        ShowRecurringSevereErrorAtEnd("Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name,
                                                      surface.LowTempErrCount,
                                                      TH12,
                                                      TH12,
                                                      _,
                                                      "C",
                                                      "C");
                    } else {
                        ShowRecurringSevereErrorAtEnd("Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name,
                                                      surface.LowTempErrCount,
                                                      TH12,
                                                      TH12,
                                                      _,
                                                      "C",
                                                      "C");
                    }
                } else {
                    if (surface.HighTempErrCount == 0) {
                        ShowSevereMessage("Temperature (high) out of bounds (" + RoundSigDigits(TH12, 2) + "] for zone=\"" + zone.Name +
                                          "\", for surface=\"" + surface.Name + "\"");
                        ShowContinueErrorTimeStamp("");
                        if (!zone.TempOutOfBoundsReported) {
                            ShowContinueError("Zone=\"" + zone.Name + "\", Diagnostic Details:");
                            if (zone.FloorArea > 0.0) {
                                ShowContinueError("...Internal Heat Gain [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) + "] W/m2");
                            } else {
                                ShowContinueError("...Internal Heat Gain (no floor) [" + RoundSigDigits(zone.InternalHeatGains, 3) + "] W");
                            }
                            if (AirflowNetwork::SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                                ShowContinueError("...Infiltration/Ventilation [" + RoundSigDigits(zone.NominalInfilVent, 3) + "] m3/s");
                                ShowContinueError("...Mixing/Cross Mixing [" + RoundSigDigits(zone.NominalMixing, 3) + "] m3/s");
                            } else {
                                ShowContinueError("...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                            }
                            if (zone.IsControlled) {
                                ShowContinueError("...Zone is part of HVAC controlled system.");
                            } else {
                                ShowContinueError("...Zone is not part of HVAC controlled system.");
                            }
                            zone.TempOutOfBoundsReported = true;
                        }
                        ShowRecurringSevereErrorAtEnd("Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name,
                                                      surface.HighTempErrCount,
                                                      TH12,
                                                      TH12,
                                                      _,
                                                      "C",
                                                      "C");
                    } else {
                        ShowRecurringSevereErrorAtEnd("Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name,
                                                      surface.HighTempErrCount,
                                                      TH12,
                                                      TH12,
                                                      _,
                                                      "C",
                                                      "C");
                    }
                }
                if (zone.EnforcedReciprocity) {
                    if (WarmupSurfTemp > 3) {
                        ShowSevereError("CalcHeatBalanceInsideSurf: Zone=\"" + zone.Name + "\" has view factor enforced reciprocity");
                        ShowContinueError(" and is having temperature out of bounds errors. Please correct zone geometry and rerun.");
                        ShowFatalError("CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
                    }
                } else if (WarmupSurfTemp > 10) {
                    ShowFatalError("CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
                }
            }
        }
        if ((TH12 > MaxSurfaceTempLimitBeforeFatal) || (TH12 < MinSurfaceTempLimitBeforeFatal)) {
            if (!WarmupFlag) {
                if (TH12 < MinSurfaceTempLimitBeforeFatal) {
                    ShowSevereError("Temperature (low) out of bounds [" + RoundSigDigits(TH12, 2) + "] for zone=\"" + zone.Name +
                                    "\", for surface=\"" + surface.Name + "\"");
                    ShowContinueErrorTimeStamp("");
                    if (!zone.TempOutOfBoundsReported) {
                        ShowContinueError("Zone=\"" + zone.Name + "\", Diagnostic Details:");
                        if (zone.FloorArea > 0.0) {
                            ShowContinueError("...Internal Heat Gain [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) + "] W/m2");
                        } else {
                            ShowContinueError("...Internal Heat Gain (no floor) [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) +
                                              "] W");
                        }
                        if (AirflowNetwork::SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                            ShowContinueError("...Infiltration/Ventilation [" + RoundSigDigits(zone.NominalInfilVent, 3) + "] m3/s");
                            ShowContinueError("...Mixing/Cross Mixing [" + RoundSigDigits(zone.NominalMixing, 3) + "] m3/s");
                        } else {
                            ShowContinueError("...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                        }
                        if (zone.IsControlled) {
                            ShowContinueError("...Zone is part of HVAC controlled system.");
                        } else {
                            ShowContinueError("...Zone is not part of HVAC controlled system.");
                        }
                        zone.TempOutOfBoundsReported = true;
                    }
                    ShowFatalError("Program terminates due to preceding condition.");
                } else {
                    ShowSevereError("Temperature (high) out of bounds [" + RoundSigDigits(TH12, 2) + "] for zone=\"" + zone.Name +
                                    "\", for surface=\"" + surface.Name + "\"");
                    ShowContinueErrorTimeStamp("");
                    if (!zone.TempOutOfBoundsReported) {
                        ShowContinueError("Zone=\"" + zone.Name + "\", Diagnostic Details:");
                        if (zone.FloorArea > 0.0) {
                            ShowContinueError("...Internal Heat Gain [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) + "] W/m2");
                        } else {
                            ShowContinueError("...Internal Heat Gain (no floor) [" + RoundSigDigits(zone.InternalHeatGains / zone.FloorArea, 3) +
                                              "] W");
                        }
                        if (AirflowNetwork::SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                            ShowContinueError("...Infiltration/Ventilation [" + RoundSigDigits(zone.NominalInfilVent, 3) + "] m3/s");
                            ShowContinueError("...Mixing/Cross Mixing [" + RoundSigDigits(zone.NominalMixing, 3) + "] m3/s");
                        } else {
                            ShowContinueError("...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                        }
                        if (zone.IsControlled) {
                            ShowContinueError("...Zone is part of HVAC controlled system.");
                        } else {
                            ShowContinueError("...Zone is not part of HVAC controlled system.");
                        }
                        zone.TempOutOfBoundsReported = true;
                    }
                    ShowFatalError("Program terminates due to preceding condition.");
                }
            } else {
                if (TH12 < -10000. || TH12 > 10000.) {
                    ShowSevereError("CalcHeatBalanceInsideSurf: The temperature of " + RoundSigDigits(TH12, 2) + " C for zone=\"" + zone.Name +
                                    "\", for surface=\"" + surface.Name + "\"");
                    ShowContinueError("..is very far out of bounds during warmup. This may be an indication of a malformed zone.");
                    ShowContinueErrorTimeStamp("");
                    ShowFatalError("Program terminates due to preceding condition.");
                }
            }
        }
    }

    void CalcOutsideSurfTemp(int const SurfNum,      // Surface number DO loop counter
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
        using namespace DataGlobals;
        using namespace DataEnvironment;
        using namespace DataHeatBalFanSys;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;
        using namespace DataSurfaces;
        using DataMoistureBalance::HAirFD;
        using DataMoistureBalance::HConvExtFD;
        using DataMoistureBalance::HConvInFD;
        using DataMoistureBalance::HGrndFD;
        using DataMoistureBalance::HMassConvExtFD;
        using DataMoistureBalance::HMassConvInFD;
        using DataMoistureBalance::HSkyFD;
        using DataMoistureBalance::RhoVaporAirIn;
        using DataMoistureBalance::RhoVaporAirOut;
        using DataMoistureBalance::RhoVaporSurfIn;
        using DataMoistureBalance::TempOutsideAirFD;
        using namespace DataDaylightingDevices;
        using namespace Psychrometrics;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // FUNCTION DEFINITIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 F1;                // Intermediate calculation variable
        Real64 F2;                // Intermediate calculation variable
        bool MovInsulPresent;     // .TRUE. if movable insulation is currently present for surface
        bool QuickConductionSurf; // .TRUE. if the cross CTF term is relatively large
        int PipeNum;              // TDD pipe object number
        int SurfNum2;             // TDD:DIFFUSER object number
        int ZoneNum2;             // TDD:DIFFUSER zone number
        int SrdSurfsNum;          // Counter
        int SrdSurfNum;           // Surrounding surface number DO loop counter
        Real64 SrdSurfTempAbs;    // Absolute temperature of a surrounding surface
        Real64 SrdSurfViewFac;    // View factor of a surrounding surface

        Real64 Ueff;    // 1 / effective R value between TDD:DOME and TDD:DIFFUSER
        Real64 RadTemp; // local value for Effective radiation temperature for OtherSideConditions model
        Real64 HRad;    // local value for effective (linearized) radiation coefficient
        Real64 TSky;
        Real64 TGround;
        // FLOW:

        // Determine whether or not movable insulation is present
        MovInsulPresent = (HMovInsul > 0.0);

        // Determine whether this surface is a "slow conductive" or "quick conductive"
        // surface.  Designates are inherited from BLAST.  Basically, a "quick" surface
        // requires the inside heat balance to be accounted for in the heat balance
        // while a "slow" surface can used the last time step's value for inside
        // surface temperature.
        auto const &construct(dataConstruction.Construct(ConstrNum));
        if (construct.CTFCross(0) > 0.01) {
            QuickConductionSurf = true;
            F1 = construct.CTFCross(0) / (construct.CTFInside(0) + HConvIn(SurfNum));
        } else {
            QuickConductionSurf = false;
        }

        TSky = SkyTemp;
        TGround = OutDryBulbTemp;

        if (Surface(SurfNum).HasSurroundingSurfProperties) {
            SrdSurfsNum = Surface(SurfNum).SurroundingSurfacesNum;
            if (SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum != 0) {
                TSky = GetCurrentScheduleValue(SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum);
            }
            if (SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum != 0) {
                TGround = GetCurrentScheduleValue(SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum);
            }
        }

        // Now, calculate the outside surface temperature using the proper heat balance equation.
        // Each case has been separated out into its own IF-THEN block for clarity.  Additional
        // cases can simply be added anywhere in the following section.  This is the last step
        // in the main loop.  Once the proper heat balance is done, the simulation goes on to
        // the next SurfNum.

        // Outside heat balance case: Tubular daylighting device
        Real64 &TH11(TH(1, 1, SurfNum));
        if (Surface(SurfNum).Class == SurfaceClass_TDD_Dome) {

            // Lookup up the TDD:DIFFUSER object
            PipeNum = SurfWinTDDPipeNum(SurfNum);
            SurfNum2 = TDDPipe(PipeNum).Diffuser;
            ZoneNum2 = Surface(SurfNum2).Zone;
            Ueff = 1.0 / TDDPipe(PipeNum).Reff;
            F1 = Ueff / (Ueff + HConvIn(SurfNum2));

            // Similar to opaque surface but inside conditions of TDD:DIFFUSER are used, and no embedded sources/sinks.
            // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
            //   QRadSWOutAbs(SurfNum) does not apply for TDD:DOME, must use QRadSWwinAbs(SurfNum,1)/2.0 instead.
            //+Construct(ConstrNum)%CTFSourceOut(0)     &   TDDs cannot be radiant systems
            // *QsrcHist(1,SurfNum)                     &
            //+Construct(ConstrNum)%CTFSourceIn(0) &   TDDs cannot be radiant systems
            // *QsrcHist(1,SurfNum)                &
            TH11 =
                (SurfWinQRadSWwinAbs(1, SurfNum) / 2.0 + SurfQRadLWOutSrdSurfs(SurfNum) + (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt +
                        SurfQAdditionalHeatSourceOutside(SurfNum) + HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround +
                 F1 * (SurfWinQRadSWwinAbs(1, SurfNum2) / 2.0 + SurfQRadThermInAbs(SurfNum2) + HConvIn(SurfNum2) * MAT(ZoneNum2) + SurfNetLWRadToSurf(SurfNum2))) /
                (Ueff + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum) -
                 F1 * Ueff); // Instead of QRadSWOutAbs(SurfNum) | ODB used to approx ground surface temp | Use TDD:DIFFUSER surface | Use
                             // TDD:DIFFUSER surface | Use TDD:DIFFUSER surface and zone | Use TDD:DIFFUSER surface

            // Outside heat balance case: No movable insulation, slow conduction
        } else if ((!MovInsulPresent) && (!QuickConductionSurf)) {
            // Add LWR from surrounding surfaces
            if (Surface(SurfNum).OSCMPtr == 0) {
                if (construct.SourceSinkPresent) {
                    TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                            (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                            HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround + construct.CTFCross(0) * TempSurfIn(SurfNum) +
                            construct.CTFSourceOut(0) * QsrcHist(SurfNum, 1)) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) +
                            HGrdExtSurf(SurfNum)); // ODB used to approx ground surface temp
                } else {
                    TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                            (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                            HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround + construct.CTFCross(0) * TempSurfIn(SurfNum)) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) +
                            HGrdExtSurf(SurfNum)); // ODB used to approx ground surface temp
                }
                // Outside Heat Balance case: Other Side Conditions Model
            } else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
                // local copies of variables for clarity in radiation terms
                // TODO: - int OSCMPtr; // "Pointer" to OSCM data structure (other side conditions from a model)
                RadTemp = OSCM(Surface(SurfNum).OSCMPtr).TRad;
                HRad = OSCM(Surface(SurfNum).OSCMPtr).HRad;

                // patterned after "No movable insulation, slow conduction," but with new radiation terms and no sun,
                if (construct.SourceSinkPresent) {
                    TH11 = (-CTFConstOutPart(SurfNum) + HcExtSurf(SurfNum) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                            construct.CTFCross(0) * TempSurfIn(SurfNum) + construct.CTFSourceOut(0) * QsrcHist(SurfNum, 1)) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HRad);
                } else {
                    TH11 = (-CTFConstOutPart(SurfNum) + HcExtSurf(SurfNum) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                            construct.CTFCross(0) * TempSurfIn(SurfNum)) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HRad);
                }
            }
            // Outside heat balance case: No movable insulation, quick conduction
        } else if ((!MovInsulPresent) && (QuickConductionSurf)) {
            if (Surface(SurfNum).OSCMPtr == 0) {
                if (construct.SourceSinkPresent) {
                    TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                            (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                            HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround + construct.CTFSourceOut(0) * QsrcHist(SurfNum, 1) +
                            F1 * (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + SurfQRadThermInAbs(SurfNum) + HConvIn(SurfNum) * MAT(ZoneNum) +
                                    SurfNetLWRadToSurf(SurfNum))) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum) -
                            F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp | MAT use here is problem for room air models
                } else {
                    TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                            (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                            HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround +
                            F1 * (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + SurfQRadThermInAbs(SurfNum) + HConvIn(SurfNum) * MAT(ZoneNum) +
                                    SurfNetLWRadToSurf(SurfNum))) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum) -
                            F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp | MAT use here is problem for room air models
                }
                // Outside Heat Balance case: Other Side Conditions Model
            } else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
                // local copies of variables for clarity in radiation terms
                RadTemp = OSCM(Surface(SurfNum).OSCMPtr).TRad;
                HRad = OSCM(Surface(SurfNum).OSCMPtr).HRad;
                // patterned after "No movable insulation, quick conduction," but with new radiation terms and no sun,
                if (construct.SourceSinkPresent) {
                    TH11 = (-CTFConstOutPart(SurfNum) + HcExtSurf(SurfNum) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                            construct.CTFSourceOut(0) * QsrcHist(SurfNum, 1) +
                            F1 * (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + SurfQRadThermInAbs(SurfNum) +
                                  construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) + HConvIn(SurfNum) * MAT(ZoneNum) + SurfNetLWRadToSurf(SurfNum))) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HRad -
                            F1 * construct.CTFCross(0)); // MAT use here is problem for room air models
                } else {
                    TH11 = (-CTFConstOutPart(SurfNum) + HcExtSurf(SurfNum) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                            F1 * (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + SurfQRadThermInAbs(SurfNum) + HConvIn(SurfNum) * MAT(ZoneNum) +
                                    SurfNetLWRadToSurf(SurfNum))) /
                           (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HRad -
                            F1 * construct.CTFCross(0)); // MAT use here is problem for room air models
                }
            }
            // Outside heat balance case: Movable insulation, slow conduction
        } else if ((MovInsulPresent) && (!QuickConductionSurf)) {

            F2 = HMovInsul / (HMovInsul + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum));

            TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) + construct.CTFCross(0) * TempSurfIn(SurfNum) +
                    F2 * (SurfQRadSWOutMvIns(SurfNum) + (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                          HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround)) /
                   (construct.CTFOutside(0) + HMovInsul - F2 * HMovInsul); // ODB used to approx ground surface temp

            // Outside heat balance case: Movable insulation, quick conduction
        } else if ((MovInsulPresent) && (QuickConductionSurf)) {

            F2 = HMovInsul / (HMovInsul + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum));

            TH11 = (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                    F1 * (CTFConstInPart(SurfNum) + SurfOpaqQRadSWInAbs(SurfNum) + SurfQRadThermInAbs(SurfNum) + HConvIn(SurfNum) * MAT(ZoneNum) +
                            SurfNetLWRadToSurf(SurfNum)) +
                    F2 * (SurfQRadSWOutMvIns(SurfNum) + (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + SurfQAdditionalHeatSourceOutside(SurfNum) +
                          HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround)) /
                   (construct.CTFOutside(0) + HMovInsul - F2 * HMovInsul - F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp

        } // ...end of outside heat balance cases IF-THEN block

        // multiply out linearized radiation coeffs for reporting
        Real64 const HExtSurf_fac(
            -(HSkyExtSurf(SurfNum) * (TH11 - TSky) + HAirExtSurf(SurfNum) * (TH11 - TempExt) + HGrdExtSurf(SurfNum) * (TH11 - TGround)));
        Real64 QRadLWOutSrdSurfsRep;
        QRadLWOutSrdSurfsRep = 0;
        // Report LWR from surrounding surfaces for current exterior surf temp
        // Current exterior surf temp would be used for the next step LWR calculation.
        if (Surface(SurfNum).HasSurroundingSurfProperties) {
            SrdSurfsNum = Surface(SurfNum).SurroundingSurfacesNum;
            for (SrdSurfNum = 1; SrdSurfNum <= SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
                SrdSurfViewFac = SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                SrdSurfTempAbs = GetCurrentScheduleValue(SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).TempSchNum) + KelvinConv;
                QRadLWOutSrdSurfsRep += StefanBoltzmann * dataMaterial.Material(dataConstruction.Construct(ConstrNum).LayerPoint(1)).AbsorpThermal *
                                        SrdSurfViewFac * (pow_4(SrdSurfTempAbs) - pow_4(TH11 + KelvinConv));
            }
        }
        QdotRadOutRep(SurfNum) = Surface(SurfNum).Area * HExtSurf_fac + Surface(SurfNum).Area * QRadLWOutSrdSurfsRep;
        QdotRadOutRepPerArea(SurfNum) = QdotRadOutRep(SurfNum) / Surface(SurfNum).Area;

        QRadOutReport(SurfNum) = QdotRadOutRep(SurfNum) * TimeStepZoneSec;

        // Calculate surface heat emission to the air, positive values indicates heat transfer from surface to the outside
        QAirExtReport(SurfNum) = Surface(SurfNum).Area * HAirExtSurf(SurfNum) * (TH(1, 1, SurfNum) - Surface(SurfNum).OutDryBulbTemp);

        // Set the radiant system heat balance coefficients if this surface is also a radiant system
        if (construct.SourceSinkPresent) {

            if (MovInsulPresent) {
                // Note: if movable insulation is ever added back in correctly, the heat balance equations above must be fixed
                ShowSevereError("Exterior movable insulation is not valid with embedded sources/sinks");
                ShowContinueError("Construction " + construct.Name + " contains an internal source or sink but also uses");
                ShowContinueError("exterior movable insulation " + dataMaterial.Material(Surface(SurfNum).MaterialMovInsulExt).Name +
                                  " for a surface with that construction.");
                ShowContinueError("This is not currently allowed because the heat balance equations do not currently accommodate this combination.");
                ErrorFlag = true;
                return;

            } else {
                Real64 const RadSysDiv(
                    1.0 / (construct.CTFOutside(0) + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum)));

                RadSysToHBConstCoef(SurfNum) =
                    (-CTFConstOutPart(SurfNum) + SurfOpaqQRadSWOutAbs(SurfNum) + SurfQRadLWOutSrdSurfs(SurfNum) +
                     (HcExtSurf(SurfNum) + HAirExtSurf(SurfNum)) * TempExt + HSkyExtSurf(SurfNum) * TSky + HGrdExtSurf(SurfNum) * TGround) *
                    RadSysDiv; // ODB used to approx ground surface temp

                RadSysToHBTinCoef(SurfNum) = construct.CTFCross(0) * RadSysDiv;

                RadSysToHBQsrcCoef(SurfNum) = construct.CTFSourceOut(0) * RadSysDiv;
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataEnvironment::IsRain;
        using DataEnvironment::OutBaroPress;
        using DataEnvironment::SkyTemp;
        using DataEnvironment::SunIsUp;
        using DataGlobals::SecInHour;
        using DataSurfaces::ExtVentedCavity;
        using DataSurfaces::OSCM;
        using DataSurfaces::Surface;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdbTwbPb;
        // unused0909  USE DataHVACGlobals , ONLY: TimeStepSys
        using ConvectionCoefficients::InitExteriorConvectionCoeff;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const BlankString;

        // INTERFACE BLOCK SPECIFICATIONS:
        // DERIVED TYPE DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

        CavNum = Surface(SurfNum).ExtCavNum;

        TempExt = Surface(SurfNum).OutDryBulbTemp;

        OutHumRatExt = PsyWFnTdbTwbPb(Surface(SurfNum).OutDryBulbTemp, Surface(SurfNum).OutWetBulbTemp, OutBaroPress);

        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, TempExt, OutHumRatExt);

        holeArea = ExtVentedCavity(CavNum).ActualArea * ExtVentedCavity(CavNum).Porosity;

        AspRat = ExtVentedCavity(CavNum).HdeltaNPL * 2.0 / ExtVentedCavity(CavNum).PlenGapThick;
        TmpTscoll = ExtVentedCavity(CavNum).TbaffleLast;
        TmpTaPlen = ExtVentedCavity(CavNum).TairLast;

        // all the work is done in this routine located in GeneralRoutines.cc

        for (iter = 1; iter <= 3; ++iter) { // this is a sequential solution approach.

            CalcPassiveExteriorBaffleGap(state,
                                         ExtVentedCavity(CavNum).SurfPtrs,
                                         holeArea,
                                         ExtVentedCavity(CavNum).Cv,
                                         ExtVentedCavity(CavNum).Cd,
                                         ExtVentedCavity(CavNum).HdeltaNPL,
                                         ExtVentedCavity(CavNum).SolAbsorp,
                                         ExtVentedCavity(CavNum).LWEmitt,
                                         ExtVentedCavity(CavNum).Tilt,
                                         AspRat,
                                         ExtVentedCavity(CavNum).PlenGapThick,
                                         ExtVentedCavity(CavNum).BaffleRoughness,
                                         ExtVentedCavity(CavNum).QdotSource,
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
        ExtVentedCavity(CavNum).Isc = Isc;
        ExtVentedCavity(CavNum).TAirCav = TmpTaPlen;
        ExtVentedCavity(CavNum).Tbaffle = TmpTscoll;
        ExtVentedCavity(CavNum).HrPlen = HrPlen;
        ExtVentedCavity(CavNum).HcPlen = HcPlen;
        ExtVentedCavity(CavNum).PassiveACH =
            (MdotVent / RhoAir) * (1.0 / (ExtVentedCavity(CavNum).ProjArea * ExtVentedCavity(CavNum).PlenGapThick)) * SecInHour;
        ExtVentedCavity(CavNum).PassiveMdotVent = MdotVent;
        ExtVentedCavity(CavNum).PassiveMdotWind = VdotWind * RhoAir;
        ExtVentedCavity(CavNum).PassiveMdotTherm = VdotThermal * RhoAir;

        // now do some updates
        ExtVentedCavity(CavNum).TairLast = ExtVentedCavity(CavNum).TAirCav;
        ExtVentedCavity(CavNum).TbaffleLast = ExtVentedCavity(CavNum).Tbaffle;

        // update the OtherSideConditionsModel coefficients.
        thisOSCM = ExtVentedCavity(CavNum).OSCMPtr;

        OSCM(thisOSCM).TConv = ExtVentedCavity(CavNum).TAirCav;
        OSCM(thisOSCM).HConv = ExtVentedCavity(CavNum).HcPlen;
        OSCM(thisOSCM).TRad = ExtVentedCavity(CavNum).Tbaffle;
        OSCM(thisOSCM).HRad = ExtVentedCavity(CavNum).HrPlen;
    }

    void GatherComponentLoadsSurfAbsFact()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   September 2012

        // PURPOSE OF THIS SUBROUTINE:
        //   Gather values during sizing used for surface absorption factors

        // METHODOLOGY EMPLOYED:
        //   Save sequence of values for report during sizing.

        using DataGlobals::CompLoadReportIsReq;
        using DataGlobals::HourOfDay;
        using DataGlobals::isPulseZoneSizing;
        using DataGlobals::NumOfTimeStepInHour;
        using DataGlobals::NumOfZones;
        using DataGlobals::TimeStep;
        using DataHeatBalance::ITABSF;
        using DataHeatBalance::TMULT;
        using DataSizing::CurOverallSimDay;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfaceClass_TDD_Dome;
        using DataSurfaces::TotSurfaces;
        using OutputReportTabular::ITABSFseq;
        using OutputReportTabular::TMULTseq;

        if (CompLoadReportIsReq && !isPulseZoneSizing) {
            int TimeStepInDay = (HourOfDay - 1) * NumOfTimeStepInHour + TimeStep;
            for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++enclosureNum) {
                TMULTseq(CurOverallSimDay, TimeStepInDay, enclosureNum) = TMULT(enclosureNum);
            }
            for (int jSurf = 1; jSurf <= TotSurfaces; ++jSurf) {
                if (!Surface(jSurf).HeatTransSurf || Surface(jSurf).Zone == 0) continue; // Skip non-heat transfer surfaces
                if (Surface(jSurf).Class == SurfaceClass_TDD_Dome) continue;             // Skip tubular daylighting device domes
                ITABSFseq(CurOverallSimDay, TimeStepInDay, jSurf) = ITABSF(jSurf);
            }
        }
    }

} // namespace HeatBalanceSurfaceManager

} // namespace EnergyPlus
