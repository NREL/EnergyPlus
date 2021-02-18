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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataComplexFenestration.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACSizingSimulationManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MatrixDataManager.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/StringUtilities.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SurfaceOctree.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>

namespace EnergyPlus {

namespace HeatBalanceManager {

    // Module containing the heat balance simulation routines
    // calculation (initialization) routines

    // MODULE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       November 1998, FW
    //       MODIFIED       April 1999, LKL
    //       MODIFIED       Dec 2006 DJS of PSU for ecoroof
    //       Added          Dec 2008 TH for thermochromic windows:
    //                       new subroutine CreateTCConstructions called by GetHeatBalanceInput
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the heat balance simulation on the building.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // The heat balance method is outlined in the "Tarp Algorithms Manual"
    // The methods are also summarized in many BSO Theses and papers.

    // OTHER NOTES:
    // This module was created from IBLAST subroutines

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataComplexFenestration;
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataRoomAirModel;
    using namespace DataIPShortCuts;
    using DataSurfaces::CalcSolRefl;
    using DataSurfaces::DividedLite;
    using DataSurfaces::FrameDivider;
    using DataSurfaces::FrameDividerProperties;
    using DataSurfaces::ShadingTransmittanceVaries;
    using DataSurfaces::StormWindow;
    using DataSurfaces::Suspended;
    using DataSurfaces::TotStormWin;
    using DataSurfaces::TotSurfaces;
    using DataWindowEquivalentLayer::TotWinEquivLayerConstructs;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;
    using WindowComplexManager::CalculateBasisLength;
    using WindowManager::W5LsqFit;

    Array1D_string const PassFail(2, {"Fail", "Pass"});

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool ManageHeatBalanceGetInputFlag(true);
        bool DoReport(false);
        bool ChangeSet(true); // Toggle for checking storm windows
        bool FirstWarmupWrite(true);
        bool WarmupConvergenceWarning(false);
        bool SizingWarmupConvergenceWarning(false);
        bool ReportWarmupConvergenceFirstWarmupWrite(true);
    } // namespace

    // Real Variables for the Heat Balance Simulation
    // Variables used to determine warmup convergence
    Array1D<Real64> MaxCoolLoadPrevDay; // Max cooling load from the previous day
    Array1D<Real64> MaxCoolLoadZone;    // Maximum zone cooling load from the current day
    Array1D<Real64> MaxHeatLoadPrevDay; // Max heating load from the previous day
    Array1D<Real64> MaxHeatLoadZone;    // Maximum zone heating load from the current day
    Array1D<Real64> MaxTempPrevDay;     // Max temperature from the previous day
    Array1D<Real64> MaxTempZone;        // Maximum zone temperature from the current day
    Array1D<Real64> MinTempPrevDay;     // Min temperature from the previous day
    Array1D<Real64> MinTempZone;        // Minimum zone temperature from the current day

    // Variables used to report difference in temperature and load from the last two warmup days
    Array1D<Real64> WarmupTempDiff;     // Temperature difference between the last two warmup days
    Array1D<Real64> WarmupLoadDiff;     // Zone load differences between the last two warmup days
    Array1D<Real64> TempZoneSecPrevDay; // Zone air temperature from the second last warmup day
    Array1D<Real64> LoadZoneSecPrevDay; // Zone load from the second last warmup day
    Array1D<Real64> TempZonePrevDay;    // Zone air temperature from the previous day
    Array1D<Real64> LoadZonePrevDay;    // Zone load from the previuos day
    Array1D<Real64> TempZone;           // Zone air temperature from the current warmup day
    Array1D<Real64> LoadZone;           // Zone load from the current warmup day

    Array2D<Real64> TempZoneRpt;       // Zone air temperature to report (average over all warmup days)
    Array1D<Real64> TempZoneRptStdDev; // Zone air temperature to report (std dev over all warmup days)
    Array2D<Real64> LoadZoneRpt;       // Zone load to report (average over all warmup days)
    Array1D<Real64> LoadZoneRptStdDev; // Zone load to report (std dev over all warmup days)
    Array2D<Real64> MaxLoadZoneRpt;    // Maximum zone load for reporting calcs
    int CountWarmupDayPoints;          // Count of warmup timesteps (to achieve warmup)

    std::string CurrentModuleObject; // to assist in getting input

    // Subroutine Specifications for the Heat Balance Module
    // Driver Routines

    // Input reader routines for the module

    // Initialization routines for module

    // Record Keeping/Utility Routines for Module

    // Reporting routines for module

    // Object Data
    Array1D<WarmupConvergence> WarmupConvergenceValues;
    std::unordered_map<std::string, std::string> UniqueMaterialNames;
    std::unordered_map<std::string, std::string> UniqueConstructNames;

    // MODULE SUBROUTINES:
    //*************************************************************************

    // Functions

    // Clears the global data in HeatBalanceManager.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        ManageHeatBalanceGetInputFlag = true;
        MaxCoolLoadPrevDay.deallocate();
        MaxCoolLoadZone.deallocate();
        MaxHeatLoadPrevDay.deallocate();
        MaxHeatLoadZone.deallocate();
        MaxTempPrevDay.deallocate();
        MaxTempZone.deallocate();
        MinTempPrevDay.deallocate();
        MinTempZone.deallocate();
        WarmupTempDiff.deallocate();
        WarmupLoadDiff.deallocate();
        TempZoneSecPrevDay.deallocate();
        LoadZoneSecPrevDay.deallocate();
        TempZonePrevDay.deallocate();
        LoadZonePrevDay.deallocate();
        TempZone.deallocate();
        LoadZone.deallocate();
        TempZoneRpt.deallocate();
        TempZoneRptStdDev.deallocate();
        LoadZoneRpt.deallocate();
        LoadZoneRptStdDev.deallocate();
        MaxLoadZoneRpt.deallocate();
        CountWarmupDayPoints = int();
        CurrentModuleObject = std::string();
        WarmupConvergenceValues.deallocate();
        UniqueMaterialNames.clear();
        UniqueConstructNames.clear();
        surfaceOctree = SurfaceOctreeCube();
        DoReport = false;
        ChangeSet = true;
        FirstWarmupWrite = true;
        WarmupConvergenceWarning = false;
        SizingWarmupConvergenceWarning = false;
        ReportWarmupConvergenceFirstWarmupWrite = true;
    }

    void ManageHeatBalance(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   January 1997
        //       MODIFIED       February 1998 Richard Liesen
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the heat balance method of calculating
        // building thermal loads.  It is called from the SimulationManager
        // at the time step level.  This driver manages the calls to all of
        // the other modules, drivers, and simulation algorithms.

        // METHODOLOGY EMPLOYED:
        // The order of this routine was taken from HeatBalanceModule with routine
        //  and Data Structuring

        // REFERENCES:
        // Legacy code from (I)BLAST, subroutine SIMZGD.

        // Using/Aliasing
        using namespace HeatBalanceSurfaceManager;
        using EMSManager::ManageEMS;
        using EMSManager::UpdateEMSTrendVariables;

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



        // Get the heat balance input at the beginning of the simulation only
        if (ManageHeatBalanceGetInputFlag) {
            GetHeatBalanceInput(state); // Obtains heat balance related parameters from input file
            HeatBalanceIntRadExchange::InitSolarViewFactors(state);

            // Surface octree setup
            //  The surface octree holds live references to surfaces so it must be updated
            //   if in the future surfaces are altered after this point
            if (TotSurfaces >= DaylightingManager::octreeCrossover) {                        // Octree can be active
                if (inputProcessor->getNumObjectsFound(state, "Daylighting:Controls") > 0) { // Daylighting is active
                    surfaceOctree.init(DataSurfaces::Surface);                               // Set up surface octree
                }
            }

            for (auto &surface : DataSurfaces::Surface)
                surface.set_computed_geometry(); // Set up extra surface geometry info for PierceSurface

            ManageHeatBalanceGetInputFlag = false;
        }

        bool anyRan;
        ManageEMS(state,
                  EMSManager::EMSCallFrom::BeginZoneTimestepBeforeInitHeatBalance,
                  anyRan,
                  ObjexxFCL::Optional_int_const()); // EMS calling point

        // These Inits will still have to be looked at as the routines are re-engineered further
        InitHeatBalance(state); // Initialize all heat balance related parameters
        ManageEMS(
            state, EMSManager::EMSCallFrom::BeginZoneTimestepAfterInitHeatBalance, anyRan, ObjexxFCL::Optional_int_const()); // EMS calling point

        // Solve the zone heat balance by first calling the Surface Heat Balance Manager
        // and then the Air Heat Balance Manager is called by the Surface Heat Balance
        // Manager.  The order of execution is still important and the zone cannot
        // go through any record keeping before the HVAC system has run because there
        // may be a radiant system in the building which will require iteration between
        // the HVAC system (called from the Air Heat Balance) and the zone (simulated
        // in the Surface Heat Balance Manager).  In the future, this may be improved.
        ManageSurfaceHeatBalance(state);
        ManageEMS(state, EMSManager::EMSCallFrom::EndZoneTimestepBeforeZoneReporting, anyRan, ObjexxFCL::Optional_int_const()); // EMS calling point
        RecKeepHeatBalance(state); // Do any heat balance related record keeping

        // This call has been moved to the FanSystemModule and does effect the output file
        //   You do get a shift in the Air Handling System Summary for the building electric loads
        // IF ((.NOT.WarmupFlag).AND.(DayOfSim.GT.0)) CALL RCKEEP  ! Do fan system accounting (to be moved later)

        ReportHeatBalance(state); // Manage heat balance reporting until the new reporting is in place

        ManageEMS(state, EMSManager::EMSCallFrom::EndZoneTimestepAfterZoneReporting, anyRan, ObjexxFCL::Optional_int_const()); // EMS calling point

        UpdateEMSTrendVariables(state);
        EnergyPlus::PluginManagement::PluginManager::updatePluginValues(state);

        if (state.dataGlobal->WarmupFlag && state.dataGlobal->EndDayFlag) {

            CheckWarmupConvergence(state);
            if (!state.dataGlobal->WarmupFlag) {
                state.dataGlobal->DayOfSim = 0; // Reset DayOfSim if Warmup converged
                state.dataGlobal->DayOfSimChr = "0";

                ManageEMS(state, EMSManager::EMSCallFrom::BeginNewEnvironmentAfterWarmUp, anyRan, ObjexxFCL::Optional_int_const()); // calling point
            }
        }

        if (!state.dataGlobal->WarmupFlag && state.dataGlobal->EndDayFlag && state.dataGlobal->DayOfSim == 1 && !state.dataGlobal->DoingSizing) {
            ReportWarmupConvergence(state);
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetHeatBalanceInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   September 1997
        //       MODIFIED       February 1998 Richard Liesen
        //                      November 1998 FW
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main driver for initializations within the
        // heat balance.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using InternalHeatGains::ManageInternalHeatGains;

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
        bool ErrorsFound(false); // If errors detected in input
        bool ValidSimulationWithNoZones;



        GetProjectControlData(state, ErrorsFound);

        GetSiteAtmosphereData(state, ErrorsFound);

        GetWindowGlassSpectralData(state, ErrorsFound);

        GetMaterialData(state, ErrorsFound); // Read materials from input file/transfer from legacy data structure

        GetFrameAndDividerData(state, ErrorsFound);

        GetConstructData(state, ErrorsFound); // Read constructs from input file/transfer from legacy data structure

        GetBuildingData(state, ErrorsFound); // Read building data from input file

        // Added SV 6/26/2013 to load scheduled surface gains
        GetScheduledSurfaceGains(state, ErrorsFound);

        // Added TH 1/9/2009 to create thermochromic window constructions
        CreateTCConstructions(state, ErrorsFound);

        if (TotSurfaces > 0 && state.dataGlobal->NumOfZones == 0) {
            ValidSimulationWithNoZones = CheckValidSimulationObjects(state);
            if (!ValidSimulationWithNoZones) {
                ShowSevereError(state, "GetHeatBalanceInput: There are surfaces in input but no zones found.  Invalid simulation.");
                ErrorsFound = true;
            }
        }

        CheckUsedConstructions(state, ErrorsFound);

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in Building Input, Program Stopped");
        }

        // following is done to "get internal heat gains" input so that lights are gotten before
        // daylighting input
        ManageInternalHeatGains(state, true);
    }

    void CheckUsedConstructions(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Counts or details unused constructions.

        // Using/Aliasing

        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const NumConstrObjects(6);
        static Array1D_string const ConstrObjects(NumConstrObjects,
                                                  {"Pipe:Indoor",
                                                   "Pipe:Outdoor",
                                                   "Pipe:Underground",
                                                   "GroundHeatExchanger:Surface",
                                                   "DaylightingDevice:Tubular",
                                                   "EnergyManagementSystem:ConstructionIndexVariable"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Unused;
        int Loop;
        int NumObjects;
        int NumAlphas;
        int NumNumbers;
        int Status;
        int CNum;
        int ONum;
        bool InErrFlag; // Preserve (no current use) the input status of ErrorsFound

        InErrFlag = ErrorsFound;

        // Needs to account for Pipe:HeatTransfer/indoor, etc constructions.
        for (ONum = 1; ONum <= NumConstrObjects; ++ONum) {
            NumObjects = inputProcessor->getNumObjectsFound(state, ConstrObjects(ONum));
            for (Loop = 1; Loop <= NumObjects; ++Loop) {
                inputProcessor->getObjectItem(state, ConstrObjects(ONum), Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status);
                if (ONum == 5) {
                    CNum = UtilityRoutines::FindItemInList(cAlphaArgs(4), state.dataConstruction->Construct);
                } else {
                    CNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct);
                }
                if (CNum == 0) continue;
                state.dataConstruction->Construct(CNum).IsUsed = true;
                if (ONum == 4 || ONum == 6) {
                    // GroundHeatExchanger:Surface or EnergyManagementSystem:ConstructionIndexVariable
                    // Include all EMS constructions since they can potentially be used by a CTF surface
                    if (!state.dataConstruction->Construct(CNum).TypeIsWindow) {
                        state.dataConstruction->Construct(CNum).IsUsedCTF = true;
                    }
                }
            }
        }
        Unused = TotConstructs - std::count_if(state.dataConstruction->Construct.begin(),
                                               state.dataConstruction->Construct.end(),
                                               [](Construction::ConstructionProps const &e) { return e.IsUsed; });
        if (Unused > 0) {
            if (!state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state, format("CheckUsedConstructions: There are {} nominally unused constructions in input.", Unused));
                ShowContinueError(state, "For explicit details on each unused construction, use Output:Diagnostics,DisplayExtraWarnings;");
            } else {
                ShowWarningError(state, format("CheckUsedConstructions: There are {} nominally unused constructions in input.", Unused));
                ShowContinueError(state, "Each Unused construction is shown.");
                for (Loop = 1; Loop <= TotConstructs; ++Loop) {
                    if (state.dataConstruction->Construct(Loop).IsUsed) continue;
                    ShowMessage(state, "Construction=" + state.dataConstruction->Construct(Loop).Name);
                }
            }
        }
    }

    bool CheckValidSimulationObjects(EnergyPlusData &state)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // If an input file presents with surfaces but no zones, there are certain objects
        // that must be present for the simulation to be valid.  This check was necessitated by
        // an input file that was entirely detached shading surfaces but no zones (and nothing else).
        // Other objects include Solar Collectors, PV arrays.

        // METHODOLOGY EMPLOYED:
        // Check for specific objects that must be present for such a simulation to be valid.

        // Return value
        bool ValidSimulation; // True is other objects appear to make this a valid simulation.

        ValidSimulation = false;
        if (inputProcessor->getNumObjectsFound(state, "SolarCollector:FlatPlate:Water") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:Photovoltaic") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:InternalCombustionEngine") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:CombustionTurbine") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:FuelCell") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:MicroCHP") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:MicroTurbine") > 0) {
            ValidSimulation = true;
        } else if (inputProcessor->getNumObjectsFound(state, "Generator:WindTurbine") > 0) {
            ValidSimulation = true;
        }

        return ValidSimulation;
    }

    void SetPreConstructionInputParameters(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   October 2014
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets parameters that need to be established before any heat balance inputs are read

        int NumAlpha;
        int NumNumber;
        int IOStat;

        // Get all the construction objects to determine the max layers and use this as the value for DataHeatBalance::MaxSolidWinLayers
        // The variable MaxSolidWinLayers is initialized to zero to immediately catch any issues with timing of this routine

        // start by setting this to 5; it will satisfy the regular window constructions (Construction) and the Window5 files
        // (Construction:WindowDataFile)
        DataHeatBalance::MaxSolidWinLayers = 7;

        // Construction:ComplexFenestrationState have a limit of 10 layers, so set it up to 10 if they are present
        if (inputProcessor->getNumObjectsFound(state, "Construction:ComplexFenestrationState") > 0) {
            DataHeatBalance::MaxSolidWinLayers = max(DataHeatBalance::MaxSolidWinLayers, 10);
        }

        // then process the rest of the relevant constructions
        std::string constructName("Construction:WindowEquivalentLayer");
        int numConstructions(inputProcessor->getNumObjectsFound(state, constructName));
        for (int constructionNum = 1; constructionNum <= numConstructions; ++constructionNum) {
            inputProcessor->getObjectItem(state,
                                          constructName,
                                          constructionNum,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            int numLayersInThisConstruct(NumAlpha - 1);
            DataHeatBalance::MaxSolidWinLayers = max(DataHeatBalance::MaxSolidWinLayers, numLayersInThisConstruct);
        }

        // construction types being ignored as they are opaque: Construction:CfactorUndergroundWall, Construction:FfactorGroundFloor,
    }

    void GetProjectControlData(EnergyPlusData &state, bool &ErrorsFound) // Set to true if errors detected during getting data
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the project control data before the rest of the building data (such as
        // materials) is obtained.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // This routine gets the following objects:
        // BUILDING
        // INSIDE CONVECTION ALGORITHM
        // OUTSIDE CONVECTION ALGORITHM
        // SOLUTION ALGORITHM
        // ASHRAE Handbook of Fundamentals, Chap 16, for the setting of Site Atmospheric defaults based
        //   on terrain.
        // ZoneAirHeatBalanceAlgorithm, Added by L. Gu, 12/09
        // ZoneAirContaminantBalance, Added by L. Gu, 06/10

        // Using/Aliasing
        using DataHVACGlobals::HVACSystemRootFinding;
        using DataSystemVariables::lMinimalShadowing;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetProjectControlData: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string AlphaName(4);
        Array1D<Real64> BuildingNumbers(5);
        int NumAlpha;
        int NumNumber;
        int IOStat;
        int NumObjects;
        std::string::size_type TMP;

        // Assign the values to the building data

        CurrentModuleObject = "Building";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // Building Name (remove certain characters)
            BuildingName = AlphaName(1);
            TMP = index(BuildingName, char(1));
            while (TMP != std::string::npos) {
                BuildingName[TMP] = ',';
                TMP = index(BuildingName, char(1));
            }
            TMP = index(BuildingName, char(2));
            while (TMP != std::string::npos) {
                BuildingName[TMP] = '!';
                TMP = index(BuildingName, char(2));
            }
            TMP = index(BuildingName, char(3));
            while (TMP != std::string::npos) {
                BuildingName[TMP] = '\\';
                TMP = index(BuildingName, char(3));
            }
            // Building Azimuth (no validation)
            BuildingAzimuth = mod(BuildingNumbers(1), 360.0);
            // Terrain
            if (AlphaName(2) == "COUNTRY" || AlphaName(2) == "1") {
                state.dataEnvrn->SiteWindExp = 0.14;
                state.dataEnvrn->SiteWindBLHeight = 270.0;
                AlphaName(2) = "Country";
            } else if (AlphaName(2) == "SUBURBS" || AlphaName(2) == "2" || AlphaName(2) == "SUBURB") {
                state.dataEnvrn->SiteWindExp = 0.22;
                state.dataEnvrn->SiteWindBLHeight = 370.0;
                AlphaName(2) = "Suburbs";
            } else if (AlphaName(2) == "CITY" || AlphaName(2) == "3") {
                state.dataEnvrn->SiteWindExp = 0.33;
                state.dataEnvrn->SiteWindBLHeight = 460.0;
                AlphaName(2) = "City";
            } else if (AlphaName(2) == "OCEAN") {
                state.dataEnvrn->SiteWindExp = 0.10;
                state.dataEnvrn->SiteWindBLHeight = 210.0;
                AlphaName(2) = "Ocean";
            } else if (AlphaName(2) == "URBAN") {
                state.dataEnvrn->SiteWindExp = 0.22;
                state.dataEnvrn->SiteWindBLHeight = 370.0;
                AlphaName(2) = "Urban";
            } else {
                ShowSevereError(state, RoutineName + CurrentModuleObject + ": " + cAlphaFieldNames(2) + " invalid=" + AlphaName(2));
                state.dataEnvrn->SiteWindExp = 0.14;
                state.dataEnvrn->SiteWindBLHeight = 270.0;
                AlphaName(2) = AlphaName(2) + "-invalid";
                ErrorsFound = true;
            }
            // Loads Convergence Tolerance Value
            LoadsConvergTol = BuildingNumbers(2);
            if (LoadsConvergTol <= 0.0) {
                ShowSevereError(state,
                                format("{}{}: {} value invalid, [{:.3R}]", RoutineName, CurrentModuleObject, cNumericFieldNames(2), LoadsConvergTol));
                ErrorsFound = true;
            }
            // Temperature Convergence Tolerance Value
            TempConvergTol = BuildingNumbers(3);
            if (TempConvergTol <= 0.0) {
                ShowSevereError(state,
                                format("{}{}: {} value invalid, [{:.3R}]", RoutineName, CurrentModuleObject, cNumericFieldNames(2), TempConvergTol));
                ErrorsFound = true;
            }
            // Solar Distribution
            if (has_prefix(AlphaName(3), "MIN") || AlphaName(3) == "-1" || lMinimalShadowing) {
                SolarDistribution = MinimalShadowing;
                AlphaName(3) = "MinimalShadowing";
                CalcSolRefl = false;
            } else if (AlphaName(3) == "FULLEXTERIOR" || AlphaName(3) == "0") {
                SolarDistribution = FullExterior;
                AlphaName(3) = "FullExterior";
                CalcSolRefl = false;
            } else if (AlphaName(3) == "FULLINTERIORANDEXTERIOR" || AlphaName(3) == "1") {
                SolarDistribution = FullInteriorExterior;
                AlphaName(3) = "FullInteriorAndExterior";
                CalcSolRefl = false;
            } else if (AlphaName(3) == "FULLEXTERIORWITHREFLECTIONS") {
                SolarDistribution = FullExterior;
                AlphaName(3) = "FullExteriorWithReflectionsFromExteriorSurfaces";
                CalcSolRefl = true;
            } else if (AlphaName(3) == "FULLINTERIORANDEXTERIORWITHREFLECTIONS") {
                SolarDistribution = FullInteriorExterior;
                AlphaName(3) = "FullInteriorAndExteriorWithReflectionsFromExteriorSurfaces";
                CalcSolRefl = true;
            } else {
                ShowSevereError(state, RoutineName + CurrentModuleObject + ": " + cAlphaFieldNames(3) + " invalid=" + AlphaName(3));
                ErrorsFound = true;
                AlphaName(3) = AlphaName(3) + "-invalid";
            }
            // Maximum Number of Warmup Days
            if (!lNumericFieldBlanks(4)) {
                MaxNumberOfWarmupDays = BuildingNumbers(4);
                if (MaxNumberOfWarmupDays <= 0) {
                    ShowSevereError(state,
                                    format("{}{}: {} invalid, [{}], {} will be used",
                                           RoutineName,
                                           CurrentModuleObject,
                                           cNumericFieldNames(4),
                                           MaxNumberOfWarmupDays,
                                           DefaultMaxNumberOfWarmupDays));
                    MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
                }
            } else {
                MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
            }
            // Minimum Number of Warmup Days
            if (!lNumericFieldBlanks(5)) {
                MinNumberOfWarmupDays = BuildingNumbers(5);
                if (MinNumberOfWarmupDays <= 0) {
                    ShowWarningError(state,
                                     format("{}{}: {} invalid, [{}], {} will be used",
                                            RoutineName,
                                            CurrentModuleObject,
                                            cNumericFieldNames(5),
                                            MinNumberOfWarmupDays,
                                            DefaultMinNumberOfWarmupDays));
                    MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
                }
            } else {
                MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
            }
            if (MinNumberOfWarmupDays > MaxNumberOfWarmupDays) {
                ShowWarningError(state,
                                 format("{}{}: {} [{}]  is greater than {} [{}], {} will be used.",
                                        RoutineName,
                                        CurrentModuleObject,
                                        cNumericFieldNames(5),
                                        MinNumberOfWarmupDays,
                                        cNumericFieldNames(4),
                                        MaxNumberOfWarmupDays,
                                        MinNumberOfWarmupDays));
                MaxNumberOfWarmupDays = MinNumberOfWarmupDays;
            }

        } else {
            ShowSevereError(state, RoutineName + " A " + CurrentModuleObject + " Object must be entered.");
            ErrorsFound = true;
            BuildingName = "NOT ENTERED";
            AlphaName(2) = "NOT ENTERED";
            AlphaName(3) = "NOT ENTERED";
            MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
            MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
        }

        static constexpr auto Format_720(" Building Information,{},{:.3R},{},{:.5R},{:.5R},{},{},{}\n");
        static constexpr auto Format_721("! <Building Information>, Building Name,North Axis {{deg}},Terrain,  Loads Convergence Tolerance "
                                         "Value,Temperature Convergence Tolerance Value,  Solar Distribution,Maximum Number of Warmup Days,Minimum "
                                         "Number of Warmup Days\n");
        // Write Building Information to the initialization output file
        print(state.files.eio, Format_721);
        print(state.files.eio,
              Format_720,
              BuildingName,
              BuildingAzimuth,
              AlphaName(2),
              LoadsConvergTol,
              TempConvergTol,
              AlphaName(3),
              MaxNumberOfWarmupDays,
              MinNumberOfWarmupDays);
        // Above should be validated...

        CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            {
                auto const SELECT_CASE_var(AlphaName(1));

                if (SELECT_CASE_var == "SIMPLE") {
                    DefaultInsideConvectionAlgo = ASHRAESimple;
                    AlphaName(1) = "Simple";

                } else if ((SELECT_CASE_var == "TARP")) {
                    DefaultInsideConvectionAlgo = ASHRAETARP;
                    AlphaName(1) = "TARP";

                } else if (SELECT_CASE_var == "CEILINGDIFFUSER") {
                    DefaultInsideConvectionAlgo = CeilingDiffuser;
                    AlphaName(1) = "CeilingDiffuser";

                } else if (SELECT_CASE_var == "TROMBEWALL") {
                    DefaultInsideConvectionAlgo = TrombeWall;
                    ShowSevereError(state,
                                    "GetInsideConvectionAlgorithm: TrombeWall has been used as a global definition. This is a zone oriented value.  "
                                    "Will be illegal in the future.");
                    AlphaName(1) = "TrombeWall";

                } else if (SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM") {
                    DefaultInsideConvectionAlgo = AdaptiveConvectionAlgorithm;
                    AlphaName(1) = "AdaptiveConvectionAlgorithm";

                } else if (SELECT_CASE_var == "ASTMC1340") {
                    DefaultInsideConvectionAlgo = ASTMC1340;
                    AlphaName(1) = "ASTMC1340";

                } else {
                    ShowWarningError(state,
                                     "GetInsideConvectionAlgorithm: Invalid value for " + CurrentModuleObject +
                                         ", defaulting to TARP, invalid value=" + AlphaName(1));
                    DefaultInsideConvectionAlgo = ASHRAETARP;
                    AlphaName(1) = "TARP";
                }
            }
        } else {
            // default value, if not specified
            DefaultInsideConvectionAlgo = ASHRAETARP;
            AlphaName(1) = "TARP";
        }
        static constexpr auto Format_722("! <Inside Convection Algorithm>, Algorithm {{Simple | TARP | CeilingDiffuser | "
                                         "AdaptiveConvectionAlgorithm}}\nInside Convection Algorithm,{}\n");
        print(state.files.eio, Format_722, AlphaName(1));

        // Get only the first (if more were input)
        CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          "SurfaceConvectionAlgorithm:Outside",
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            {
                auto const SELECT_CASE_var(AlphaName(1));

                if ((SELECT_CASE_var == "SIMPLECOMBINED")) {
                    DefaultOutsideConvectionAlgo = ASHRAESimple;
                    AlphaName(1) = "SimpleCombined";

                } else if ((SELECT_CASE_var == "TARP")) {
                    DefaultOutsideConvectionAlgo = ASHRAETARP;
                    AlphaName(1) = "TARP";

                } else if (SELECT_CASE_var == "MOWITT") {
                    DefaultOutsideConvectionAlgo = MoWiTTHcOutside;
                    AlphaName(1) = "MoWitt";

                } else if ((SELECT_CASE_var == "DOE-2")) {
                    DefaultOutsideConvectionAlgo = DOE2HcOutside;
                    AlphaName(1) = "DOE-2";

                } else if (SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM") {
                    DefaultOutsideConvectionAlgo = AdaptiveConvectionAlgorithm;
                    AlphaName(1) = "AdaptiveConvectionAlgorithm";

                } else {
                    ShowWarningError(state,
                                     "GetOutsideConvectionAlgorithm: Invalid value for " + CurrentModuleObject +
                                         ", defaulting to DOE-2, invalid value=" + AlphaName(1));
                    DefaultOutsideConvectionAlgo = DOE2HcOutside;
                    AlphaName(1) = "DOE-2";
                }
            }
        } else {
            // default value, if not specified
            DefaultOutsideConvectionAlgo = DOE2HcOutside;
            AlphaName(1) = "DOE-2";
        }

        static constexpr auto Format_723("! <Outside Convection Algorithm>, Algorithm {{SimpleCombined | TARP | MoWitt | DOE-2 | "
                                         "AdaptiveConvectionAlgorithm}}\nOutside Convection Algorithm,{}\n");
        print(state.files.eio, Format_723, AlphaName(1));

        CurrentModuleObject = "HeatBalanceAlgorithm";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            {
                auto const SELECT_CASE_var(AlphaName(1));
                // The default is CTF
                if (SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION") {
                    OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;

                } else if (SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION") {
                    OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_EMPD;
                    DataHeatBalance::AnyEMPD = true;
                    DataHeatBalance::AllCTF = false;
                } else if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                    OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_CondFD;
                    DataHeatBalance::AnyCondFD = true;
                    DataHeatBalance::AllCTF = false;
                    if (state.dataGlobal->NumOfTimeStepInHour < 20) {
                        ShowSevereError(
                            state,
                            format("GetSolutionAlgorithm: {} {} is Conduction Finite Difference but Number of TimeSteps in Hour < 20, Value is {}.",
                                   CurrentModuleObject,
                                   cAlphaFieldNames(1),
                                   state.dataGlobal->NumOfTimeStepInHour));
                        ShowContinueError(state,
                                          "...Suggested minimum number of time steps in hour for Conduction Finite Difference solutions is 20. "
                                          "Errors or inaccurate calculations may occur.");
                    }

                } else if (SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT") {
                    OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_HAMT;
                    DataHeatBalance::AnyHAMT = true;
                    DataHeatBalance::AllCTF = false;
                    if (state.dataGlobal->NumOfTimeStepInHour < 20) {
                        ShowSevereError(state,
                                        format("GetSolutionAlgorithm: {} {} is Combined Heat and Moisture Finite Element but Number of TimeSteps in "
                                               "Hour < 20, Value is {}.",
                                               CurrentModuleObject,
                                               cAlphaFieldNames(1),
                                               state.dataGlobal->NumOfTimeStepInHour));
                        ShowContinueError(state,
                                          "...Suggested minimum number of time steps in hour for Combined Heat and Moisture Finite Element solutions "
                                          "is 20. Errors or inaccurate calculations may occur.");
                        ShowContinueError(state,
                                          "...If the simulation crashes, look at material properties (esp porosity), use timestep=60, or less layers "
                                          "in your constructions.");
                    }

                } else {
                    OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;
                }
            }

            if (NumNumber > 0) {
                MaxSurfaceTempLimit = BuildingNumbers(1);
                MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
                if (MaxSurfaceTempLimit < MinSurfaceTempLimit) {
                } else if (MaxSurfaceTempLimit < 0.0) {
                    MaxSurfaceTempLimit = DefaultSurfaceTempLimit;
                    MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
                }
            }

            if (!lNumericFieldBlanks(2)) {
                LowHConvLimit = BuildingNumbers(2);
            }
            if (!lNumericFieldBlanks(3)) {
                HighHConvLimit = BuildingNumbers(3);
            }

        } else {
            OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel_CTF;
            DataHeatBalance::AnyCTF = true;
            MaxSurfaceTempLimit = DefaultSurfaceTempLimit;
            MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
        }

        // algorithm input checks now deferred until surface properties are read in,
        //  moved to SurfaceGeometry.cc routine GetSurfaceHeatTransferAlgorithmOverrides

        static constexpr auto Format_724("! <Sky Radiance Distribution>, Value {{Anisotropic}}\nSky Radiance Distribution,Anisotropic\n");
        print(state.files.eio, Format_724);

        CurrentModuleObject = "Compliance:Building";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // Building Rotation for Appendix G
            BuildingRotationAppendixG = mod(BuildingNumbers(1), 360.0);
        }

        // A new object is added by L. Gu, 12/09
        CurrentModuleObject = "ZoneAirHeatBalanceAlgorithm";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (NumAlpha > 0) {
                {
                    auto const SELECT_CASE_var(AlphaName(1));
                    if (SELECT_CASE_var == "THIRDORDERBACKWARDDIFFERENCE") {
                        ZoneAirSolutionAlgo = Use3rdOrder;
                        AlphaName(1) = "ThirdOrderBackwardDifference";
                    } else if (SELECT_CASE_var == "ANALYTICALSOLUTION") {
                        ZoneAirSolutionAlgo = UseAnalyticalSolution;
                        AlphaName(1) = "AnalyticalSolution";
                    } else if (SELECT_CASE_var == "EULERMETHOD") {
                        ZoneAirSolutionAlgo = UseEulerMethod;
                        AlphaName(1) = "EulerMethod";
                    } else {
                        ZoneAirSolutionAlgo = Use3rdOrder;
                        AlphaName(1) = "ThirdOrderBackwardDifference";
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(1) +
                                             ". The default choice is assigned = " + AlphaName(1));
                        ShowContinueError(state, "Valid choices are: ThirdOrderBackwardDifference, AnalyticalSolution, or EulerMethod.");
                    }
                }
            }
        } else {
            ZoneAirSolutionAlgo = Use3rdOrder;
            AlphaName(1) = "ThirdOrderBackwardDifference";
        }
        if (DataHeatBalance::OverrideZoneAirSolutionAlgo) {
            ZoneAirSolutionAlgo = UseEulerMethod;
            AlphaName(1) = "EulerMethod";
        }

        // Write Solution Algorithm to the initialization output file for User Verification
        static constexpr auto Format_726(
            "! <Zone Air Solution Algorithm>, Value {{ThirdOrderBackwardDifference | AnalyticalSolution | EulerMethod}}\n");
        print(state.files.eio, Format_726);
        static constexpr auto Format_727(" Zone Air Solution Algorithm, {}\n");
        print(state.files.eio, Format_727, AlphaName(1));

        // A new object is added by L. Gu, 06/10
        CurrentModuleObject = "ZoneAirContaminantBalance";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (NumAlpha > 0) {
                {
                    auto const SELECT_CASE_var(AlphaName(1));
                    if (SELECT_CASE_var == "YES") {
                        state.dataContaminantBalance->Contaminant.CO2Simulation = true;
                        state.dataContaminantBalance->Contaminant.SimulateContaminants = true;
                    } else if (SELECT_CASE_var == "NO") {
                        state.dataContaminantBalance->Contaminant.CO2Simulation = false;
                    } else {
                        state.dataContaminantBalance->Contaminant.CO2Simulation = false;
                        AlphaName(1) = "NO";
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(1) + ". The default choice is assigned = NO");
                    }
                }
            }
            if (NumAlpha == 1 && state.dataContaminantBalance->Contaminant.CO2Simulation) {
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    ShowSevereError(state, CurrentModuleObject + ", " + cAlphaFieldNames(2) + " is required and not given.");
                    ErrorsFound = true;
                }
            } else if (NumAlpha > 1 && state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr = GetScheduleIndex(state, AlphaName(2));
                if (state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr == 0) {
                    ShowSevereError(state, CurrentModuleObject + ", " + cAlphaFieldNames(2) + " not found: " + AlphaName(2));
                    ErrorsFound = true;
                }
            }
            if (NumAlpha > 2) {
                {
                    auto const SELECT_CASE_var(AlphaName(3));
                    if (SELECT_CASE_var == "YES") {
                        state.dataContaminantBalance->Contaminant.GenericContamSimulation = true;
                        if (!state.dataContaminantBalance->Contaminant.CO2Simulation)
                            state.dataContaminantBalance->Contaminant.SimulateContaminants = true;
                    } else if (SELECT_CASE_var == "NO") {
                        state.dataContaminantBalance->Contaminant.GenericContamSimulation = false;
                    } else {
                        state.dataContaminantBalance->Contaminant.GenericContamSimulation = false;
                        AlphaName(3) = "NO";
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(3) + ". The default choice is assigned = NO");
                    }
                }
                if (NumAlpha == 3 && state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        ShowSevereError(state, CurrentModuleObject + ", " + cAlphaFieldNames(4) + " is required and not given.");
                        ErrorsFound = true;
                    }
                } else if (NumAlpha > 3 && state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr = GetScheduleIndex(state, AlphaName(4));
                    if (state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr == 0) {
                        ShowSevereError(state, CurrentModuleObject + ", " + cAlphaFieldNames(4) + " not found: " + AlphaName(4));
                        ErrorsFound = true;
                    }
                }
            }
        } else {
            state.dataContaminantBalance->Contaminant.SimulateContaminants = false;
            state.dataContaminantBalance->Contaminant.CO2Simulation = false;
            state.dataContaminantBalance->Contaminant.GenericContamSimulation = false;
            AlphaName(1) = "NO";
            AlphaName(3) = "NO";
        }

        WindowManager::initWindowModel(state);

        static constexpr auto Format_728("! <Zone Air Carbon Dioxide Balance Simulation>, Simulation {{Yes/No}}, Carbon Dioxide Concentration\n");
        print(state.files.eio, Format_728);
        static constexpr auto Format_730(" Zone Air Carbon Dioxide Balance Simulation, {},{}\n");
        if (state.dataContaminantBalance->Contaminant.SimulateContaminants && state.dataContaminantBalance->Contaminant.CO2Simulation) {
            print(state.files.eio, Format_730, "Yes", AlphaName(1));
        } else {
            print(state.files.eio, Format_730, "No", "N/A");
        }

        static constexpr auto Format_729(
            "! <Zone Air Generic Contaminant Balance Simulation>, Simulation {{Yes/No}}, Generic Contaminant Concentration\n");
        static constexpr auto Format_731(" Zone Air Generic Contaminant Balance Simulation, {},{}\n");
        print(state.files.eio, Format_729);
        if (state.dataContaminantBalance->Contaminant.SimulateContaminants && state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            print(state.files.eio, Format_731, "Yes", AlphaName(3));
        } else {
            print(state.files.eio, Format_731, "No", "N/A");
        }

        // A new object is added by B. Nigusse, 02/14
        CurrentModuleObject = "ZoneAirMassFlowConservation";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        ZoneAirMassFlow.EnforceZoneMassBalance = false;

        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (NumAlpha > 0) {
                {
                    auto const SELECT_CASE_var(AlphaName(1));
                    if (SELECT_CASE_var == "YES") {
                        ZoneAirMassFlow.BalanceMixing = true;
                        ZoneAirMassFlow.EnforceZoneMassBalance = true;
                        AlphaName(1) = "Yes";
                    } else if (SELECT_CASE_var == "NO") {
                        ZoneAirMassFlow.BalanceMixing = false;
                        AlphaName(1) = "No";
                    } else {
                        ZoneAirMassFlow.BalanceMixing = false;
                        AlphaName(1) = "No";
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(1) + ". The default choice is assigned = No");
                    }
                }
            }
            if (NumAlpha > 1) {
                {
                    auto const SELECT_CASE_var(AlphaName(2));
                    if (SELECT_CASE_var == "ADDINFILTRATIONFLOW") {
                        ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
                        ZoneAirMassFlow.EnforceZoneMassBalance = true;
                        AlphaName(2) = "AddInfiltrationFlow";
                        if (!state.dataContaminantBalance->Contaminant.CO2Simulation)
                            state.dataContaminantBalance->Contaminant.SimulateContaminants = true;
                    } else if (SELECT_CASE_var == "ADJUSTINFILTRATIONFLOW") {
                        ZoneAirMassFlow.InfiltrationTreatment = AdjustInfiltrationFlow;
                        ZoneAirMassFlow.EnforceZoneMassBalance = true;
                        AlphaName(2) = "AddInfiltrationFlow";
                        if (!state.dataContaminantBalance->Contaminant.CO2Simulation)
                            state.dataContaminantBalance->Contaminant.SimulateContaminants = true;
                    } else if (SELECT_CASE_var == "NONE") {
                        ZoneAirMassFlow.InfiltrationTreatment = NoInfiltrationFlow;
                        AlphaName(2) = "None";
                    } else {
                        ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
                        ZoneAirMassFlow.EnforceZoneMassBalance = true;
                        AlphaName(2) = "AddInfiltrationFlow";
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(2) +
                                             ". The default choice is assigned = AddInfiltrationFlow");
                    }
                }
            } else {
                ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
                ZoneAirMassFlow.EnforceZoneMassBalance = true;
                AlphaName(2) = "AddInfiltrationFlow";
            }
            if (ZoneAirMassFlow.InfiltrationTreatment == NoInfiltrationFlow) {
                AlphaName(3) = "N/A";
            } else {
                if (NumAlpha > 2) {
                    {
                        auto const SELECT_CASE_var(AlphaName(3));
                        if (SELECT_CASE_var == "MIXINGSOURCEZONESONLY") {
                            ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
                            AlphaName(3) = "MixingSourceZonesOnly";
                        } else if (SELECT_CASE_var == "ALLZONES") {
                            ZoneAirMassFlow.InfiltrationZoneType = AllZones;
                            AlphaName(3) = "AllZones";
                        } else {
                            ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
                            AlphaName(3) = "MixingSourceZonesOnly";
                            ShowWarningError(state,
                                             CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(3) +
                                                 ". The default choice is assigned = MixingSourceZonesOnly");
                        }
                    }
                } else {
                    ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
                    AlphaName(3) = "MixingSourceZonesOnly";
                }
            }
        } else {
            ZoneAirMassFlow.EnforceZoneMassBalance = false;
        }

        static constexpr auto Format_732(
            "! <Zone Air Mass Flow Balance Simulation>, Enforce Mass Balance, Adjust Zone Mixing, Adjust Zone Infiltration "
            "{{AddInfiltration | AdjustInfiltration | None}}, Infiltration Zones {{MixingSourceZonesOnly | AllZones}}\n");
        static constexpr auto Format_733(" Zone Air Mass Flow Balance Simulation, {},{},{},{}\n");

        print(state.files.eio, Format_732);
        if (ZoneAirMassFlow.EnforceZoneMassBalance) {
            print(state.files.eio, Format_733, "Yes", AlphaName(1), AlphaName(2), AlphaName(3));
        } else {
            print(state.files.eio, Format_733, "No", "N/A", "N/A", "N/A");
        }

        // A new object is added by L. Gu, 4/17
        CurrentModuleObject = "HVACSystemRootFindingAlgorithm";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (NumObjects > 0) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaName,
                                          NumAlpha,
                                          BuildingNumbers,
                                          NumNumber,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (NumAlpha > 0) {
                HVACSystemRootFinding.Algorithm = AlphaName(1);
                {
                    auto const SELECT_CASE_var(AlphaName(1));
                    if ((SELECT_CASE_var == "REGULAFALSI")) {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi;
                    } else if (SELECT_CASE_var == "BISECTION") {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::Bisection;
                    } else if (SELECT_CASE_var == "BISECTIONTHENREGULAFALSI") {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi;
                    } else if (SELECT_CASE_var == "REGULAFALSITHENBISECTION") {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection;
                    } else if (SELECT_CASE_var == "ALTERNATION") {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::Alternation;
                    } else {
                        HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi;
                        ShowWarningError(state,
                                         CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(1) +
                                             ". The default choice is assigned = " + AlphaName(1));
                        ShowContinueError(
                            state, "Valid choices are: RegulaFalsi, Bisection, BisectionThenRegulaFalsi, RegulaFalsiThenBisection, or Alternation.");
                    }
                }
            }
            if (NumNumber > 0) {
                HVACSystemRootFinding.NumOfIter = BuildingNumbers(1);
            }
        } else {
            HVACSystemRootFinding.Algorithm = "RegulaFalsi";
            HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi;
        }

        // Write Solution Algorithm to the initialization output file for User Verification
        static constexpr auto Format_734(
            "! <HVACSystemRootFindingAlgorithm>, Value {{RegulaFalsi | Bisection | BisectionThenRegulaFalsi | RegulaFalsiThenBisection}}\n");
        static constexpr auto Format_735(" HVACSystemRootFindingAlgorithm, {}\n");
        print(state.files.eio, Format_734);
        print(state.files.eio, Format_735, HVACSystemRootFinding.Algorithm);
    }

    void GetSiteAtmosphereData(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads the input data for the SITE ATMOSPHERIC VARIATION object.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumObjects;
        int NumAlphas;               // Number of elements in the alpha array
        int NumNums;                 // Number of elements in the numeric array
        int IOStat;                  // IO Status when calling get input subroutine
        Array1D_string AlphArray(1); // Character string data
        Array1D<Real64> NumArray(3); // Numeric data

        // Formats
        static constexpr auto Format_720("Environment:Site Atmospheric Variation,{:.3R},{:.3R},{:.6R}\n");


        CurrentModuleObject = "Site:HeightVariation";
        NumObjects = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        if (NumObjects == 1) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (NumNums > 0) state.dataEnvrn->SiteWindExp = NumArray(1);
            if (NumNums > 1) state.dataEnvrn->SiteWindBLHeight = NumArray(2);
            if (NumNums > 2) state.dataEnvrn->SiteTempGradient = NumArray(3);

        } else if (NumObjects > 1) {
            ShowSevereError(state, "Too many " + CurrentModuleObject + " objects, only 1 allowed.");
            ErrorsFound = true;
        } else { //  None entered
            // IDD defaults would have this:
            // Building object defaults use Terrain to set SiteWindExp and SiteWindBLHeight but would
            // be overridden by a Site Atmospheric Variation Object.
            // SiteWindExp = 0.22
            // SiteWindBLHeight = 370.0
            state.dataEnvrn->SiteTempGradient = 0.0065;
        }

        // Write to the initialization output file
        print(state.files.eio,
              "! <Environment:Site Atmospheric Variation>,Wind Speed Profile Exponent {{}},Wind Speed Profile Boundary "
              "Layer Thickness {{m}},Air Temperature Gradient Coefficient {{K/m}}\n");

        print(state.files.eio, Format_720, state.dataEnvrn->SiteWindExp, state.dataEnvrn->SiteWindBLHeight, state.dataEnvrn->SiteTempGradient);
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

        using CurveManager::GetCurveIndex;
        using CurveManager::GetCurveMinMaxValues;

        using General::ScanForReports;

        // if this has a size, then input has already been gotten
        if (UniqueMaterialNames.size()) {
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

        int EcoRoofMat; // Materials for ecoRoof
        int NumGas;     // Index for loop over gap gases in a mixture
        int NumGases;   // Number of gasses in a mixture
        int GasType;    // Gas type index: 1=air, 2=argon, 3=krypton, 4=xenon
        int Loop;
        int ICoeff;            // Gas property coefficient index
        std::string TypeOfGas; // Type of window gas fill (Air, Argon, Krypton, &
        // Xenon, or Custom
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
        static int iTC(0);
        static int iMat(0);

        // Added TH 7/27/2009 for constructions defined with F or C factor method
        int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
        int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

        std::string RoutineName("GetMaterialData: ");

        RegMat = inputProcessor->getNumObjectsFound(state, "Material");
        RegRMat = inputProcessor->getNumObjectsFound(state, "Material:NoMass");
        IRTMat = inputProcessor->getNumObjectsFound(state, "Material:InfraredTransparent");
        AirMat = inputProcessor->getNumObjectsFound(state, "Material:AirGap");
        W5GlsMat = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Glazing");
        W5GlsMatAlt = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Glazing:RefractionExtinctionMethod");
        W5GasMat = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Gas");
        W5GasMatMixture = inputProcessor->getNumObjectsFound(state, "WindowMaterial:GasMixture");
        TotShades = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Shade");
        TotComplexShades = inputProcessor->getNumObjectsFound(state, "WindowMaterial:ComplexShade");
        TotComplexGaps = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Gap");
        TotScreens = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Screen");
        TotBlinds = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Blind");
        EcoRoofMat = inputProcessor->getNumObjectsFound(state, "Material:RoofVegetation");
        TotSimpleWindow = inputProcessor->getNumObjectsFound(state, "WindowMaterial:SimpleGlazingSystem");

        W5GlsMatEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Glazing:EquivalentLayer");
        TotShadesEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Shade:EquivalentLayer");
        TotDrapesEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Drape:EquivalentLayer");
        TotBlindsEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Blind:EquivalentLayer");
        TotScreensEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Screen:EquivalentLayer");
        W5GapMatEQL = inputProcessor->getNumObjectsFound(state, "WindowMaterial:Gap:EquivalentLayer");

        TotMaterials = RegMat + RegRMat + AirMat + W5GlsMat + W5GlsMatAlt + W5GasMat + W5GasMatMixture + TotShades + TotScreens + TotBlinds +
                       EcoRoofMat + IRTMat + TotSimpleWindow + TotComplexShades + TotComplexGaps + W5GlsMatEQL + TotShadesEQL + TotDrapesEQL +
                       TotBlindsEQL + TotScreensEQL + W5GapMatEQL;

        TotFfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:FfactorGroundFloor");
        TotCfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:CfactorUndergroundWall");

        if (TotFfactorConstructs > 0) {
            NoFfactorConstructionsUsed = false;
        }

        if (TotCfactorConstructs > 0) {
            NoCfactorConstructionsUsed = false;
        }

        if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
            // Add a new fictitious insulation layer and a thermal mass layer for each F or C factor defined construction
            TotMaterials += 1 + TotFfactorConstructs + TotCfactorConstructs;
        }

        state.dataMaterial->Material.allocate(TotMaterials); // Allocate the array Size to the number of materials
        UniqueMaterialNames.reserve(static_cast<unsigned>(TotMaterials));

        NominalR.dimension(TotMaterials, 0.0);

        MaterNum = 0;

        // Regular Materials

        CurrentModuleObject = "Material";
        for (Loop = 1; Loop <= RegMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }
            // Load the material derived type from the input data.
            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = RegularMaterial;
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);

            ValidateMaterialRoughness(state, MaterNum, MaterialNames(2), ErrorsFound);

            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).Density = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).SpecHeat = MaterialProps(4);
            // min fields is 6 -- previous four will be there
            if (MaterialNumProp >= 5) {
                state.dataMaterial->Material(MaterNum).AbsorpThermal = MaterialProps(5);
                state.dataMaterial->Material(MaterNum).AbsorpThermalInput = MaterialProps(5);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpThermal = 0.9;
                state.dataMaterial->Material(MaterNum).AbsorpThermalInput = 0.9;
            }
            if (MaterialNumProp >= 6) {
                state.dataMaterial->Material(MaterNum).AbsorpSolar = MaterialProps(6);
                state.dataMaterial->Material(MaterNum).AbsorpSolarInput = MaterialProps(6);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpSolar = 0.7;
                state.dataMaterial->Material(MaterNum).AbsorpSolarInput = 0.7;
            }
            if (MaterialNumProp >= 7) {
                state.dataMaterial->Material(MaterNum).AbsorpVisible = MaterialProps(7);
                state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = MaterialProps(7);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpVisible = 0.7;
                state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = 0.7;
            }

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                state.dataMaterial->Material(MaterNum).Resistance = NominalR(MaterNum);
            } else {
                ShowSevereError(state, "Positive thermal conductivity required for material " + state.dataMaterial->Material(MaterNum).Name);
                ErrorsFound = true;
            }
        }

        // Add the 6" heavy concrete for constructions defined with F or C factor method
        if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
            ++MaterNum;

            state.dataMaterial->Material(MaterNum).Group = RegularMaterial;
            state.dataMaterial->Material(MaterNum).Name = "~FC_Concrete";
            state.dataMaterial->Material(MaterNum).Thickness = 0.15;    // m, 0.15m = 6 inches
            state.dataMaterial->Material(MaterNum).Conductivity = 1.95; // W/mK
            state.dataMaterial->Material(MaterNum).Density = 2240.0;    // kg/m3
            state.dataMaterial->Material(MaterNum).SpecHeat = 900.0;    // J/kgK
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).AbsorpSolar = 0.7;
            state.dataMaterial->Material(MaterNum).AbsorpThermal = 0.9;
            state.dataMaterial->Material(MaterNum).AbsorpVisible = 0.7;
            NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
            state.dataMaterial->Material(MaterNum).Resistance = NominalR(MaterNum);

            ++RegMat;
        }

        CurrentModuleObject = "Material:NoMass";
        for (Loop = 1; Loop <= RegRMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            // Load the material derived type from the input data.
            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = RegularMaterial;
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);

            ValidateMaterialRoughness(state, MaterNum, MaterialNames(2), ErrorsFound);

            state.dataMaterial->Material(MaterNum).Resistance = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ROnly = true;
            if (MaterialNumProp >= 2) {
                state.dataMaterial->Material(MaterNum).AbsorpThermal = MaterialProps(2);
                state.dataMaterial->Material(MaterNum).AbsorpThermalInput = MaterialProps(2);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpThermal = 0.9;
                state.dataMaterial->Material(MaterNum).AbsorpThermalInput = 0.9;
            }
            if (MaterialNumProp >= 3) {
                state.dataMaterial->Material(MaterNum).AbsorpSolar = MaterialProps(3);
                state.dataMaterial->Material(MaterNum).AbsorpSolarInput = MaterialProps(3);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpSolar = 0.7;
                state.dataMaterial->Material(MaterNum).AbsorpSolarInput = 0.7;
            }
            if (MaterialNumProp >= 4) {
                state.dataMaterial->Material(MaterNum).AbsorpVisible = MaterialProps(4);
                state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = MaterialProps(4);
            } else {
                state.dataMaterial->Material(MaterNum).AbsorpVisible = 0.7;
                state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = 0.7;
            }

            NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Resistance;
        }

        // Add a fictitious insulation layer for each construction defined with F or C factor method
        if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
            for (Loop = 1; Loop <= TotFfactorConstructs + TotCfactorConstructs; ++Loop) {
                ++MaterNum;
                state.dataMaterial->Material(MaterNum).Group = RegularMaterial;
                state.dataMaterial->Material(MaterNum).Name = format("~FC_Insulation_{}", Loop);
                state.dataMaterial->Material(MaterNum).ROnly = true;
                state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
                state.dataMaterial->Material(MaterNum).AbsorpSolar = 0.0;
                state.dataMaterial->Material(MaterNum).AbsorpThermal = 0.0;
                state.dataMaterial->Material(MaterNum).AbsorpVisible = 0.0;
            }
            RegRMat += TotFfactorConstructs + TotCfactorConstructs;
        }

        // Air Materials (for air spaces in opaque constructions)
        CurrentModuleObject = "Material:AirGap";
        for (Loop = 1; Loop <= AirMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            // Load the material derived type from the input data.
            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = Air;
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);

            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;

            state.dataMaterial->Material(MaterNum).Resistance = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ROnly = true;

            NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Resistance;
        }

        CurrentModuleObject = "Material:InfraredTransparent";
        for (Loop = 1; Loop <= IRTMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = IRTMaterial;

            // Load the material derived type from the input data.
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);

            // Load data for other properties that need defaults
            state.dataMaterial->Material(MaterNum).ROnly = true;
            state.dataMaterial->Material(MaterNum).Resistance = 0.01;
            state.dataMaterial->Material(MaterNum).AbsorpThermal = 0.9999;
            state.dataMaterial->Material(MaterNum).AbsorpThermalInput = 0.9999;
            state.dataMaterial->Material(MaterNum).AbsorpSolar = 1.0;
            state.dataMaterial->Material(MaterNum).AbsorpSolarInput = 1.0;
            state.dataMaterial->Material(MaterNum).AbsorpVisible = 1.0;
            state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = 1.0;

            NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Resistance;
        }

        // Glass materials, regular input: transmittance and front/back reflectance

        CurrentModuleObject = "WindowMaterial:Glazing";
        for (Loop = 1; Loop <= W5GlsMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowGlass;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;
            state.dataMaterial->Material(MaterNum).ROnly = true;
            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            if (!UtilityRoutines::SameString(MaterialNames(2), "SpectralAndAngle")) {
                state.dataMaterial->Material(MaterNum).Trans = MaterialProps(2);
                state.dataMaterial->Material(MaterNum).ReflectSolBeamFront = MaterialProps(3);
                state.dataMaterial->Material(MaterNum).ReflectSolBeamBack = MaterialProps(4);
                state.dataMaterial->Material(MaterNum).TransVis = MaterialProps(5);
                state.dataMaterial->Material(MaterNum).ReflectVisBeamFront = MaterialProps(6);
                state.dataMaterial->Material(MaterNum).ReflectVisBeamBack = MaterialProps(7);
                state.dataMaterial->Material(MaterNum).TransThermal = MaterialProps(8);
            }
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = MaterialProps(10);
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(11);
            state.dataMaterial->Material(MaterNum).GlassTransDirtFactor = MaterialProps(12);
            state.dataMaterial->Material(MaterNum).YoungModulus = MaterialProps(13);
            state.dataMaterial->Material(MaterNum).PoissonsRatio = MaterialProps(14);
            if (MaterialProps(12) == 0.0) state.dataMaterial->Material(MaterNum).GlassTransDirtFactor = 1.0;
            state.dataMaterial->Material(MaterNum).AbsorpThermal = state.dataMaterial->Material(MaterNum).AbsorpThermalBack;

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                state.dataMaterial->Material(MaterNum).Resistance = NominalR(MaterNum);
            } else {
                ErrorsFound = true;
                ShowSevereError(state,
                                "Window glass material " + state.dataMaterial->Material(MaterNum).Name +
                                    " has Conductivity = 0.0, must be >0.0, default = .9");
            }

            state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;
            if (TotSpectralData > 0 && !lAlphaFieldBlanks(3)) {
                state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = UtilityRoutines::FindItemInList(MaterialNames(3), SpectralData);
            }
            if (UtilityRoutines::SameString(MaterialNames(2), "SpectralAverage")) state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;
            // No need for spectral data for BSDF either
            if (UtilityRoutines::SameString(MaterialNames(2), "BSDF")) state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;
            if (UtilityRoutines::SameString(MaterialNames(2), "SpectralAndAngle"))
                state.dataMaterial->Material(MaterNum).GlassSpectralAndAngle = true;

            if (state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr == 0 && UtilityRoutines::SameString(MaterialNames(2), "Spectral")) {
                ErrorsFound = true;
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataMaterial->Material(MaterNum).Name + "\" has " + cAlphaFieldNames(2) +
                                    " = Spectral but has no matching MaterialProperty:GlazingSpectralData set");
                if (lAlphaFieldBlanks(3)) {
                    ShowContinueError(state, "..." + cAlphaFieldNames(3) + " is blank.");
                } else {
                    ShowContinueError(state,
                                      "..." + cAlphaFieldNames(3) + "=\"" + MaterialNames(3) +
                                          "\" not found as item in MaterialProperty:GlazingSpectralData objects.");
                }
            }

            if (!UtilityRoutines::SameString(MaterialNames(2), "SpectralAverage") && !UtilityRoutines::SameString(MaterialNames(2), "Spectral") &&
                !UtilityRoutines::SameString(MaterialNames(2), "BSDF") && !UtilityRoutines::SameString(MaterialNames(2), "SpectralAndAngle")) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataMaterial->Material(MaterNum).Name + "\", invalid specification.");
                ShowContinueError(state,
                                  cAlphaFieldNames(2) + " must be SpectralAverage, Spectral, BSDF or SpectralAndAngle, value=" + MaterialNames(2));
            }

            // TH 8/24/2011, allow glazing properties MaterialProps(2 to 10) to equal 0 or 1: 0.0 =< Prop <= 1.0
            // Fixed CR 8413 - modeling spandrel panels as glazing systems
            if (UtilityRoutines::SameString(MaterialNames(2), "SpectralAverage")) {

                if (MaterialProps(2) + MaterialProps(3) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(2) + " + " + cNumericFieldNames(3) + " not <= 1.0");
                }

                if (MaterialProps(2) + MaterialProps(4) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(2) + " + " + cNumericFieldNames(4) + " not <= 1.0");
                }

                if (MaterialProps(5) + MaterialProps(6) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(5) + " + " + cNumericFieldNames(6) + " not <= 1.0");
                }

                if (MaterialProps(5) + MaterialProps(7) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(5) + " + " + cNumericFieldNames(7) + " not <= 1.0");
                }

                if (MaterialProps(8) + MaterialProps(9) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(8) + " + " + cNumericFieldNames(9) + " not <= 1.0");
                }

                if (MaterialProps(8) + MaterialProps(10) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(8) + " + " + cNumericFieldNames(10) + " not <= 1.0");
                }

                if (MaterialProps(2) < 0.0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(2) + " not >= 0.0");
                    ErrorsFound = true;
                }

                if (MaterialProps(2) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(2) + " not <= 1.0");
                }

                if (MaterialProps(3) < 0.0 || MaterialProps(3) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(3) + " not >= 0.0 and <= 1.0");
                }

                if (MaterialProps(4) < 0.0 || MaterialProps(4) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(4) + " not >= 0.0 and <= 1.0");
                }

                if (MaterialProps(5) < 0.0) {
                    ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", minimal value.");
                    ShowWarningError(state, cNumericFieldNames(5) + " not >= 0.0");
                }

                if (MaterialProps(5) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(5) + " not <= 1.0");
                }

                if (MaterialProps(6) < 0.0 || MaterialProps(6) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(6) + " not >= 0.0 and <= 1.0");
                }

                if (MaterialProps(7) < 0.0 || MaterialProps(7) > 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(7) + " not >= 0.0 and <= 1.0");
                }
            }

            if (MaterialProps(8) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(8) + " not <= 1.0");
            }

            if (MaterialProps(9) <= 0.0 || MaterialProps(9) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(9) + " not > 0.0 and < 1.0");
            }

            if (MaterialProps(10) <= 0.0 || MaterialProps(10) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(10) + " not > 0.0 and < 1.0");
            }

            if (MaterialProps(11) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(11) + " not > 0.0");
            }

            if (MaterialProps(13) < 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(13) + " not > 0.0");
            }

            if (MaterialProps(14) < 0.0 || MaterialProps(14) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(14) + " not > 0.0 and < 1.0");
            }

            if (MaterialNames(4) == "") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = false;
            } else if (MaterialNames(4) == "YES") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = true;
            } else if (MaterialNames(4) == "NO") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = false;
            } else {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(4) + " must be Yes or No, entered value=" + MaterialNames(4));
            }
            // Get SpectralAndAngle table names
            if (state.dataMaterial->Material(MaterNum).GlassSpectralAndAngle) {
                if (lAlphaFieldBlanks(5)) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", blank field.");
                    ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
                } else {
                    state.dataMaterial->Material(MaterNum).GlassSpecAngTransDataPtr = CurveManager::GetCurveIndex(state, MaterialNames(5));
                    if (state.dataMaterial->Material(MaterNum).GlassSpecAngTransDataPtr == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Invalid name.");
                        ShowContinueError(state, cAlphaFieldNames(5) + " requires a valid table object name, entered input=" + MaterialNames(5));
                    } else {
                        ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                    state.dataMaterial->Material(MaterNum).GlassSpecAngTransDataPtr, // Curve index
                                                                    {2},                                         // Valid dimensions
                                                                    RoutineName,                                 // Routine name
                                                                    CurrentModuleObject,                         // Object Type
                                                                    state.dataMaterial->Material(MaterNum).Name, // Object Name
                                                                    cAlphaFieldNames(5));                        // Field Name

                        GetCurveMinMaxValues(state,
                                             state.dataMaterial->Material(MaterNum).GlassSpecAngTransDataPtr,
                                             minAngValue,
                                             maxAngValue,
                                             minLamValue,
                                             maxLamValue);
                        if (minAngValue > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), minAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the minumum value = 0.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the maximum value = 90.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (minLamValue < 0.1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), minLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the minumum value = 0.1 micron in the entered table name=" + MaterialNames(5));
                        }
                        if (maxLamValue > 4.0) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the maximum value = 4.0 microns in the entered table name=" + MaterialNames(5));
                        }
                    }
                }
                if (lAlphaFieldBlanks(6)) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", blank field.");
                    ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
                } else {
                    state.dataMaterial->Material(MaterNum).GlassSpecAngFRefleDataPtr = CurveManager::GetCurveIndex(state, MaterialNames(6));
                    if (state.dataMaterial->Material(MaterNum).GlassSpecAngFRefleDataPtr == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Invalid name.");
                        ShowContinueError(state, cAlphaFieldNames(6) + " requires a valid table object name, entered input=" + MaterialNames(6));
                    } else {
                        ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                    state.dataMaterial->Material(MaterNum).GlassSpecAngFRefleDataPtr, // Curve index
                                                                    {2},                                         // Valid dimensions
                                                                    RoutineName,                                 // Routine name
                                                                    CurrentModuleObject,                         // Object Type
                                                                    state.dataMaterial->Material(MaterNum).Name, // Object Name
                                                                    cAlphaFieldNames(6));                        // Field Name

                        GetCurveMinMaxValues(state,
                                             state.dataMaterial->Material(MaterNum).GlassSpecAngFRefleDataPtr,
                                             minAngValue,
                                             maxAngValue,
                                             minLamValue,
                                             maxLamValue);
                        if (minAngValue > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), minAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the minumum value = 0.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the maximum value = 90.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (minLamValue < 0.1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), minLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the minumum value = 0.1 micron in the entered table name=" + MaterialNames(5));
                        }
                        if (maxLamValue > 4.0) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the maximum value = 4.0 microns in the entered table name=" + MaterialNames(5));
                        }
                    }
                }
                if (lAlphaFieldBlanks(7)) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", blank field.");
                    ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
                } else {
                    state.dataMaterial->Material(MaterNum).GlassSpecAngBRefleDataPtr = CurveManager::GetCurveIndex(state, MaterialNames(7));
                    if (state.dataMaterial->Material(MaterNum).GlassSpecAngBRefleDataPtr == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Invalid name.");
                        ShowContinueError(state, cAlphaFieldNames(7) + " requires a valid table object name, entered input=" + MaterialNames(7));
                    } else {
                        ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                                    state.dataMaterial->Material(MaterNum).GlassSpecAngBRefleDataPtr, // Curve index
                                                                    {2},                                         // Valid dimensions
                                                                    RoutineName,                                 // Routine name
                                                                    CurrentModuleObject,                         // Object Type
                                                                    state.dataMaterial->Material(MaterNum).Name, // Object Name
                                                                    cAlphaFieldNames(7));                        // Field Name

                        GetCurveMinMaxValues(state,
                                             state.dataMaterial->Material(MaterNum).GlassSpecAngBRefleDataPtr,
                                             minAngValue,
                                             maxAngValue,
                                             minLamValue,
                                             maxLamValue);
                        if (minAngValue > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), minAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the minumum value = 0.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxAngValue));
                            ShowContinueError(
                                state, cAlphaFieldNames(5) + " requires the maximum value = 90.0 in the entered table name=" + MaterialNames(5));
                        }
                        if (minLamValue < 0.1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), minLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the minumum value = 0.1 micron in the entered table name=" + MaterialNames(5));
                        }
                        if (maxLamValue > 4.0) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state,
                                format(
                                    "{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.", CurrentModuleObject, MaterialNames(1), maxLamValue));
                            ShowContinueError(state,
                                              cAlphaFieldNames(5) +
                                                  " requires the maximum value = 4.0 microns in the entered table name=" + MaterialNames(5));
                        }
                    }
                }
            }
        }

        // Glass materials, alternative input: index of refraction and extinction coefficient

        CurrentModuleObject = "WindowMaterial:Glazing:RefractionExtinctionMethod";
        for (Loop = 1; Loop <= W5GlsMatAlt; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowGlass;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;
            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ROnly = true;

            // Calculate solar and visible transmittance and reflectance at normal incidence from thickness,
            // index of refraction and extinction coefficient. With the alternative input the front and back
            // properties are assumed to be the same.

            ReflectivitySol = pow_2((MaterialProps(2) - 1.0) / (MaterialProps(2) + 1.0));
            ReflectivityVis = pow_2((MaterialProps(4) - 1.0) / (MaterialProps(4) + 1.0));
            TransmittivitySol = std::exp(-MaterialProps(3) * MaterialProps(1));
            TransmittivityVis = std::exp(-MaterialProps(5) * MaterialProps(1));
            state.dataMaterial->Material(MaterNum).Trans =
                TransmittivitySol * pow_2(1.0 - ReflectivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol));
            state.dataMaterial->Material(MaterNum).ReflectSolBeamFront =
                ReflectivitySol *
                (1.0 + pow_2(1.0 - ReflectivitySol) * pow_2(TransmittivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol)));
            state.dataMaterial->Material(MaterNum).ReflectSolBeamBack = state.dataMaterial->Material(MaterNum).ReflectSolBeamFront;
            state.dataMaterial->Material(MaterNum).TransVis =
                TransmittivityVis * pow_2(1.0 - ReflectivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis));

            state.dataMaterial->Material(MaterNum).ReflectVisBeamFront =
                ReflectivityVis *
                (1.0 + pow_2(1.0 - ReflectivityVis) * pow_2(TransmittivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis)));
            state.dataMaterial->Material(MaterNum).ReflectVisBeamBack = state.dataMaterial->Material(MaterNum).ReflectSolBeamFront;
            state.dataMaterial->Material(MaterNum).TransThermal = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).GlassTransDirtFactor = MaterialProps(9);
            if (MaterialProps(9) == 0.0) state.dataMaterial->Material(MaterNum).GlassTransDirtFactor = 1.0;
            state.dataMaterial->Material(MaterNum).AbsorpThermal = state.dataMaterial->Material(MaterNum).AbsorpThermalBack;

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                state.dataMaterial->Material(MaterNum).Resistance = NominalR(MaterNum);
            }

            state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;

            if (MaterialProps(6) + MaterialProps(7) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " + " + cNumericFieldNames(7) + " not < 1.0");
            }

            if (MaterialNames(2) == "") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = false;
            } else if (MaterialNames(2) == "YES") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = true;
            } else if (MaterialNames(2) == "NO") {
                state.dataMaterial->Material(MaterNum).SolarDiffusing = false;
            } else {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(2) + " must be Yes or No, entered value=" + MaterialNames(4));
            }
        }

        // Glass materials, equivalent layer (ASHWAT) method
        CurrentModuleObject = "WindowMaterial:Glazing:EquivalentLayer";
        for (Loop = 1; Loop <= W5GlsMatEQL; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = GlassEquivalentLayer;

            // Load the material derived type from the input data.
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            state.dataMaterial->Material(MaterNum).TausFrontBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausBackBeamBeam = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamBeam = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).ReflBackBeamBeam = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeamVis = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).TausBackBeamBeamVis = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamBeamVis = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).ReflBackBeamBeamVis = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiff = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiff = MaterialProps(10);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff = MaterialProps(11);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiff = MaterialProps(12);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiffVis = MaterialProps(13);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiffVis = MaterialProps(14);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiffVis = MaterialProps(15);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiffVis = MaterialProps(16);
            state.dataMaterial->Material(MaterNum).TausDiffDiff = MaterialProps(17);
            state.dataMaterial->Material(MaterNum).ReflFrontDiffDiff = MaterialProps(18);
            state.dataMaterial->Material(MaterNum).ReflBackDiffDiff = MaterialProps(19);
            state.dataMaterial->Material(MaterNum).TausDiffDiffVis = MaterialProps(20);
            state.dataMaterial->Material(MaterNum).ReflFrontDiffDiffVis = MaterialProps(21);
            state.dataMaterial->Material(MaterNum).ReflBackDiffDiffVis = MaterialProps(22);
            state.dataMaterial->Material(MaterNum).TausThermal = MaterialProps(23);
            state.dataMaterial->Material(MaterNum).EmissThermalFront = MaterialProps(24);
            state.dataMaterial->Material(MaterNum).EmissThermalBack = MaterialProps(25);
            state.dataMaterial->Material(MaterNum).Resistance = MaterialProps(26);
            if (state.dataMaterial->Material(MaterNum).Resistance <= 0.0)
                state.dataMaterial->Material(MaterNum).Resistance = 0.158; // equivalent to single pane of 1/4" inch standard glass
            // Assumes thermal emissivity is the same as thermal absorptance
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = state.dataMaterial->Material(MaterNum).EmissThermalFront;
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = state.dataMaterial->Material(MaterNum).EmissThermalBack;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).TausThermal;

            if (UtilityRoutines::SameString(MaterialNames(2), "SpectralAverage")) state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;

            // IF(dataMaterial.Material(MaterNum)%GlassSpectralDataPtr == 0 .AND. UtilityRoutines::SameString(MaterialNames(2),'Spectral')) THEN
            //  ErrorsFound = .TRUE.
            //  CALL ShowSevereError(state, TRIM(CurrentModuleObject)//'="'//Trim(dataMaterial.Material(MaterNum)%Name)// &
            //        '" has '//TRIM(cAlphaFieldNames(2))//' = Spectral but has no matching MaterialProperty:GlazingSpectralData set')
            //  IF (lAlphaFieldBlanks(3)) THEN
            //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//' is blank.')
            //  ELSE
            //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//'="'//TRIM(MaterialNames(3))//  &
            //       '" not found as item in MaterialProperty:GlazingSpectralData objects.')
            //  END IF
            // END IF

            if (!UtilityRoutines::SameString(MaterialNames(2), "SpectralAverage")) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataMaterial->Material(MaterNum).Name + "\", invalid specification.");
                ShowContinueError(state, cAlphaFieldNames(2) + " must be SpectralAverage, value=" + MaterialNames(2));
            }

        } // W5GlsMatEQL loop

        // Window gas materials (for gaps with a single gas)

        CurrentModuleObject = "WindowMaterial:Gas";
        for (Loop = 1; Loop <= W5GasMat; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowGas;
            state.dataMaterial->Material(MaterNum).GasType(1) = -1;
            state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = 1;
            state.dataMaterial->Material(MaterNum).GasFract(1) = 1.0;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = 1;
            TypeOfGas = MaterialNames(2);
            if (TypeOfGas == "AIR") state.dataMaterial->Material(MaterNum).GasType(1) = 1;
            if (TypeOfGas == "ARGON") state.dataMaterial->Material(MaterNum).GasType(1) = 2;
            if (TypeOfGas == "KRYPTON") state.dataMaterial->Material(MaterNum).GasType(1) = 3;
            if (TypeOfGas == "XENON") state.dataMaterial->Material(MaterNum).GasType(1) = 4;
            if (TypeOfGas == "CUSTOM") state.dataMaterial->Material(MaterNum).GasType(1) = 0;

            if (state.dataMaterial->Material(MaterNum).GasType(1) == -1) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state,
                                  cAlphaFieldNames(2) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, Xenon or Custom.");
            }

            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;

            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ROnly = true;

            GasType = state.dataMaterial->Material(MaterNum).GasType(1);
            if (GasType >= 1 && GasType <= 4) {
                state.dataMaterial->Material(MaterNum).GasWght(1) = GasWght(GasType);
                state.dataMaterial->Material(MaterNum).GasSpecHeatRatio(1) = GasSpecificHeatRatio(GasType);
                for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                    state.dataMaterial->Material(MaterNum).GasCon(ICoeff, 1) = GasCoeffsCon(ICoeff, GasType);
                    state.dataMaterial->Material(MaterNum).GasVis(ICoeff, 1) = GasCoeffsVis(ICoeff, GasType);
                    state.dataMaterial->Material(MaterNum).GasCp(ICoeff, 1) = GasCoeffsCp(ICoeff, GasType);
                }
            }

            // Custom gas

            if (GasType == 0) {
                for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                    state.dataMaterial->Material(MaterNum).GasCon(ICoeff, 1) = MaterialProps(1 + ICoeff);
                    state.dataMaterial->Material(MaterNum).GasVis(ICoeff, 1) = MaterialProps(4 + ICoeff);
                    state.dataMaterial->Material(MaterNum).GasCp(ICoeff, 1) = MaterialProps(7 + ICoeff);
                }
                state.dataMaterial->Material(MaterNum).GasWght(1) = MaterialProps(11);
                state.dataMaterial->Material(MaterNum).GasSpecHeatRatio(1) = MaterialProps(12);

                // Check for errors in custom gas properties
                //      IF(dataMaterial.Material(MaterNum)%GasCon(1,1) <= 0.0) THEN
                //        ErrorsFound = .TRUE.
                //        CALL ShowSevereError(state, 'Conductivity Coefficient A for custom window gas='&
                //                 //TRIM(MaterialNames(1))//' should be > 0.')
                //      END IF

                if (state.dataMaterial->Material(MaterNum).GasVis(1, 1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(3 + ICoeff) + " not > 0.0");
                }
                if (state.dataMaterial->Material(MaterNum).GasCp(1, 1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(5 + ICoeff) + " not > 0.0");
                }
                if (state.dataMaterial->Material(MaterNum).GasWght(1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(8) + " not > 0.0");
                }
            }

            // Nominal resistance of gap at room temperature
            if (!ErrorsFound) {
                DenomRGas = (state.dataMaterial->Material(MaterNum).GasCon(1, 1) + state.dataMaterial->Material(MaterNum).GasCon(2, 1) * 300.0 +
                             state.dataMaterial->Material(MaterNum).GasCon(3, 1) * 90000.0);
                if (DenomRGas > 0.0) {
                    NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / DenomRGas;
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(
                        state, format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                    ErrorsFound = true;
                }
            }
        }

        // Window gap materials (for gaps with a single gas for EquivalentLayer)

        CurrentModuleObject = "WindowMaterial:Gap:EquivalentLayer";
        for (Loop = 1; Loop <= W5GapMatEQL; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = GapEquivalentLayer;
            state.dataMaterial->Material(MaterNum).GasType(1) = -1;
            state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = 1;
            state.dataMaterial->Material(MaterNum).GasFract(1) = 1.0;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = 1;
            TypeOfGas = MaterialNames(2);
            state.dataMaterial->Material(MaterNum).GasName = TypeOfGas;
            if (TypeOfGas == "AIR") state.dataMaterial->Material(MaterNum).GasType(1) = 1;
            if (TypeOfGas == "ARGON") state.dataMaterial->Material(MaterNum).GasType(1) = 2;
            if (TypeOfGas == "KRYPTON") state.dataMaterial->Material(MaterNum).GasType(1) = 3;
            if (TypeOfGas == "XENON") state.dataMaterial->Material(MaterNum).GasType(1) = 4;
            if (TypeOfGas == "CUSTOM") state.dataMaterial->Material(MaterNum).GasType(1) = 0;

            if (state.dataMaterial->Material(MaterNum).GasType(1) == -1) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cAlphaFieldNames(2) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, Xenon");
            }

            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;

            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ROnly = true;

            GasType = state.dataMaterial->Material(MaterNum).GasType(1);
            if (GasType >= 1 && GasType <= 4) {
                state.dataMaterial->Material(MaterNum).GasWght(1) = GasWght(GasType);
                state.dataMaterial->Material(MaterNum).GasSpecHeatRatio(1) = GasSpecificHeatRatio(GasType);
                for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                    state.dataMaterial->Material(MaterNum).GasCon(ICoeff, 1) = GasCoeffsCon(ICoeff, GasType);
                    state.dataMaterial->Material(MaterNum).GasVis(ICoeff, 1) = GasCoeffsVis(ICoeff, GasType);
                    state.dataMaterial->Material(MaterNum).GasCp(ICoeff, 1) = GasCoeffsCp(ICoeff, GasType);
                }
            }

            if (!lAlphaFieldBlanks(2)) {
                // Get gap vent type
                if (UtilityRoutines::SameString(MaterialNames(3), "Sealed")) {
                    state.dataMaterial->Material(MaterNum).GapVentType = 1;
                } else if (UtilityRoutines::SameString(MaterialNames(3), "VentedIndoor")) {
                    state.dataMaterial->Material(MaterNum).GapVentType = 2;
                } else if (UtilityRoutines::SameString(MaterialNames(3), "VentedOutdoor")) {
                    state.dataMaterial->Material(MaterNum).GapVentType = 3;
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal gap vent type.");
                    ShowContinueError(state,
                                      "Gap vent type allowed are Sealed, VentedIndoor, or VentedOutdoor." + cAlphaFieldNames(3) +
                                          " entered =" + MaterialNames(3));
                    state.dataMaterial->Material(MaterNum).GapVentType = 1;
                    // ErrorsFound=.TRUE.
                }
            }

            if (GasType == 0) {
                for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                    state.dataMaterial->Material(MaterNum).GasCon(ICoeff, 1) = MaterialProps(1 + ICoeff);
                    state.dataMaterial->Material(MaterNum).GasVis(ICoeff, 1) = MaterialProps(4 + ICoeff);
                    state.dataMaterial->Material(MaterNum).GasCp(ICoeff, 1) = MaterialProps(7 + ICoeff);
                }
                state.dataMaterial->Material(MaterNum).GasWght(1) = MaterialProps(11);
                state.dataMaterial->Material(MaterNum).GasSpecHeatRatio(1) = MaterialProps(12);

                if (state.dataMaterial->Material(MaterNum).GasVis(1, 1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(5) + " not > 0.0");
                }
                if (state.dataMaterial->Material(MaterNum).GasCp(1, 1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(8) + " not > 0.0");
                }
                if (state.dataMaterial->Material(MaterNum).GasWght(1) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(11) + " not > 0.0");
                }
            }

            // Nominal resistance of gap at room temperature
            if (!ErrorsFound) {
                DenomRGas = (state.dataMaterial->Material(MaterNum).GasCon(1, 1) + state.dataMaterial->Material(MaterNum).GasCon(2, 1) * 300.0 +
                             state.dataMaterial->Material(MaterNum).GasCon(3, 1) * 90000.0);
                if (DenomRGas > 0.0) {
                    NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / DenomRGas;
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(
                        state, format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                    ErrorsFound = true;
                }
            }
        }

        // Window gas mixtures (for gaps with two or more gases)

        CurrentModuleObject = "WindowMaterial:GasMixture";
        for (Loop = 1; Loop <= W5GasMatMixture; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, cAlphaArgs(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowGasMixture;
            state.dataMaterial->Material(MaterNum).GasType = -1;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = cAlphaArgs(1);
            NumGases = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = NumGases;
            for (NumGas = 1; NumGas <= NumGases; ++NumGas) {
                TypeOfGas = cAlphaArgs(1 + NumGas);
                if (TypeOfGas == "AIR") state.dataMaterial->Material(MaterNum).GasType(NumGas) = 1;
                if (TypeOfGas == "ARGON") state.dataMaterial->Material(MaterNum).GasType(NumGas) = 2;
                if (TypeOfGas == "KRYPTON") state.dataMaterial->Material(MaterNum).GasType(NumGas) = 3;
                if (TypeOfGas == "XENON") state.dataMaterial->Material(MaterNum).GasType(NumGas) = 4;
                if (state.dataMaterial->Material(MaterNum).GasType(NumGas) == -1) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Illegal value.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(2 + NumGas) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, or Xenon.");
                }
            }

            state.dataMaterial->Material(MaterNum).Roughness = MediumRough; // Unused

            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(1);
            if (state.dataMaterial->Material(MaterNum).Thickness <= 0.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(1) + " must be greater than 0.");
            }
            state.dataMaterial->Material(MaterNum).ROnly = true;

            for (NumGas = 1; NumGas <= NumGases; ++NumGas) {
                GasType = state.dataMaterial->Material(MaterNum).GasType(NumGas);
                if (GasType >= 1 && GasType <= 4) {
                    state.dataMaterial->Material(MaterNum).GasWght(NumGas) = GasWght(GasType);
                    state.dataMaterial->Material(MaterNum).GasSpecHeatRatio(NumGas) = GasSpecificHeatRatio(GasType);
                    state.dataMaterial->Material(MaterNum).GasFract(NumGas) = MaterialProps(2 + NumGas);
                    for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                        state.dataMaterial->Material(MaterNum).GasCon(ICoeff, NumGas) = GasCoeffsCon(ICoeff, GasType);
                        state.dataMaterial->Material(MaterNum).GasVis(ICoeff, NumGas) = GasCoeffsVis(ICoeff, GasType);
                        state.dataMaterial->Material(MaterNum).GasCp(ICoeff, NumGas) = GasCoeffsCp(ICoeff, GasType);
                    }
                }
            }

            // Nominal resistance of gap at room temperature (based on first gas in mixture)
            NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness /
                                 (state.dataMaterial->Material(MaterNum).GasCon(1, 1) + state.dataMaterial->Material(MaterNum).GasCon(2, 1) * 300.0 +
                                  state.dataMaterial->Material(MaterNum).GasCon(3, 1) * 90000.0);
        }

        // Window Shade Materials

        CurrentModuleObject = "WindowMaterial:Shade";
        for (Loop = 1; Loop <= TotShades; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = Shade;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).Trans = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).ReflectShade = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).TransVis = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).ReflectShadeVis = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).AbsorpThermal = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).AbsorpThermalInput = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).TransThermal = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).AbsorpSolar =
                max(0.0, 1.0 - state.dataMaterial->Material(MaterNum).Trans - state.dataMaterial->Material(MaterNum).ReflectShade);
            state.dataMaterial->Material(MaterNum).AbsorpSolarInput = state.dataMaterial->Material(MaterNum).AbsorpSolar;
            state.dataMaterial->Material(MaterNum).WinShadeToGlassDist = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).WinShadeTopOpeningMult = MaterialProps(10);
            state.dataMaterial->Material(MaterNum).WinShadeBottomOpeningMult = MaterialProps(11);
            state.dataMaterial->Material(MaterNum).WinShadeLeftOpeningMult = MaterialProps(12);
            state.dataMaterial->Material(MaterNum).WinShadeRightOpeningMult = MaterialProps(13);
            state.dataMaterial->Material(MaterNum).WinShadeAirFlowPermeability = MaterialProps(14);
            state.dataMaterial->Material(MaterNum).ROnly = true;

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
            } else {
                NominalR(MaterNum) = 1.0;
            }

            if (MaterialProps(1) + MaterialProps(2) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(1) + " + " + cNumericFieldNames(2) + " not < 1.0");
            }

            if (MaterialProps(3) + MaterialProps(4) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(3) + " + " + cNumericFieldNames(4) + " not < 1.0");
            }

            if (MaterialProps(5) + MaterialProps(6) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(5) + " + " + cNumericFieldNames(6) + " not < 1.0");
            }
        }

        // Window Shade Materials

        CurrentModuleObject = "WindowMaterial:Shade:EquivalentLayer";
        for (Loop = 1; Loop <= TotShadesEQL; ++Loop) {

            MaterialProps = 0;

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = ShadeEquivalentLayer;

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            //  Front side and back side have the same beam-Beam Transmittance
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausBackBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiff = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiff = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiff = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeamVis = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiffVis = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiffVis = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).TausThermal = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).EmissThermalFront = MaterialProps(10);
            state.dataMaterial->Material(MaterNum).EmissThermalBack = MaterialProps(11);
            // Assumes thermal emissivity is the same as thermal absorptance
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = state.dataMaterial->Material(MaterNum).EmissThermalFront;
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = state.dataMaterial->Material(MaterNum).EmissThermalBack;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).TausThermal;

            if (MaterialProps(1) + MaterialProps(2) + MaterialProps(4) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(1) + " + " + cNumericFieldNames(2) + " + " + cNumericFieldNames(4) + "not < 1.0");
            }
            if (MaterialProps(1) + MaterialProps(3) + MaterialProps(5) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(1) + " + " + cNumericFieldNames(3) + " + " + cNumericFieldNames(5) + "not < 1.0");
            }
            if (MaterialProps(6) + MaterialProps(7) + MaterialProps(8) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " + " + cNumericFieldNames(7) + " + " + cNumericFieldNames(8) + "not < 1.0");
            }
            if (MaterialProps(9) + MaterialProps(10) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(10) + " not < 1.0");
            }
            if (MaterialProps(9) + MaterialProps(11) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(11) + " not < 1.0");
            }

        } // TotShadesEQL loop

        // Window drape materials

        CurrentModuleObject = "WindowMaterial:Drape:EquivalentLayer";
        for (Loop = 1; Loop <= TotDrapesEQL; ++Loop) {

            MaterialProps = 0;

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = DrapeEquivalentLayer;

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            //  Front side and back side have the same properties
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausBackBeamBeam = MaterialProps(1);

            state.dataMaterial->Material(MaterNum).TausFrontBeamDiff = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiff = MaterialProps(3);

            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiff = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeamVis = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiffVis = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiffVis = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).TausThermal = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).EmissThermalFront = MaterialProps(10);
            state.dataMaterial->Material(MaterNum).EmissThermalBack = MaterialProps(11);
            // Assumes thermal emissivity is the same as thermal absorptance
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = state.dataMaterial->Material(MaterNum).EmissThermalFront;
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = state.dataMaterial->Material(MaterNum).EmissThermalBack;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).TausThermal;

            if (!lNumericFieldBlanks(12) && !lNumericFieldBlanks(13)) {
                if (MaterialProps(12) != 0.0 && MaterialProps(13) != 0.0) {
                    state.dataMaterial->Material(MaterNum).PleatedDrapeWidth = MaterialProps(12);
                    state.dataMaterial->Material(MaterNum).PleatedDrapeLength = MaterialProps(13);
                    state.dataMaterial->Material(MaterNum).ISPleatedDrape = true;
                }
            } else {
                state.dataMaterial->Material(MaterNum).ISPleatedDrape = false;
            }
            if (MaterialProps(1) + MaterialProps(2) + MaterialProps(4) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(1) + " + " + cNumericFieldNames(2) + " + " + cNumericFieldNames(4) + "not < 1.0");
            }
            if (MaterialProps(6) + MaterialProps(7) + MaterialProps(8) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(4) + " + " + cNumericFieldNames(5) + " + " + cNumericFieldNames(6) + "not < 1.0");
            }
            if (MaterialProps(9) + MaterialProps(10) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(10) + " not < 1.0");
            }

        } // TotDrapesEQL loop

        // Window Screen Materials

        CurrentModuleObject = "WindowMaterial:Screen";
        for (Loop = 1; Loop <= TotScreens; ++Loop) {

            // Call GetObjectItem routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = Screen;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).ReflectanceModeling = MaterialNames(2);
            if (!(UtilityRoutines::SameString(MaterialNames(2), "DoNotModel") || UtilityRoutines::SameString(MaterialNames(2), "ModelAsDirectBeam") ||
                  UtilityRoutines::SameString(MaterialNames(2), "ModelAsDiffuse"))) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(
                    state, cAlphaFieldNames(2) + "=\"" + MaterialNames(2) + "\", must be one of DoNotModel, ModelAsDirectBeam or ModelAsDiffuse.");
            }
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).ReflectShade = MaterialProps(1);
            if (state.dataMaterial->Material(MaterNum).ReflectShade < 0.0 || state.dataMaterial->Material(MaterNum).ReflectShade > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(1) + " must be >= 0 and <= 1");
            }
            state.dataMaterial->Material(MaterNum).ReflectShadeVis = MaterialProps(2);
            if (state.dataMaterial->Material(MaterNum).ReflectShadeVis < 0.0 || state.dataMaterial->Material(MaterNum).ReflectShadeVis > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state,
                                  cNumericFieldNames(2) + " must be >= 0 and <= 1 for material " + state.dataMaterial->Material(MaterNum).Name + '.');
            }
            state.dataMaterial->Material(MaterNum).AbsorpThermal = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).AbsorpThermalInput = MaterialProps(3);
            if (state.dataMaterial->Material(MaterNum).AbsorpThermal < 0.0 || state.dataMaterial->Material(MaterNum).AbsorpThermal > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(3) + " must be >= 0 and <= 1");
            }
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(6); // thickness = diameter

            if (MaterialProps(5) > 0.0) {
                //      SurfaceScreens(ScNum)%ScreenDiameterToSpacingRatio = MaterialProps(6)/MaterialProps(5) or
                //      1-SQRT(dataMaterial.Material(MaterNum)%Trans
                if (MaterialProps(6) / MaterialProps(5) >= 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(6) + " must be less than " + cNumericFieldNames(5));
                } else {
                    //       Calculate direct normal transmittance (open area fraction)
                    state.dataMaterial->Material(MaterNum).Trans = pow_2(1.0 - MaterialProps(6) / MaterialProps(5));
                }
            } else {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(5) + " must be > 0.");
                MaterialProps(5) = 0.000000001;
            }

            if (MaterialProps(6) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(6) + " must be > 0.");
            }

            //   Modify reflectance to account for the open area in the screen assembly
            state.dataMaterial->Material(MaterNum).ReflectShade *= (1.0 - state.dataMaterial->Material(MaterNum).Trans);
            state.dataMaterial->Material(MaterNum).ReflectShadeVis *= (1.0 - state.dataMaterial->Material(MaterNum).Trans);

            state.dataMaterial->Material(MaterNum).WinShadeToGlassDist = MaterialProps(7);
            if (state.dataMaterial->Material(MaterNum).WinShadeToGlassDist < 0.001 ||
                state.dataMaterial->Material(MaterNum).WinShadeToGlassDist > 1.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(7) + " must be greater than or equal to 0.001 and less than or equal to 1.");
            }

            state.dataMaterial->Material(MaterNum).WinShadeTopOpeningMult = MaterialProps(8);
            if (state.dataMaterial->Material(MaterNum).WinShadeTopOpeningMult < 0.0 ||
                state.dataMaterial->Material(MaterNum).WinShadeTopOpeningMult > 1.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(8) + " must be greater than or equal to 0 and less than or equal to 1.");
            }

            state.dataMaterial->Material(MaterNum).WinShadeBottomOpeningMult = MaterialProps(9);
            if (state.dataMaterial->Material(MaterNum).WinShadeBottomOpeningMult < 0.0 ||
                state.dataMaterial->Material(MaterNum).WinShadeBottomOpeningMult > 1.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(9) + " must be greater than or equal to 0 and less than or equal to 1.");
            }

            state.dataMaterial->Material(MaterNum).WinShadeLeftOpeningMult = MaterialProps(10);
            if (state.dataMaterial->Material(MaterNum).WinShadeLeftOpeningMult < 0.0 ||
                state.dataMaterial->Material(MaterNum).WinShadeLeftOpeningMult > 1.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(10) + " must be greater than or equal to 0 and less than or equal to 1.");
            }

            state.dataMaterial->Material(MaterNum).WinShadeRightOpeningMult = MaterialProps(11);
            if (state.dataMaterial->Material(MaterNum).WinShadeRightOpeningMult < 0.0 ||
                state.dataMaterial->Material(MaterNum).WinShadeRightOpeningMult > 1.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(11) + " must be greater than or equal to 0 and less than or equal to 1.");
            }

            state.dataMaterial->Material(MaterNum).ScreenMapResolution = MaterialProps(12);
            if (state.dataMaterial->Material(MaterNum).ScreenMapResolution < 0 || state.dataMaterial->Material(MaterNum).ScreenMapResolution > 5 ||
                state.dataMaterial->Material(MaterNum).ScreenMapResolution == 4) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(12) + " must be 0, 1, 2, 3, or 5.");
                ErrorsFound = true;
            }

            //   Default air flow permeability to open area fraction
            state.dataMaterial->Material(MaterNum).WinShadeAirFlowPermeability = state.dataMaterial->Material(MaterNum).Trans;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).Trans;
            state.dataMaterial->Material(MaterNum).TransVis = state.dataMaterial->Material(MaterNum).Trans;

            state.dataMaterial->Material(MaterNum).ROnly = true;

            //   Calculate absorptance accounting for the open area in the screen assembly (used only in CreateShadedWindowConstruction)
            state.dataMaterial->Material(MaterNum).AbsorpSolar =
                max(0.0, 1.0 - state.dataMaterial->Material(MaterNum).Trans - state.dataMaterial->Material(MaterNum).ReflectShade);
            state.dataMaterial->Material(MaterNum).AbsorpSolarInput = state.dataMaterial->Material(MaterNum).AbsorpSolar;
            state.dataMaterial->Material(MaterNum).AbsorpVisible =
                max(0.0, 1.0 - state.dataMaterial->Material(MaterNum).TransVis - state.dataMaterial->Material(MaterNum).ReflectShadeVis);
            state.dataMaterial->Material(MaterNum).AbsorpVisibleInput = state.dataMaterial->Material(MaterNum).AbsorpVisible;
            state.dataMaterial->Material(MaterNum).AbsorpThermal *= (1.0 - state.dataMaterial->Material(MaterNum).Trans);
            state.dataMaterial->Material(MaterNum).AbsorpThermalInput = state.dataMaterial->Material(MaterNum).AbsorpThermal;

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = (1.0 - state.dataMaterial->Material(MaterNum).Trans) * state.dataMaterial->Material(MaterNum).Thickness /
                                     state.dataMaterial->Material(MaterNum).Conductivity;
            } else {
                NominalR(MaterNum) = 1.0;
                ShowWarningError(
                    state,
                    "Conductivity for material=\"" + state.dataMaterial->Material(MaterNum).Name +
                        "\" must be greater than 0 for calculating Nominal R-value, Nominal R is defaulted to 1 and the simulation continues.");
            }

            if (state.dataMaterial->Material(MaterNum).Trans + state.dataMaterial->Material(MaterNum).ReflectShade >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
                ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
            }

            if (state.dataMaterial->Material(MaterNum).TransVis + state.dataMaterial->Material(MaterNum).ReflectShadeVis >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
                ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
            }

            if (state.dataMaterial->Material(MaterNum).TransThermal + state.dataMaterial->Material(MaterNum).AbsorpThermal >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
            }
        }

        CurrentModuleObject = "WindowMaterial:Screen:EquivalentLayer";
        for (Loop = 1; Loop <= TotScreensEQL; ++Loop) {

            MaterialProps = 0;

            // Call GetObjectItem routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = ScreenEquivalentLayer;

            // Load the material derived type from the input data.
            // WindowMaterial:Screen:EquivalentLayer,
            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
            state.dataMaterial->Material(MaterNum).ROnly = true;
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausBackBeamBeam = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiff = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiff = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiff = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).TausFrontBeamBeamVis = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).TausFrontBeamDiffVis = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).ReflFrontDiffDiffVis = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).TausThermal = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).EmissThermalFront = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).EmissThermalBack = MaterialProps(8);

            // Assumes thermal emissivity is the same as thermal absorptance
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = state.dataMaterial->Material(MaterNum).EmissThermalFront;
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = state.dataMaterial->Material(MaterNum).EmissThermalBack;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).TausThermal;

            if (MaterialProps(3) < 0.0 || MaterialProps(3) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, cNumericFieldNames(3) + " must be >= 0 and <= 1");
            }

            if (MaterialProps(6) < 0.0 || MaterialProps(6) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state,
                                  cNumericFieldNames(6) + " must be >= 0 and <= 1 for material " + state.dataMaterial->Material(MaterNum).Name + '.');
            }

            if (!lNumericFieldBlanks(9)) {
                if (MaterialProps(9) > 0.00001) {
                    state.dataMaterial->Material(MaterNum).ScreenWireSpacing = MaterialProps(9); // screen wire spacing
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(9) + " must be > 0.");
                    ShowContinueError(state, "...Setting screen wire spacing to a default value of 0.025m and simulation continues.");
                    state.dataMaterial->Material(MaterNum).ScreenWireSpacing = 0.025;
                }
            }

            if (!lNumericFieldBlanks(10)) {
                if (MaterialProps(10) > 0.00001 && MaterialProps(10) < state.dataMaterial->Material(MaterNum).ScreenWireSpacing) {
                    state.dataMaterial->Material(MaterNum).ScreenWireDiameter = MaterialProps(10); // screen wire spacing
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                    ShowContinueError(state, cNumericFieldNames(10) + " must be > 0.");
                    ShowContinueError(state, "...Setting screen wire diameter to a default value of 0.005m and simulation continues.");
                    state.dataMaterial->Material(MaterNum).ScreenWireDiameter = 0.005;
                }
            }

            if (state.dataMaterial->Material(MaterNum).ScreenWireSpacing > 0.0) {
                if (state.dataMaterial->Material(MaterNum).ScreenWireDiameter / state.dataMaterial->Material(MaterNum).ScreenWireSpacing >= 1.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state, cNumericFieldNames(10) + " must be less than " + cNumericFieldNames(9));
                } else {
                    //  Calculate direct normal transmittance (open area fraction)
                    Openness = pow_2(1.0 - state.dataMaterial->Material(MaterNum).ScreenWireDiameter /
                                               state.dataMaterial->Material(MaterNum).ScreenWireSpacing);
                    if ((state.dataMaterial->Material(MaterNum).TausFrontBeamBeam - Openness) / Openness > 0.01) {
                        ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", screen openness specified.");
                        ShowContinueError(state, cNumericFieldNames(1) + " is > 1.0% of the value calculated from input fields:");
                        ShowContinueError(state, cNumericFieldNames(9) + " and " + (cNumericFieldNames(10)));
                        ShowContinueError(state, " using the formula (1-diameter/spacing)**2");
                        ShowContinueError(state, " ...the screen diameter is recalculated from the material openness specified ");
                        ShowContinueError(state, " ...and wire spacing using the formula = wire spacing * (1.0 - SQRT(Opennes))");
                        state.dataMaterial->Material(MaterNum).ScreenWireDiameter =
                            state.dataMaterial->Material(MaterNum).ScreenWireSpacing *
                            (1.0 - std::sqrt(state.dataMaterial->Material(MaterNum).TausFrontBeamBeam));
                        ShowContinueError(state,
                                          format(" ...Recalculated {}={:.4R} m",
                                                 cNumericFieldNames(10),
                                                 state.dataMaterial->Material(MaterNum).ScreenWireDiameter));
                    }
                }
            }

            if (state.dataMaterial->Material(MaterNum).TausFrontBeamBeam + state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
                ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
            }

            if (state.dataMaterial->Material(MaterNum).TausFrontBeamBeamVis + state.dataMaterial->Material(MaterNum).ReflFrontDiffDiffVis >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
                ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
            }
            if (state.dataMaterial->Material(MaterNum).TransThermal + state.dataMaterial->Material(MaterNum).AbsorpThermal >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
            }

        } // TotScreensEQL loop

        // Window Blind Materials

        if (TotBlinds > 0) {
            Blind.allocate(TotBlinds); // Allocate the array Size to the number of blinds
        }

        CurrentModuleObject = "WindowMaterial:Blind";
        for (Loop = 1; Loop <= TotBlinds; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowBlind;

            // Load the material derived type from the input data.

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            Blind(Loop).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = Rough;
            state.dataMaterial->Material(MaterNum).BlindDataPtr = Loop;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            Blind(Loop).MaterialNumber = MaterNum;
            if (UtilityRoutines::SameString(MaterialNames(2), "Horizontal")) {
                Blind(Loop).SlatOrientation = Horizontal;
            } else if (UtilityRoutines::SameString(MaterialNames(2), "Vertical")) {
                Blind(Loop).SlatOrientation = Vertical;
            }
            Blind(Loop).SlatWidth = MaterialProps(1);
            Blind(Loop).SlatSeparation = MaterialProps(2);
            Blind(Loop).SlatThickness = MaterialProps(3);
            Blind(Loop).SlatAngle = MaterialProps(4);
            Blind(Loop).SlatConductivity = MaterialProps(5);
            Blind(Loop).SlatTransSolBeamDiff = MaterialProps(6);
            Blind(Loop).SlatFrontReflSolBeamDiff = MaterialProps(7);
            Blind(Loop).SlatBackReflSolBeamDiff = MaterialProps(8);
            Blind(Loop).SlatTransSolDiffDiff = MaterialProps(9);
            Blind(Loop).SlatFrontReflSolDiffDiff = MaterialProps(10);
            Blind(Loop).SlatBackReflSolDiffDiff = MaterialProps(11);
            Blind(Loop).SlatTransVisBeamDiff = MaterialProps(12);
            Blind(Loop).SlatFrontReflVisBeamDiff = MaterialProps(13);
            Blind(Loop).SlatBackReflVisBeamDiff = MaterialProps(14);
            Blind(Loop).SlatTransVisDiffDiff = MaterialProps(15);
            Blind(Loop).SlatFrontReflVisDiffDiff = MaterialProps(16);
            Blind(Loop).SlatBackReflVisDiffDiff = MaterialProps(17);
            Blind(Loop).SlatTransIR = MaterialProps(18);
            Blind(Loop).SlatFrontEmissIR = MaterialProps(19);
            Blind(Loop).SlatBackEmissIR = MaterialProps(20);
            Blind(Loop).BlindToGlassDist = MaterialProps(21);
            Blind(Loop).BlindTopOpeningMult = MaterialProps(22);
            Blind(Loop).BlindBottomOpeningMult = MaterialProps(23);
            Blind(Loop).BlindLeftOpeningMult = MaterialProps(24);
            Blind(Loop).BlindRightOpeningMult = MaterialProps(25);
            Blind(Loop).MinSlatAngle = MaterialProps(26);
            Blind(Loop).MaxSlatAngle = MaterialProps(27);

            // TH 2/11/2010. For CR 8010
            // By default all blinds have fixed slat angle, new blinds with variable slat angle are created if
            //  they are used with window shading controls that adjust slat angles like ScheduledSlatAngle or BlockBeamSolar
            Blind(Loop).SlatAngleType = FixedSlats;

            if (Blind(Loop).SlatWidth < Blind(Loop).SlatSeparation) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Angles/Widths");
                ShowContinueError(state,
                                  format("{} [{:.2R}] is less than {} [{:.2R}].",
                                         cNumericFieldNames(1),
                                         Blind(Loop).SlatWidth,
                                         cNumericFieldNames(2),
                                         Blind(Loop).SlatSeparation));
                ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
            }

            if (!UtilityRoutines::SameString(MaterialNames(2), "Horizontal") && !UtilityRoutines::SameString(MaterialNames(2), "Vertical")) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value");
                ShowContinueError(state, cAlphaFieldNames(2) + "=\"" + MaterialNames(2) + "\", must be Horizontal or Vertical.");
            }

            if ((MaterialProps(6) + MaterialProps(7) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " + " + cNumericFieldNames(7) + " not < 1.0");
            }
            if ((MaterialProps(6) + MaterialProps(8) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " + " + cNumericFieldNames(8) + " not < 1.0");
            }

            if ((MaterialProps(9) + MaterialProps(10) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(10) + " not < 1.0");
            }
            if ((MaterialProps(9) + MaterialProps(11) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(11) + " not < 1.0");
            }

            if ((MaterialProps(12) + MaterialProps(13) >= 1.0) || (MaterialProps(12) + MaterialProps(14) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(12) + " + " + cNumericFieldNames(13) + " not < 1.0 OR");
                ShowContinueError(state, cNumericFieldNames(12) + " + " + cNumericFieldNames(14) + " not < 1.0");
            }

            if ((MaterialProps(12) + MaterialProps(13) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(12) + " + " + cNumericFieldNames(13) + " not < 1.0");
            }
            if ((MaterialProps(12) + MaterialProps(14) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(12) + " + " + cNumericFieldNames(14) + " not < 1.0");
            }

            if ((MaterialProps(15) + MaterialProps(16) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(15) + " + " + cNumericFieldNames(16) + " not < 1.0");
            }
            if ((MaterialProps(15) + MaterialProps(17) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(15) + " + " + cNumericFieldNames(17) + " not < 1.0");
            }

            // Require that beam and diffuse properties be the same
            if (std::abs(MaterialProps(9) - MaterialProps(6)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " must equal " + cNumericFieldNames(9));
            }

            if (std::abs(MaterialProps(10) - MaterialProps(7)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(7) + " must equal " + cNumericFieldNames(10));
            }

            if (std::abs(MaterialProps(11) - MaterialProps(8)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(8) + " must equal " + cNumericFieldNames(11));
            }

            if (std::abs(MaterialProps(15) - MaterialProps(12)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(12) + " must equal " + cNumericFieldNames(15));
            }

            if (std::abs(MaterialProps(16) - MaterialProps(13)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(13) + " must equal " + cNumericFieldNames(16));
            }

            if (std::abs(MaterialProps(17) - MaterialProps(14)) > 1.e-5) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(14) + " must equal " + cNumericFieldNames(17));
            }

            if ((MaterialProps(18) + MaterialProps(19) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(18) + " + " + cNumericFieldNames(19) + " not < 1.0");
            }
            if ((MaterialProps(18) + MaterialProps(20) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(18) + " + " + cNumericFieldNames(20) + " not < 1.0");
            }

            if (Blind(Loop).BlindToGlassDist < 0.5 * Blind(Loop).SlatWidth) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(21) + " is less than half of the " + cNumericFieldNames(1));
            }

            // Minimum and maximum slat angles allowed by slat geometry
            if (Blind(Loop).SlatWidth > Blind(Loop).SlatSeparation) {
                MinSlatAngGeom = std::asin(Blind(Loop).SlatThickness / (Blind(Loop).SlatThickness + Blind(Loop).SlatSeparation)) /
                                 DataGlobalConstants::DegToRadians;
            } else {
                MinSlatAngGeom = 0.0;
            }
            MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

            // Error if input slat angle not in range allowed by slat geometry
            if ((Blind(Loop).SlatSeparation + Blind(Loop).SlatThickness) < Blind(Loop).SlatWidth) {
                if (Blind(Loop).SlatAngle < MinSlatAngGeom) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state,
                                      format("{}=[{:.1R}], is less than smallest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                             cNumericFieldNames(4),
                                             Blind(Loop).SlatAngle,
                                             MinSlatAngGeom));
                } else if (Blind(Loop).SlatAngle > MaxSlatAngGeom) {
                    ErrorsFound = true;
                    ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                    ShowContinueError(state,
                                      format("{}=[{:.1R}], is greater than largest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                             cNumericFieldNames(4),
                                             Blind(Loop).SlatAngle,
                                             MinSlatAngGeom));
                }
            }

            // By default all Blinds are "fixed" slats.  Only with Shading Control is one considered variable and this check
            // is now done when that happens.  9.3.2009 LKL

            //    IF(Blind(Loop)%SlatAngleType == VariableSlats) THEN
            //      ! Error if maximum slat angle less than minimum
            //      IF(Blind(Loop)%MaxSlatAngle < Blind(Loop)%MinSlatAngle) THEN
            //        ErrorsFound = .TRUE.
            //        CALL ShowSevereError(state, TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
            //        CALL ShowContinueError(state, TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
            //           '], is greater than '//TRIM(cNumericFieldNames(27))//'=['//  &
            //           TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
            //      END IF
            //      ! Error if input slat angle not in input min/max range
            //      IF(Blind(Loop)%MaxSlatAngle > Blind(Loop)%MinSlatAngle .AND. (Blind(Loop)%SlatAngle < Blind(Loop)%MinSlatAngle &
            //          .OR. Blind(Loop)%SlatAngle > Blind(Loop)%MaxSlatAngle)) THEN
            //        ErrorsFound = .TRUE.
            //        CALL ShowSevereError(state, TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
            //        CALL ShowContinueError(state, TRIM(cNumericFieldNames(4))//'=['//TRIM(RoundSigDigits(Blind(Loop)%SlatAngle,1))//  &
            //           '] is outside of the input min/max range, min=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
            //           '], max=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
            //      END IF
            //      ! Error if input minimum slat angle is less than that allowed by slat geometry
            //      IF(Blind(Loop)%MinSlatAngle < MinSlatAngGeom) THEN
            //        CALL ShowSevereError(state, TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
            //        CALL ShowContinueError(state, TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
            //           '] is less than the smallest allowed by slat dimensions and spacing, min=['//  &
            //           TRIM(RoundSigDigits(MinSlatAngGeom,1))//'] deg.')
            //        CALL ShowContinueError(state, 'Minimum Slat Angle will be set to '//TRIM(RoundSigDigits(MinSlatAngGeom,1))//' deg.')
            //        Blind(Loop)%MinSlatAngle = MinSlatAngGeom
            //      END IF
            //      ! Error if input maximum slat angle is greater than that allowed by slat geometry
            //      IF(Blind(Loop)%MaxSlatAngle > MaxSlatAngGeom) THEN
            //        CALL ShowWarningError(state, TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
            //        CALL ShowContinueError(state, TRIM(cNumericFieldNames(27))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//  &
            //           '] is greater than the largest allowed by slat dimensions and spacing, ['//  &
            //           TRIM(RoundSigDigits(MaxSlatAngGeom,1))//'] deg.')
            //        CALL ShowContinueError(state, 'Maximum Slat Angle will be set to '//TRIM(RoundSigDigits(MaxSlatAngGeom,1))//' deg.')
            //        Blind(Loop)%MaxSlatAngle = MaxSlatAngGeom
            //      END IF
            //    END IF  ! End of check if slat angle is variable
        }

        // Window Blind Materials for EquivalentLayer Model

        CurrentModuleObject = "WindowMaterial:Blind:EquivalentLayer";
        for (Loop = 1; Loop <= TotBlindsEQL; ++Loop) {

            // Call Input Get routine to retrieve material data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = BlindEquivalentLayer;

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            state.dataMaterial->Material(MaterNum).Roughness = Rough;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            if (UtilityRoutines::SameString(MaterialNames(2), "Horizontal")) {
                state.dataMaterial->Material(MaterNum).SlatOrientation = Horizontal;
            } else if (UtilityRoutines::SameString(MaterialNames(2), "Vertical")) {
                state.dataMaterial->Material(MaterNum).SlatOrientation = Vertical;
            }
            state.dataMaterial->Material(MaterNum).SlatWidth = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).SlatSeparation = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).SlatCrown = MaterialProps(3);
            state.dataMaterial->Material(MaterNum).SlatAngle = MaterialProps(4);

            state.dataMaterial->Material(MaterNum).TausFrontBeamDiff = MaterialProps(5);
            state.dataMaterial->Material(MaterNum).TausBackBeamDiff = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).ReflFrontBeamDiff = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).ReflBackBeamDiff = MaterialProps(8);

            if (!lNumericFieldBlanks(9) && !lNumericFieldBlanks(10) && !lNumericFieldBlanks(11) && !lNumericFieldBlanks(12)) {
                state.dataMaterial->Material(MaterNum).TausFrontBeamDiffVis = MaterialProps(9);
                state.dataMaterial->Material(MaterNum).TausBackBeamDiffVis = MaterialProps(10);
                state.dataMaterial->Material(MaterNum).ReflFrontBeamDiffVis = MaterialProps(11);
                state.dataMaterial->Material(MaterNum).ReflBackBeamDiffVis = MaterialProps(12);
            }
            if (!lNumericFieldBlanks(13) && !lNumericFieldBlanks(14) && !lNumericFieldBlanks(15)) {
                state.dataMaterial->Material(MaterNum).TausDiffDiff = MaterialProps(13);
                state.dataMaterial->Material(MaterNum).ReflFrontDiffDiff = MaterialProps(14);
                state.dataMaterial->Material(MaterNum).ReflBackDiffDiff = MaterialProps(15);
            }
            if (!lNumericFieldBlanks(16) && !lNumericFieldBlanks(17) && !lNumericFieldBlanks(18)) {
                state.dataMaterial->Material(MaterNum).TausDiffDiffVis = MaterialProps(13);
                state.dataMaterial->Material(MaterNum).ReflFrontDiffDiffVis = MaterialProps(14);
                state.dataMaterial->Material(MaterNum).ReflBackDiffDiffVis = MaterialProps(15);
            }
            if (!lNumericFieldBlanks(19)) {
                state.dataMaterial->Material(MaterNum).TausThermal = MaterialProps(19);
            }
            if (!lNumericFieldBlanks(20)) {
                state.dataMaterial->Material(MaterNum).EmissThermalFront = MaterialProps(20);
            }
            if (!lNumericFieldBlanks(21)) {
                state.dataMaterial->Material(MaterNum).EmissThermalBack = MaterialProps(21);
            }
            // Assumes thermal emissivity is the same as thermal absorptance
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = state.dataMaterial->Material(MaterNum).EmissThermalFront;
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = state.dataMaterial->Material(MaterNum).EmissThermalBack;
            state.dataMaterial->Material(MaterNum).TransThermal = state.dataMaterial->Material(MaterNum).TausThermal;

            // By default all blinds have fixed slat angle,
            //  they are used with window shading controls that adjust slat angles like MaximizeSolar or BlockBeamSolar
            if (!lAlphaFieldBlanks(3)) {
                if (UtilityRoutines::SameString(MaterialNames(3), "FixedSlatAngle")) {
                    state.dataMaterial->Material(MaterNum).SlatAngleType = state.dataWindowEquivalentLayer->lscNONE;
                } else if (UtilityRoutines::SameString(MaterialNames(3), "MaximizeSolar")) {
                    state.dataMaterial->Material(MaterNum).SlatAngleType = state.dataWindowEquivalentLayer->lscVBPROF;
                } else if (UtilityRoutines::SameString(MaterialNames(3), "BlockBeamSolar")) {
                    state.dataMaterial->Material(MaterNum).SlatAngleType = state.dataWindowEquivalentLayer->lscVBNOBM;
                } else {
                    state.dataMaterial->Material(MaterNum).SlatAngleType = 0;
                }
            } else {
                state.dataMaterial->Material(MaterNum).SlatAngleType = 0;
            }
            if (state.dataMaterial->Material(MaterNum).SlatWidth < state.dataMaterial->Material(MaterNum).SlatSeparation) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Seperation/Width");
                ShowContinueError(state,
                                  format("{} [{:.2R}] is less than {} [{:.2R}].",
                                         cNumericFieldNames(1),
                                         state.dataMaterial->Material(MaterNum).SlatWidth,
                                         cNumericFieldNames(2),
                                         state.dataMaterial->Material(MaterNum).SlatSeparation));
                ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
            }
            if (state.dataMaterial->Material(MaterNum).SlatSeparation < 0.001) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Seperation");
                ShowContinueError(
                    state,
                    format("{} [{:.2R}]. Slate spacing must be > 0.0", cNumericFieldNames(2), state.dataMaterial->Material(MaterNum).SlatSeparation));
                ShowContinueError(state, "...Setting slate spacing to default value of 0.025 m and simulation continues.");
                state.dataMaterial->Material(MaterNum).SlatSeparation = 0.025;
            }
            if (state.dataMaterial->Material(MaterNum).SlatWidth < 0.001 ||
                state.dataMaterial->Material(MaterNum).SlatWidth >= 2.0 * state.dataMaterial->Material(MaterNum).SlatSeparation) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Width");
                ShowContinueError(state,
                                  format("{} [{:.2R}]. Slat width range is 0 < Width <= 2*Spacing",
                                         cNumericFieldNames(1),
                                         state.dataMaterial->Material(MaterNum).SlatWidth));
                ShowContinueError(state, "...Setting slate width equal to slate spacing and simulation continues.");
                state.dataMaterial->Material(MaterNum).SlatWidth = state.dataMaterial->Material(MaterNum).SlatSeparation;
            }
            if (state.dataMaterial->Material(MaterNum).SlatCrown < 0.0 ||
                state.dataMaterial->Material(MaterNum).SlatCrown >= 0.5 * state.dataMaterial->Material(MaterNum).SlatWidth) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Crown");
                ShowContinueError(state,
                                  format("{} [{:.2R}]. Slat crwon range is 0 <= crown < 0.5*Width",
                                         cNumericFieldNames(3),
                                         state.dataMaterial->Material(MaterNum).SlatCrown));
                ShowContinueError(state, "...Setting slate crown to 0.0 and simulation continues.");
                state.dataMaterial->Material(MaterNum).SlatCrown = 0.0;
            }
            if (state.dataMaterial->Material(MaterNum).SlatAngle < -90.0 || state.dataMaterial->Material(MaterNum).SlatAngle > 90.0) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Angle");
                ShowContinueError(state,
                                  format("{} [{:.2R}]. Slat angle range is -90.0 <= Angle < 90.0",
                                         cNumericFieldNames(4),
                                         state.dataMaterial->Material(MaterNum).SlatAngle));
                ShowContinueError(state, "...Setting slate angle to 0.0 and simulation continues.");
                state.dataMaterial->Material(MaterNum).SlatAngle = 0.0;
            }

            if (!UtilityRoutines::SameString(MaterialNames(2), "Horizontal") && !UtilityRoutines::SameString(MaterialNames(2), "Vertical")) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value");
                ShowContinueError(state, cAlphaFieldNames(2) + "=\"" + MaterialNames(2) + "\", must be Horizontal or Vertical.");
            }

            if ((MaterialProps(5) + MaterialProps(7) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(5) + " + " + cNumericFieldNames(7) + " not < 1.0");
            }
            if ((MaterialProps(6) + MaterialProps(8) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(6) + " + " + cNumericFieldNames(8) + " not < 1.0");
            }
            if ((MaterialProps(9) + MaterialProps(11) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(9) + " + " + cNumericFieldNames(11) + " not < 1.0");
            }
            if ((MaterialProps(10) + MaterialProps(12) >= 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(10) + " + " + cNumericFieldNames(12) + " not < 1.0");
            }

        } // TotBlindsEQL loop

        // EcoRoof Materials
        // PSU 2006
        CurrentModuleObject = "Material:RoofVegetation";
        for (Loop = 1; Loop <= EcoRoofMat; ++Loop) {
            // Call Input Get Routine to retrieve material data from ecoroof

            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          MaterialNames,
                                          MaterialNumAlpha,
                                          MaterialProps,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, MaterialNames(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            // this part is similar to the regular material
            // Load the material derived type from the input data.
            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = EcoRoof;

            // this part is new for Ecoroof properties,
            // especially for the Plant Layer of the ecoroof
            state.dataMaterial->Material(MaterNum).HeightOfPlants = MaterialProps(1);
            state.dataMaterial->Material(MaterNum).LAI = MaterialProps(2);
            state.dataMaterial->Material(MaterNum).Lreflectivity = MaterialProps(3); // Albedo
            state.dataMaterial->Material(MaterNum).LEmissitivity = MaterialProps(4);
            state.dataMaterial->Material(MaterNum).RStomata = MaterialProps(5);

            state.dataMaterial->Material(MaterNum).Name = MaterialNames(1);
            // need to treat the A2 with is just the name of the soil(it is
            // not important)
            ValidateMaterialRoughness(state, MaterNum, MaterialNames(3), ErrorsFound);
            if (UtilityRoutines::SameString(MaterialNames(4), "Simple")) {
                state.dataMaterial->Material(MaterNum).EcoRoofCalculationMethod = 1;
            } else if (UtilityRoutines::SameString(MaterialNames(4), "Advanced") || lAlphaFieldBlanks(4)) {
                state.dataMaterial->Material(MaterNum).EcoRoofCalculationMethod = 2;
            } else {
                ShowSevereError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value");
                ShowContinueError(state, cAlphaFieldNames(4) + "=\"" + MaterialNames(4) + "\".");
                ShowContinueError(state, "...Valid values are \"Simple\" or \"Advanced\".");
                ErrorsFound = true;
            }

            state.dataMaterial->Material(MaterNum).Thickness = MaterialProps(6);
            state.dataMaterial->Material(MaterNum).Conductivity = MaterialProps(7);
            state.dataMaterial->Material(MaterNum).Density = MaterialProps(8);
            state.dataMaterial->Material(MaterNum).SpecHeat = MaterialProps(9);
            state.dataMaterial->Material(MaterNum).AbsorpThermal = MaterialProps(10); // emissivity
            state.dataMaterial->Material(MaterNum).AbsorpSolar = MaterialProps(11);   // (1 - Albedo)
            state.dataMaterial->Material(MaterNum).AbsorpVisible = MaterialProps(12);
            state.dataMaterial->Material(MaterNum).Porosity = MaterialProps(13);
            state.dataMaterial->Material(MaterNum).MinMoisture = MaterialProps(14);
            state.dataMaterial->Material(MaterNum).InitMoisture = MaterialProps(15);

            if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                NominalR(MaterNum) = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                state.dataMaterial->Material(MaterNum).Resistance = NominalR(MaterNum);
            } else {
                ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                ShowContinueError(state, cNumericFieldNames(7) + " is <=0.");
                ErrorsFound = true;
            }

            if (state.dataMaterial->Material(MaterNum).InitMoisture > state.dataMaterial->Material(MaterNum).Porosity) {
                ShowWarningError(state, CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state, cNumericFieldNames(15) + " is greater than " + cNumericFieldNames(13) + ". It must be less or equal.");
                ShowContinueError(state, format("{} = {:.3T}.", cNumericFieldNames(13), state.dataMaterial->Material(MaterNum).Porosity));
                ShowContinueError(state, format("{} = {:.3T}.", cNumericFieldNames(15), state.dataMaterial->Material(MaterNum).InitMoisture));
                ShowContinueError(state,
                                  format("{} is reset to the maximum (saturation) value = {:.3T}.",
                                         cNumericFieldNames(15),
                                         state.dataMaterial->Material(MaterNum).Porosity));
                ShowContinueError(state, "Simulation continues.");
                state.dataMaterial->Material(MaterNum).InitMoisture = state.dataMaterial->Material(MaterNum).Porosity;
            }
        }

        // Thermochromic glazing group
        // get the number of WindowMaterial:GlazingGroup:Thermochromic objects in the idf file
        CurrentModuleObject = "WindowMaterial:GlazingGroup:Thermochromic";
        TotTCGlazings = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (TotTCGlazings >= 1) {
            // Read TC glazings
            TCGlazings.allocate(TotTCGlazings);

            for (Loop = 1; Loop <= TotTCGlazings; ++Loop) {
                // Get each TCGlazings from the input processor
                inputProcessor->getObjectItem(state,
                                              CurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              MaterialNumAlpha,
                                              rNumericArgs,
                                              MaterialNumProp,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), CurrentModuleObject, ErrorsFound)) {
                    ShowContinueError(state, "...All Thermochromic Glazing names must be unique regardless of subtype.");
                    continue;
                }

                if (MaterialNumProp + 1 != MaterialNumAlpha) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                    ShowContinueError(state, "Check number of " + cAlphaFieldNames(2) + " compared to number of " + cNumericFieldNames(1));
                    ErrorsFound = true;
                    continue;
                }

                // Allocate arrays
                TCGlazings(Loop).SpecTemp.allocate(MaterialNumProp);
                TCGlazings(Loop).LayerName.allocate(MaterialNumProp);
                TCGlazings(Loop).LayerPoint.allocate(MaterialNumProp);
                TCGlazings(Loop).SpecTemp = 0.0;
                TCGlazings(Loop).LayerName = "";
                TCGlazings(Loop).LayerPoint = 0;

                TCGlazings(Loop).Name = cAlphaArgs(1);
                TCGlazings(Loop).NumGlzMat = MaterialNumProp;

                for (iTC = 1; iTC <= MaterialNumProp; ++iTC) {
                    TCGlazings(Loop).SpecTemp(iTC) = rNumericArgs(iTC);
                    TCGlazings(Loop).LayerName(iTC) = cAlphaArgs(1 + iTC);

                    // Find this glazing material in the material list
                    iMat = UtilityRoutines::FindItemInList(cAlphaArgs(1 + iTC), state.dataMaterial->Material);
                    if (iMat != 0) {
                        // TC glazing
                        state.dataMaterial->Material(iMat).SpecTemp = rNumericArgs(iTC);
                        state.dataMaterial->Material(iMat).TCParent = Loop;
                        TCGlazings(Loop).LayerPoint(iTC) = iMat;

                        // test that named material is of the right type
                        if (state.dataMaterial->Material(iMat).Group != WindowGlass) {
                            ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                            ShowContinueError(state, "Material named: " + cAlphaArgs(1 + iTC) + " is not a window glazing ");
                            ErrorsFound = true;
                        }

                    } else { // thow error because not found
                        ShowSevereError(state, CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                        ShowContinueError(state, "Material named: " + cAlphaArgs(1 + iTC) + " was not found ");
                        ErrorsFound = true;
                    }
                }
            }
        }

        cCurrentModuleObject = "WindowMaterial:SimpleGlazingSystem";
        for (Loop = 1; Loop <= TotSimpleWindow; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          MaterialNumAlpha,
                                          rNumericArgs,
                                          MaterialNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, cAlphaArgs(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }
            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = WindowSimpleGlazing;
            state.dataMaterial->Material(MaterNum).Name = cAlphaArgs(1);
            state.dataMaterial->Material(MaterNum).SimpleWindowUfactor = rNumericArgs(1);
            state.dataMaterial->Material(MaterNum).SimpleWindowSHGC = rNumericArgs(2);
            if (!lNumericFieldBlanks(3)) {
                state.dataMaterial->Material(MaterNum).SimpleWindowVisTran = rNumericArgs(3);
                state.dataMaterial->Material(MaterNum).SimpleWindowVTinputByUser = true;
            }

            SetupSimpleWindowGlazingSystem(state, MaterNum);
        }

        // Simon: Place to load materials for complex fenestrations
        if ((TotComplexShades > 0) || (TotComplexGaps > 0)) {
            SetupComplexFenestrationMaterialInput(state, MaterNum, ErrorsFound);
            if (ErrorsFound) {
                ShowSevereError(state, "Errors found in processing complex fenestration material input");
            }
        }
        ScanForReports(state, "Constructions", DoReport, "Materials");

        if (DoReport) {

            print(state.files.eio,
                  "! <Material Details>,Material Name,ThermalResistance {{m2-K/w}},Roughness,Thickness {{m}},Conductivity "
                  "{{w/m-K}},Density {{kg/m3}},Specific Heat "
                  "{{J/kg-K}},Absorptance:Thermal,Absorptance:Solar,Absorptance:Visible\n");

            print(state.files.eio, "! <Material:Air>,Material Name,ThermalResistance {{m2-K/w}}\n");

            // Formats
            static constexpr auto Format_701(" Material Details,{},{:.4R},{},{:.4R},{:.3R},{:.3R},{:.3R},{:.4R},{:.4R},{:.4R}\n");
            static constexpr auto Format_702(" Material:Air,{},{:.4R}\n");

            for (MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum) {

                {
                    auto const SELECT_CASE_var(state.dataMaterial->Material(MaterNum).Group);
                    if (SELECT_CASE_var == Air) {
                        print(state.files.eio,
                              Format_702,
                              state.dataMaterial->Material(MaterNum).Name,
                              state.dataMaterial->Material(MaterNum).Resistance);
                    } else {
                        print(state.files.eio,
                              Format_701,
                              state.dataMaterial->Material(MaterNum).Name,
                              state.dataMaterial->Material(MaterNum).Resistance,
                              DisplayMaterialRoughness(state.dataMaterial->Material(MaterNum).Roughness),
                              state.dataMaterial->Material(MaterNum).Thickness,
                              state.dataMaterial->Material(MaterNum).Conductivity,
                              state.dataMaterial->Material(MaterNum).Density,
                              state.dataMaterial->Material(MaterNum).SpecHeat,
                              state.dataMaterial->Material(MaterNum).AbsorpThermal,
                              state.dataMaterial->Material(MaterNum).AbsorpSolar,
                              state.dataMaterial->Material(MaterNum).AbsorpVisible);
                    }
                }
            }
        }

        //  FORMATS.

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // setup surface property EMS actuators

            for (MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum) {
                if (state.dataMaterial->Material(MaterNum).Group != RegularMaterial) continue;
                SetupEMSActuator(state,
                                 "Material",
                                 state.dataMaterial->Material(MaterNum).Name,
                                 "Surface Property Solar Absorptance",
                                 "[ ]",
                                 state.dataMaterial->Material(MaterNum).AbsorpSolarEMSOverrideOn,
                                 state.dataMaterial->Material(MaterNum).AbsorpSolarEMSOverride);
                SetupEMSActuator(state,
                                 "Material",
                                 state.dataMaterial->Material(MaterNum).Name,
                                 "Surface Property Thermal Absorptance",
                                 "[ ]",
                                 state.dataMaterial->Material(MaterNum).AbsorpThermalEMSOverrideOn,
                                 state.dataMaterial->Material(MaterNum).AbsorpThermalEMSOverride);
                SetupEMSActuator(state,
                                 "Material",
                                 state.dataMaterial->Material(MaterNum).Name,
                                 "Surface Property Visible Absorptance",
                                 "[ ]",
                                 state.dataMaterial->Material(MaterNum).AbsorpVisibleEMSOverrideOn,
                                 state.dataMaterial->Material(MaterNum).AbsorpVisibleEMSOverride);
            }
        }

        // try assigning phase change material properties for each material, won't do anything for non pcm surfaces
        for (auto &m : state.dataMaterial->Material) {
            m.phaseChange = HysteresisPhaseChange::HysteresisPhaseChange::factory(state, m.Name);
        }
    }

    void GetWindowGlassSpectralData(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets spectral data (transmittance, front reflectance, and back
        // reflectance at normal incidence vs. wavelength) for glass

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetWindowGlassSpectralData: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;                      // IO Status when calling get input subroutine
        Array1D_string SpecDataNames(1); // Spectral data alpha names
        int SpecDataNumAlpha;            // Number of spectral data alpha names being passed
        int SpecDataNumProp;             // Number of spectral data properties being passed
        Array1D<Real64> SpecDataProps;   // Temporary array to transfer spectal data properties
        int Loop;
        int LamNum; // Wavelength number
        int TotLam; // Total wavelengths
        Real64 Lam; // Wavelength (microns)
        Real64 Tau; // Transmittance, front reflectance, back reflectance
        Real64 RhoF;
        Real64 RhoB;

        CurrentModuleObject = "MaterialProperty:GlazingSpectralData";
        TotSpectralData = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        SpectralData.allocate(TotSpectralData);
        if (TotSpectralData > 0) SpecDataProps.allocate(Construction::MaxSpectralDataElements * 4);

        for (Loop = 1; Loop <= TotSpectralData; ++Loop) {

            // Call Input Get routine to retrieve spectral data
            // Name is followed by up to 450 sets of normal-incidence measured values of
            // [wavelength (microns), transmittance, front reflectance, back reflectance] for
            // wavelengths covering the short-wave solar spectrum (from about 0.25 to 2.5 microns)
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          SpecDataNames,
                                          SpecDataNumAlpha,
                                          SpecDataProps,
                                          SpecDataNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (UtilityRoutines::IsNameEmpty(state, SpecDataNames(1), CurrentModuleObject, ErrorsFound)) continue;

            // Load the spectral data derived type from the input data.
            SpectralData(Loop).Name = SpecDataNames(1);
            TotLam = SpecDataNumProp / 4;
            if (mod(SpecDataNumProp, 4) != 0) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid set.");
                ShowContinueError(
                    state,
                    format("... set not even multiple of 4 items (Wavelength,Trans,ReflFront,ReflBack), number of items in dataset = {}",
                           SpecDataNumProp));
                ShowContinueError(state, format("... remainder after div by 4 = {}, remainder items will be set to 0.0", mod(SpecDataNumProp, 4)));
                SpecDataProps({SpecDataNumProp + 1, min(SpecDataNumProp + 4, Construction::MaxSpectralDataElements * 4)}) = 0.0;
            }
            if (TotLam > Construction::MaxSpectralDataElements) {
                ErrorsFound = true;
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid set.");
                ShowContinueError(
                    state,
                    format("... More than max [{}] (Wavelength,Trans,ReflFront,ReflBack) entries in set.", Construction::MaxSpectralDataElements));
                continue;
            }
            SpectralData(Loop).NumOfWavelengths = TotLam;

            SpectralData(Loop).WaveLength.allocate(TotLam); // Wavelength (microns)
            SpectralData(Loop).Trans.allocate(TotLam);      // Transmittance at normal incidence
            SpectralData(Loop).ReflFront.allocate(TotLam);  // Front reflectance at normal incidence
            SpectralData(Loop).ReflBack.allocate(TotLam);   // Back reflectance at normal incidence

            for (LamNum = 1; LamNum <= TotLam; ++LamNum) {
                SpectralData(Loop).WaveLength(LamNum) = SpecDataProps(4 * LamNum - 3);
                SpectralData(Loop).Trans(LamNum) = SpecDataProps(4 * LamNum - 2);
                // Following is needed since angular calculation in subr TransAndReflAtPhi
                // fails for Trans = 0.0
                if (SpectralData(Loop).Trans(LamNum) < 0.001) SpectralData(Loop).Trans(LamNum) = 0.001;
                SpectralData(Loop).ReflFront(LamNum) = SpecDataProps(4 * LamNum - 1);
                SpectralData(Loop).ReflBack(LamNum) = SpecDataProps(4 * LamNum);
            }

            // Check integrity of the spectral data
            for (LamNum = 1; LamNum <= TotLam; ++LamNum) {
                Lam = SpectralData(Loop).WaveLength(LamNum);
                Tau = SpectralData(Loop).Trans(LamNum);
                RhoF = SpectralData(Loop).ReflFront(LamNum);
                RhoB = SpectralData(Loop).ReflBack(LamNum);
                if (LamNum < TotLam) {
                    if (SpectralData(Loop).WaveLength(LamNum + 1) <= Lam) {
                        ErrorsFound = true;
                        ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid set.");
                        ShowContinueError(state,
                                          format("... Wavelengths not in increasing order. at wavelength#={}, value=[{:.4T}], next is [{:.4T}].",
                                                 LamNum,
                                                 Lam,
                                                 SpectralData(Loop).WaveLength(LamNum + 1)));
                    }
                }

                if (Lam < 0.1 || Lam > 4.0) {
                    ErrorsFound = true;
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid value.");
                    ShowContinueError(
                        state, format("... A wavelength is not in the range 0.1 to 4.0 microns; at wavelength#={}, value=[{:.4T}].", LamNum, Lam));
                }

                // TH 2/15/2011. CR 8343
                // IGDB (International Glazing Database) does not meet the above strict restrictions.
                //  Relax rules to allow directly use of spectral data from IGDB
                if (Tau > 1.01) {
                    ErrorsFound = true;
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid value.");
                    ShowContinueError(state, format("... A transmittance is > 1.0; at wavelength#={}, value=[{:.4T}].", LamNum, Tau));
                }

                if (RhoF < 0.0 || RhoF > 1.02 || RhoB < 0.0 || RhoB > 1.02) {
                    ErrorsFound = true;
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid value.");
                    ShowContinueError(state, format("... A reflectance is < 0.0 or > 1.0; at wavelength#={}, RhoF value=[{:.4T}].", LamNum, RhoF));
                    ShowContinueError(state, format("... A reflectance is < 0.0 or > 1.0; at wavelength#={}, RhoB value=[{:.4T}].", LamNum, RhoB));
                }

                if ((Tau + RhoF) > 1.03 || (Tau + RhoB) > 1.03) {
                    ErrorsFound = true;
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + SpecDataNames(1) + "\" invalid value.");
                    ShowContinueError(state,
                                      "... Transmittance + reflectance) > 1.0 for an entry; at wavelength#=" +
                                          format("{}, value(Tau+RhoF)=[{:.4T}], value(Tau+RhoB)=[{:.4T}].", LamNum, (Tau + RhoF), (Tau + RhoB)));
                }
            }
        }

        if (TotSpectralData > 0) SpecDataProps.deallocate();
    }

    void ValidateMaterialRoughness(EnergyPlusData &state,
                                   int const MaterNum,           // Which Material number being validated.
                                   std::string const &Roughness, // Roughness String
                                   bool &ErrorsFound             // If errors found
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine compares the input Roughness value against the
        // valid values and sets the correct value in the Material Data Structure.

        // METHODOLOGY EMPLOYED:
        // Error message provided if not valid.

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

        // Select the correct Number for the associated ascii name for the roughness type
        if (UtilityRoutines::SameString(Roughness, "VeryRough")) state.dataMaterial->Material(MaterNum).Roughness = VeryRough;
        if (UtilityRoutines::SameString(Roughness, "Rough")) state.dataMaterial->Material(MaterNum).Roughness = Rough;
        if (UtilityRoutines::SameString(Roughness, "MediumRough")) state.dataMaterial->Material(MaterNum).Roughness = MediumRough;
        if (UtilityRoutines::SameString(Roughness, "MediumSmooth")) state.dataMaterial->Material(MaterNum).Roughness = MediumSmooth;
        if (UtilityRoutines::SameString(Roughness, "Smooth")) state.dataMaterial->Material(MaterNum).Roughness = Smooth;
        if (UtilityRoutines::SameString(Roughness, "VerySmooth")) state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;

        // Was it set?
        if (state.dataMaterial->Material(MaterNum).Roughness == 0) {
            ShowSevereError(state, "Material=" + state.dataMaterial->Material(MaterNum).Name + ",Illegal Roughness=" + Roughness);
            ErrorsFound = true;
        }
    }

    void GetConstructData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   September 1997
        //       MODIFIED       January 2003, FCW: accommodate between-glass shading device
        //                      July 2009, TH: added constructions defined with F and C factors
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the input through the input processor for Constructions.
        // Data read in this routine is stored in a derived type (Construct)
        // defined in the DataHeatBalance module.
        // This subroutine only sets those parameters which must be obtained
        // from the input file--all other portions of the Construct derived
        // type are set during the initializations.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;

        // If UniqueConstructionNames size, then input has already been gotten
        if (UniqueConstructNames.size()) return;

        int ConstrNum;                                                           // Counter to keep track of the construction number
        int Layer;                                                               // loop index for each of the construction layers
        int ConstructNumAlpha;                                                   // Number of construction alpha names being passed
        int DummyNumProp;                                                        // dummy variable for properties being passed
        int IOStat;                                                              // IO Status when calling get input subroutine
        Array1D_string ConstructAlphas({0, Construction::MaxLayersInConstruct}); // Construction Alpha names defined
        Array1D<Real64> DummyProps(5);                                           // Temporary array to transfer construction properties
        int Loop;
        int TotRegConstructs; // Number of "regular" constructions (no embedded sources or sinks and

        int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
        int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

        // int TotSourceConstructs;  // Number of constructions with embedded sources or sinks
        int TotWindow5Constructs; // Number of constructions from Window5 data file
        bool ConstructionFound;   // True if input window construction name is found in the
        //  Window5 data file
        bool EOFonW5File;       // True if EOF encountered reading Window5 data file
        int MaterialLayerGroup; // window construction layer material group index

        int iMatGlass; // number of glass layers
        Array1D_string WConstructNames;



        // Get the Total number of Constructions from the input
        TotRegConstructs = inputProcessor->getNumObjectsFound(state, "Construction");
        int totAirBoundaryConstructs = inputProcessor->getNumObjectsFound(state, "Construction:AirBoundary");

        TotFfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:FfactorGroundFloor");
        TotCfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:CfactorUndergroundWall");

        if (TotFfactorConstructs > 0) {
            NoFfactorConstructionsUsed = false;
        }

        if (TotCfactorConstructs > 0) {
            NoCfactorConstructionsUsed = false;
        }

        state.dataBSDFWindow->TotComplexFenStates = inputProcessor->getNumObjectsFound(state, "Construction:ComplexFenestrationState");
        TotWindow5Constructs = inputProcessor->getNumObjectsFound(state, "Construction:WindowDataFile");
        TotWinEquivLayerConstructs = inputProcessor->getNumObjectsFound(state, "Construction:WindowEquivalentLayer");

        WConstructNames.allocate(TotWindow5Constructs);

        TotConstructs = TotRegConstructs + TotFfactorConstructs + TotCfactorConstructs + totAirBoundaryConstructs +
                        state.dataBSDFWindow->TotComplexFenStates + TotWinEquivLayerConstructs;

        NominalRforNominalUCalculation.dimension(TotConstructs, 0.0);
        NominalU.dimension(TotConstructs, 0.0);

        // Allocate the array to the number of constructions/initialize selected variables
        state.dataConstruction->Construct.allocate(TotConstructs);
        UniqueConstructNames.reserve(TotConstructs);
        // Note: If TotWindow5Constructs > 0, additional constructions are created in
        // subr. SearchWindow5DataFile corresponding to those found on the data file.
        for (auto &e : state.dataConstruction->Construct) {
            // Initialize CTF and History terms
            e.NumCTFTerms = 0;
            e.NumHistories = 0;

            // Initialize some heat source/sink variables
            e.SourceSinkPresent = false; // "default" is no source or sink present
            e.SolutionDimensions = 1;    // "default" is 1-D heat transfer
            e.SourceAfterLayer = 0;      // this has no meaning if a source/sink is not present
            e.TempAfterLayer = 0;        // this has no meaning if a source/sink is not present
            e.ThicknessPerpend = 0.0;    // this has no meaning if a source/sink is not present

            e.W5FrameDivider = 0;
            e.FromWindow5DataFile = false;
        }

        ConstrNum = 0;

        CurrentModuleObject = "Construction";
        for (Loop = 1; Loop <= TotRegConstructs; ++Loop) { // Loop through all constructs in the input...

            // Get the object names for each construction from the input processor
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          ConstructAlphas,
                                          ConstructNumAlpha,
                                          DummyProps,
                                          DummyNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueConstructNames, ConstructAlphas(0), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            // Glass layer counter
            iMatGlass = 0;

            ++ConstrNum;
            // Assign Construction name to the Derived Type using the zeroth position of the array
            state.dataConstruction->Construct(ConstrNum).Name = ConstructAlphas(0);

            // Set the total number of layers for the construction
            state.dataConstruction->Construct(ConstrNum).TotLayers = ConstructNumAlpha - 1;

            // Loop through all of the layers of the construct to match the material names.
            // The loop index is the number minus 1
            for (Layer = 1; Layer <= ConstructNumAlpha - 1; ++Layer) {

                // Find the material in the list of materials

                state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) =
                    UtilityRoutines::FindItemInList(ConstructAlphas(Layer), state.dataMaterial->Material);

                // count number of glass layers
                if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) > 0) {
                    if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Group == WindowGlass)
                        ++iMatGlass;
                    MaterialLayerGroup = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Group;
                    if ((MaterialLayerGroup == GlassEquivalentLayer) || (MaterialLayerGroup == ShadeEquivalentLayer) ||
                        (MaterialLayerGroup == DrapeEquivalentLayer) || (MaterialLayerGroup == BlindEquivalentLayer) ||
                        (MaterialLayerGroup == ScreenEquivalentLayer) || (MaterialLayerGroup == GapEquivalentLayer)) {
                        ShowSevereError(state,
                                        "Invalid material layer type in window " + CurrentModuleObject + " = " +
                                            state.dataConstruction->Construct(ConstrNum).Name);
                        ShowSevereError(state,
                                        "Equivalent Layer material type = " + ConstructAlphas(Layer) +
                                            " is allowed only in Construction:WindowEquivalentLayer window object.");
                        ErrorsFound = true;
                    }
                }

                if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) == 0) {
                    // This may be a TC GlazingGroup
                    state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) =
                        UtilityRoutines::FindItemInList(ConstructAlphas(Layer), TCGlazings);

                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) > 0) {
                        // reset layer pointer to the first glazing in the TC GlazingGroup
                        state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) =
                            TCGlazings(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).LayerPoint(1);
                        state.dataConstruction->Construct(ConstrNum).TCLayer = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Group == WindowGlass)
                            ++iMatGlass;
                        state.dataConstruction->Construct(ConstrNum).TCFlag = 1;
                        state.dataConstruction->Construct(ConstrNum).TCMasterConst = ConstrNum;
                        state.dataConstruction->Construct(ConstrNum).TCGlassID = iMatGlass; // the TC glass layer ID
                        state.dataConstruction->Construct(ConstrNum).TCLayerID = Layer;
                        state.dataConstruction->Construct(ConstrNum).TypeIsWindow = true;
                    }
                }

                if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) == 0) {
                    ShowSevereError(state,
                                    "Did not find matching material for " + CurrentModuleObject + ' ' +
                                        state.dataConstruction->Construct(ConstrNum).Name + ", missing material = " + ConstructAlphas(Layer));
                    ErrorsFound = true;
                } else {
                    NominalRforNominalUCalculation(ConstrNum) += NominalR(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer));
                    if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Group == RegularMaterial &&
                        !state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).ROnly) {
                        NoRegularMaterialsUsed = false;
                    }
                }

            } // ...end of the Layer DO loop

        } // ...end of Regular Construction DO loop

        TotRegConstructs = ConstrNum;

        // Added TH 7/2009 for underground walls and floors constructions
        if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
            CreateFCfactorConstructions(state, ConstrNum, ErrorsFound);
            if (ErrorsFound) {
                ShowSevereError(state, "Errors found in creating the constructions defined with Ffactor or Cfactor method");
            }
            TotRegConstructs += TotFfactorConstructs + TotCfactorConstructs;
        }

        if (totAirBoundaryConstructs >= 1) {
            CreateAirBoundaryConstructions(state, ConstrNum, ErrorsFound);
            if (ErrorsFound) {
                ShowSevereError(state, "Errors found in creating the constructions defined with Construction:AirBoundary.");
            }
            TotRegConstructs += totAirBoundaryConstructs;
        }

        // Added BG 6/2010 for complex fenestration
        if (state.dataBSDFWindow->TotComplexFenStates > 0) {
            SetupComplexFenestrationStateInput(state, ConstrNum, ErrorsFound);
            if (ErrorsFound) {
                ShowSevereError(state, "Errors found in processing complex fenestration input");
            }
            TotRegConstructs += state.dataBSDFWindow->TotComplexFenStates;
        }

        CurrentModuleObject = "ConstructionProperty:InternalHeatSource";

        auto instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            AnyInternalHeatSourceInInput = true;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());

                std::string construction_name{UtilityRoutines::MakeUPPERCase(fields.at("construction_name"))};
                int source_after_layer_number{fields.at("thermal_source_present_after_layer_number")};
                int calculation_after_layer_number{fields.at("temperature_calculation_requested_after_layer_number")};
                int ctf_dimensions{fields.at("dimensions_for_the_ctf_calculation")};
                if ((ctf_dimensions < 1) || (ctf_dimensions > 2)) {
                    ShowWarningError(state, "ConstructionProperty:InternalHeatSource must be either 1- or 2-D.  Reset to 1-D solution.");
                    ShowContinueError(state, "Construction=" + construction_name + " is affected.");
                    ctf_dimensions = 1;
                }
                Real64 tube_spacing{fields.at("tube_spacing")};
                Real64 calculation_position{fields.at("two_dimensional_temperature_calculation_position")};

                // Find the construction
                int construction_index = UtilityRoutines::FindItemInList(construction_name, state.dataConstruction->Construct);

                if (construction_index == 0) {
                    ShowSevereError(state,
                                    "Did not find matching construction for " + CurrentModuleObject + ' ' + thisObjectName +
                                        ", missing construction = " + construction_name);
                    ErrorsFound = true;
                    continue;
                }

                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key());

                auto &thisConstruct(state.dataConstruction->Construct(construction_index));

                // May need some additional validation of the construction here
                if (thisConstruct.SourceSinkPresent) {
                    // Emulate old behavior by disallowing two sources in a single material
                    ShowSevereError(state, "Construction " + construction_name + " has more than one internal heat source referencing it, which is not allowed");
                    ErrorsFound = true;
                    continue;
                }

                thisConstruct.SourceSinkPresent = true;
                thisConstruct.SourceAfterLayer = source_after_layer_number;
                thisConstruct.TempAfterLayer = calculation_after_layer_number;
                thisConstruct.SolutionDimensions = ctf_dimensions;
                thisConstruct.ThicknessPerpend = tube_spacing / 2.0;
                thisConstruct.userTemperatureLocationPerpendicular =
                    thisConstruct.setUserTemperatureLocationPerpendicular(state, calculation_position);

                // Set the total number of layers for the construction
                if ((thisConstruct.SourceAfterLayer >= thisConstruct.TotLayers) || (thisConstruct.SourceAfterLayer <= 0)) {
                    ShowWarningError(state, "Construction " + thisConstruct.Name + " must have a source that is between two layers");
                    ShowContinueError(state, "The source after layer parameter has been set to one less than the number of layers.");
                    thisConstruct.SourceAfterLayer = thisConstruct.TotLayers - 1;
                }
                if ((thisConstruct.TempAfterLayer >= thisConstruct.TotLayers) || (thisConstruct.TempAfterLayer <= 0)) {
                    ShowWarningError(state, "Construction " + thisConstruct.Name + " must have a temperature calculation that is between two layers");
                    ShowContinueError(state, "The temperature calculation after layer parameter has been set to one less than the number of layers.");
                    thisConstruct.TempAfterLayer = thisConstruct.TotLayers - 1;
                }
            }
        }

        TotConstructs = TotRegConstructs;

        if (TotConstructs > 0 && (NoRegularMaterialsUsed && NoCfactorConstructionsUsed && NoFfactorConstructionsUsed)) {
            ShowWarningError(state, "This building has no thermal mass which can cause an unstable solution.");
            ShowContinueError(state, "Use Material object for all opaque material definitions except very light insulation layers.");
        }

        ConstrNum = 0;
        CurrentModuleObject = "Construction:WindowEquivalentLayer";
        for (Loop = 1; Loop <= TotWinEquivLayerConstructs; ++Loop) { // Loop through all constructs with Window EquivalentLayer ...

            // Get the object names for each construction from the input processor
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          ConstructAlphas,
                                          ConstructNumAlpha,
                                          DummyProps,
                                          DummyNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueConstructNames, ConstructAlphas(0), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            ++ConstrNum;
            // Assign Construction name to the Derived Type using the zeroth position of the array
            state.dataConstruction->Construct(TotRegConstructs + ConstrNum).Name = ConstructAlphas(0);

            // Set the total number of layers for the construction
            state.dataConstruction->Construct(TotRegConstructs + ConstrNum).TotLayers = ConstructNumAlpha - 1;
            if (state.dataConstruction->Construct(TotRegConstructs + ConstrNum).TotLayers < 1) {
                ShowSevereError(state,
                                "Construction " + state.dataConstruction->Construct(TotRegConstructs + ConstrNum).Name +
                                    " must have at least a single layer");
                ErrorsFound = true;
            }

            // Loop through all of the layers of the construct to match the material names.
            // The loop index is the number minus 1
            for (Layer = 1; Layer <= ConstructNumAlpha - 1; ++Layer) {

                // Find the material in the list of materials
                state.dataConstruction->Construct(TotRegConstructs + ConstrNum).LayerPoint(Layer) =
                    UtilityRoutines::FindItemInList(ConstructAlphas(Layer), state.dataMaterial->Material);

                if (state.dataConstruction->Construct(TotRegConstructs + ConstrNum).LayerPoint(Layer) == 0) {
                    ShowSevereError(state,
                                    "Did not find matching material for " + CurrentModuleObject + ' ' +
                                        state.dataConstruction->Construct(ConstrNum).Name + ", missing material = " + ConstructAlphas(Layer));
                    ErrorsFound = true;
                } else {
                    MaterialLayerGroup =
                        state.dataMaterial->Material(state.dataConstruction->Construct(TotRegConstructs + ConstrNum).LayerPoint(Layer)).Group;
                    if (!((MaterialLayerGroup == GlassEquivalentLayer) || (MaterialLayerGroup == ShadeEquivalentLayer) ||
                          (MaterialLayerGroup == DrapeEquivalentLayer) || (MaterialLayerGroup == BlindEquivalentLayer) ||
                          (MaterialLayerGroup == ScreenEquivalentLayer) || (MaterialLayerGroup == GapEquivalentLayer))) {
                        ShowSevereError(state,
                                        "Invalid material layer type in window " + CurrentModuleObject + " = " +
                                            state.dataConstruction->Construct(TotRegConstructs + ConstrNum).Name);
                        ShowContinueError(state,
                                          "...Window layer = " + ConstructAlphas(Layer) +
                                              " is not allowed in Construction:WindowEquivalentLayer window object.");
                        ShowContinueError(state, "Only materials of type Material:*:EquivalentLayer are allowed");
                        ErrorsFound = true;
                    }

                    if (ConstructNumAlpha <= 2) {

                    } else {
                        NominalRforNominalUCalculation(TotRegConstructs + ConstrNum) +=
                            NominalR(state.dataConstruction->Construct(TotRegConstructs + ConstrNum).LayerPoint(Layer));
                    }
                }

            } // Layer loop
            state.dataConstruction->Construct(TotRegConstructs + ConstrNum).EQLConsPtr = ConstrNum;
            state.dataConstruction->Construct(TotRegConstructs + ConstrNum).WindowTypeEQL = true;
        } // TotWinEquivLayerConstructs loop

        TotWinEquivLayerConstructs = ConstrNum;
        TotRegConstructs += TotWinEquivLayerConstructs;
        TotConstructs = TotRegConstructs;
        //-------------------------------------------------------------------------------
        ConstrNum = 0;

        CurrentModuleObject = "Construction:WindowDataFile";
        for (Loop = 1; Loop <= TotWindow5Constructs; ++Loop) { // Loop through all Window5 constructions. These constructions come
                                                               // from the Window5 data file and can be referenced only by windows

            // Get the object names for each construction from the input processor
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          ConstructAlphas,
                                          ConstructNumAlpha,
                                          DummyProps,
                                          DummyNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, ConstructAlphas(0), CurrentModuleObject, ErrorsFound)) continue;

            ++ConstrNum;
            WConstructNames(ConstrNum) = ConstructAlphas(0);

            // Obtain the data
            if (DummyNumProp != 0) {
                ShowSevereError(state, "Construction From Window5 Data File: there should be no numerical inputs for " + ConstructAlphas(0));
                ErrorsFound = true;
                continue;
            }

            // See if this construction is in the W5DataFile produced by the WINDOW 5 program;
            // if so, ConstructionFound will be set to true and the Material objects
            // associated with the construction will be created in subr. SearchWindow5DataFile.
            // (If the matching construction on the Window5 data file has two glazing systems, a
            // second construction and its associated materials will be created in subr.
            // SearchWindow5DataFile and TotConstructs WILL BE INCREMENTED BY 1 in that routine.
            // A FrameAndDivider object will also be created if window on data file has a
            // frame or divider.)

            std::string window5DataFileName;
            if (ConstructAlphas(1) == "") {
                window5DataFileName = CurrentWorkingFolder + "Window5DataFile.dat";
            } else {
                window5DataFileName = ConstructAlphas(1);
            }
            DisplayString(state, "Searching Window5 data file for Construction=" + ConstructAlphas(0));

            SearchWindow5DataFile(state, window5DataFileName, ConstructAlphas(0), ConstructionFound, EOFonW5File, ErrorsFound);

            if (EOFonW5File || !ConstructionFound) {
                DisplayString(state, "--Construction not found");
                ErrorsFound = true;
                ShowSevereError(state, "No match on WINDOW5 data file for Construction=" + ConstructAlphas(0) + ", or error in data file.");
                ShowContinueError(state, "...Looking on file=" + window5DataFileName);
                continue;
            }

        } // ...end of Window5 Constructions DO loop

        WConstructNames.deallocate();

        // set some (default) properties of the Construction Derived Type
        for (ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum) {

            // For air boundaries, skip TypeIsAirBoundary
            if (state.dataConstruction->Construct(ConstrNum).TypeIsAirBoundary) continue;
            if (NominalRforNominalUCalculation(ConstrNum) != 0.0) {
                NominalU(ConstrNum) = 1.0 / NominalRforNominalUCalculation(ConstrNum);
            } else {
                if (!state.dataConstruction->Construct(ConstrNum).WindowTypeEQL) {
                    ShowSevereError(state, "Nominal U is zero, for construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                    ErrorsFound = true;
                }
            }

            CheckAndSetConstructionProperties(state, ConstrNum, ErrorsFound);

        } // End of ConstrNum DO loop
    }

    void GetBuildingData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   November 1997
        //       MODIFIED       October 1998, FW; May 1999 FW; Oct 2004 LKL
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine calls other routines to get the Zone, and Surface data
        //  from the input file.

        // METHODOLOGY EMPLOYED:
        // The GetObjectItem routines are employed to retrieve the data.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace SurfaceGeometry;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        GetZoneData(state, ErrorsFound); // Read Zone data from input file

        SetupZoneGeometry(state, ErrorsFound);
    }

    void GetZoneData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   November 1997
        //       MODIFIED       PGE: Added ZONE LIST and ZONE GROUP objects, Nov 2003
        //                      RJH: Added init of DElight member of ZoneDaylight object, Jan 2004
        //                      JG: Added Part of Total Floor Area field March 2006
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the zone data for each zone in the input file.

        // METHODOLOGY EMPLOYED:
        // The GetObjectItem routines are employed to retrieve the data.

        // REFERENCES:
        // IDD Definition for Zone object

        // Using/Aliasing
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneData: ");
        //  INTEGER, PARAMETER :: MaxZonesInList = 100 ! This is to allow DIMENSIONing below

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNumbers;
        int IOStatus;
        int ZoneLoop;
        std::string::size_type TMP;
        int Loop;
        int ListNum;
        int ZoneNum;
        std::string ZoneName;
        int GroupNum;

        cCurrentModuleObject = "Zone";
        state.dataGlobal->NumOfZones = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        Zone.allocate(state.dataGlobal->NumOfZones);
        DataViewFactorInformation::ZoneRadiantInfo.allocate(state.dataGlobal->NumOfZones);
        DataViewFactorInformation::ZoneSolarInfo.allocate(state.dataGlobal->NumOfZones);
        state.dataDaylightingData->ZoneDaylight.allocate(state.dataGlobal->NumOfZones);

        ZoneLoop = 0;

        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {

            rNumericArgs = 0.0; // Zero out just in case
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            TMP = index(cAlphaArgs(1), char(1));
            while (TMP != std::string::npos) {
                cAlphaArgs(1)[TMP] = ',';
                TMP = index(cAlphaArgs(1), char(1));
            }
            TMP = index(cAlphaArgs(1), char(2));
            while (TMP != std::string::npos) {
                cAlphaArgs(1)[TMP] = '!';
                TMP = index(cAlphaArgs(1), char(2));
            }

            if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), CurrentModuleObject, ErrorsFound)) continue;

            ++ZoneLoop;
            ProcessZoneData(state,
                            cCurrentModuleObject,
                            ZoneLoop,
                            cAlphaArgs,
                            NumAlphas,
                            rNumericArgs,
                            NumNumbers,
                            lNumericFieldBlanks,
                            lAlphaFieldBlanks,
                            cAlphaFieldNames,
                            cNumericFieldNames,
                            ErrorsFound);

        } // Loop

        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            // Check to see if "nominally" controlled -- Zone Name appears in Zone Equip Configuration
            // relies on zone name being the "name" of the Zone Controlled Equip Configuration
            if (inputProcessor->getObjectItemNum(state, "ZoneHVAC:EquipmentConnections", "zone_name", Zone(Loop).Name) > 0) {
                Zone(Loop).isNominalControlled = true;
            } else {
                Zone(Loop).isNominalControlled = false;
            }
        }

        // Get ZONE LIST objects
        cCurrentModuleObject = "ZoneList";
        NumOfZoneLists = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (NumOfZoneLists > 0) {

            ZoneList.allocate(NumOfZoneLists);

            for (ListNum = 1; ListNum <= NumOfZoneLists; ++ListNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              ListNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                ZoneList(ListNum).Name = cAlphaArgs(1);
                if (UtilityRoutines::FindItemInList(ZoneList(ListNum).Name, Zone) > 0) {
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  is a duplicate of a zone name.");
                    ShowContinueError(state, "This could be a problem in places where either a Zone Name or a Zone List can be used.");
                }

                // List of zones
                ZoneList(ListNum).NumOfZones = NumAlphas - 1;

                if (ZoneList(ListNum).NumOfZones < 1) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  No zones specified.");
                    ErrorsFound = true;
                } else {
                    ZoneList(ListNum).Zone.allocate(ZoneList(ListNum).NumOfZones);
                    ZoneList(ListNum).Zone = 0;

                    for (ZoneNum = 1; ZoneNum <= ZoneList(ListNum).NumOfZones; ++ZoneNum) {
                        ZoneName = cAlphaArgs(ZoneNum + 1);
                        ZoneList(ListNum).MaxZoneNameLength = max(ZoneList(ListNum).MaxZoneNameLength, len(ZoneName));
                        ZoneList(ListNum).Zone(ZoneNum) = UtilityRoutines::FindItemInList(ZoneName, Zone);
                        if (ZoneList(ListNum).Zone(ZoneNum) == 0) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  " + cAlphaFieldNames(ZoneNum + 1) +
                                                ' ' + ZoneName + " not found.");
                            ErrorsFound = true;
                        }

                        // Check for duplicate zones
                        for (Loop = 1; Loop <= ZoneNum - 1; ++Loop) {
                            if (ZoneList(ListNum).Zone(ZoneNum) == ZoneList(ListNum).Zone(Loop)) {
                                ShowSevereError(state,
                                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  " + cAlphaFieldNames(ZoneNum + 1) +
                                                    ' ' + ZoneName + " appears more than once in list.");
                                ErrorsFound = true;
                            }
                        } // Loop
                    }     // ZoneNum
                }

            } // ListNum
        }

        // Get ZONE GROUP objects
        cCurrentModuleObject = "ZoneGroup";
        NumOfZoneGroups = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (NumOfZoneGroups > 0) {
            ZoneGroup.allocate(NumOfZoneGroups);

            for (GroupNum = 1; GroupNum <= NumOfZoneGroups; ++GroupNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              GroupNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                ZoneGroup(GroupNum).Name = cAlphaArgs(1);

                // Multiplier - checked already by IDD rules
                ZoneGroup(GroupNum).Multiplier = rNumericArgs(1);

                // Zone list
                ListNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), ZoneList);
                ZoneGroup(GroupNum).ZoneList = ListNum;

                if (ListNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  " + cAlphaFieldNames(2) + " named " +
                                        cAlphaArgs(2) + " not found.");
                    ErrorsFound = true;
                } else {
                    // Check to make sure list is not in use by another ZONE GROUP
                    for (Loop = 1; Loop <= GroupNum - 1; ++Loop) {
                        if (ZoneGroup(GroupNum).ZoneList == ZoneGroup(Loop).ZoneList) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  " + cAlphaFieldNames(2) +
                                                " already used by " + cCurrentModuleObject + " named " + ZoneGroup(Loop).Name + '.');
                            ErrorsFound = true;
                        }
                    } // Loop

                    // Set group multiplier for each zone in the list
                    for (Loop = 1; Loop <= ZoneList(ListNum).NumOfZones; ++Loop) {
                        ZoneNum = ZoneList(ListNum).Zone(Loop);

                        if (ZoneNum > 0) {
                            // Check to make sure group multiplier was not already set by another ZONE GROUP
                            if (Zone(ZoneNum).ListGroup == 0) {
                                Zone(ZoneNum).ListMultiplier = ZoneGroup(GroupNum).Multiplier;
                                Zone(ZoneNum).ListGroup = ListNum;
                            } else {
                                ShowSevereError(state,
                                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\":  Zone " + Zone(ZoneNum).Name +
                                                    " in ZoneList already exists in ZoneList of another ZoneGroup.");
                                ShowContinueError(state, "Previous ZoneList=" + ZoneList(Zone(ZoneNum).ListGroup).Name);
                                ErrorsFound = true;
                            }
                        }
                    } // Loop
                }

            } // GroupNum
        }

        GetZoneLocalEnvData(state, ErrorsFound);

        // allocate the array the holds the predefined report data
        ZonePreDefRep.allocate(state.dataGlobal->NumOfZones);
    }

    void GetZoneLocalEnvData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         X LUO
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for Outdoor Air Node for zones

        // METHODOLOGY EMPLOYED:
        // usual E+ input processes

        // Using/Aliasing
        using namespace DataIPShortCuts;

        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;

        using DataHeatBalance::ZoneLocalEnvironment;
        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeType_Air;
        using DataLoopNode::ObjectIsParent;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneLocalEnvData: ");

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumeric;

        int Loop;
        int ZoneLoop;
        int ZoneNum; // DO loop counter for zones
        int TotZoneEnv;
        int IOStat;
        int NodeNum;

        //-----------------------------------------------------------------------
        //               ZoneProperty:LocalEnvironment
        //-----------------------------------------------------------------------

        cCurrentModuleObject = "ZoneProperty:LocalEnvironment";
        TotZoneEnv = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotZoneEnv > 0) {
            // Check if IDD definition is correct
            state.dataGlobal->AnyLocalEnvironmentsInModel = true;

            if (!allocated(ZoneLocalEnvironment)) {
                ZoneLocalEnvironment.allocate(TotZoneEnv);
            }

            for (Loop = 1; Loop <= TotZoneEnv; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlpha,
                                              rNumericArgs,
                                              NumNumeric,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                ZoneLocalEnvironment(Loop).Name = cAlphaArgs(1);

                // Assign zone number
                ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
                if (ZoneNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                          "\" no corresponding zone has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    ZoneLocalEnvironment(Loop).ZonePtr = ZoneNum;
                }

                // Assign outdoor air node number;
                NodeNum = GetOnlySingleNode(state,
                                            cAlphaArgs(3),
                                            ErrorsFound,
                                            cCurrentModuleObject,
                                            cAlphaArgs(1),
                                            NodeType_Air,
                                            NodeConnectionType_Inlet,
                                            1,
                                            ObjectIsParent);
                if (NodeNum == 0 && CheckOutAirNodeNumber(state, NodeNum)) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
                                          "\" no corresponding schedule has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    ZoneLocalEnvironment(Loop).OutdoorAirNodePtr = NodeNum;
                }
            }
        }
        // Link zone properties to zone object
        for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
            for (Loop = 1; Loop <= TotZoneEnv; ++Loop) {
                if (ZoneLocalEnvironment(Loop).ZonePtr == ZoneLoop) {
                    if (ZoneLocalEnvironment(Loop).OutdoorAirNodePtr != 0) {
                        Zone(ZoneLoop).HasLinkedOutAirNode = true;
                        Zone(ZoneLoop).LinkedOutAirNode = ZoneLocalEnvironment(Loop).OutdoorAirNodePtr;
                    }
                }
            }
        }
    }

    void ProcessZoneData(EnergyPlusData &state,
                         std::string const &cCurrentModuleObject,
                         int const ZoneLoop,
                         Array1D_string const &cAlphaArgs,
                         int &NumAlphas,
                         Array1D<Real64> const &rNumericArgs,
                         int &NumNumbers,
                         [[maybe_unused]] Array1D_bool const &lNumericFieldBlanks, // Unused
                         Array1D_bool const &lAlphaFieldBlanks,
                         Array1D_string const &cAlphaFieldNames,
                         [[maybe_unused]] Array1D_string const &cNumericFieldNames, // Unused
                         bool &ErrorsFound                                          // If errors found in input
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   November 1997
        //       MODIFIED       PGE: Added ZONE LIST and ZONE GROUP objects, Nov 2003
        //                      RJH: Added init of DElight member of ZoneDaylight object, Jan 2004
        //                      JG: Added Part of Total Floor Area field March 2006
        //       RE-ENGINEERED  MJW: Split out processing zone input to facilitate unit testing, Nov 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the zone data for each zone in the input file.

        // METHODOLOGY EMPLOYED:
        // The GetObjectItem routines are employed to retrieve the data.

        // REFERENCES:
        // IDD Definition for Zone object

        // Using/Aliasing
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ProcessZoneData: ");
        //  INTEGER, PARAMETER :: MaxZonesInList = 100 ! This is to allow DIMENSIONing below

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Zone(ZoneLoop).Name = cAlphaArgs(1);
        if (NumNumbers >= 1) Zone(ZoneLoop).RelNorth = rNumericArgs(1);
        if (NumNumbers >= 2) Zone(ZoneLoop).OriginX = rNumericArgs(2);
        if (NumNumbers >= 3) Zone(ZoneLoop).OriginY = rNumericArgs(3);
        if (NumNumbers >= 4) Zone(ZoneLoop).OriginZ = rNumericArgs(4);
        if (NumNumbers >= 5) Zone(ZoneLoop).OfType = rNumericArgs(5);
        Zone(ZoneLoop).OfType = StandardZone;
        if (NumNumbers >= 6) Zone(ZoneLoop).Multiplier = rNumericArgs(6);
        if (NumNumbers >= 7) Zone(ZoneLoop).CeilingHeight = rNumericArgs(7);
        if (NumNumbers >= 8) Zone(ZoneLoop).Volume = rNumericArgs(8);
        if (NumNumbers >= 9) Zone(ZoneLoop).UserEnteredFloorArea = rNumericArgs(9);

        if (NumAlphas > 1 && !lAlphaFieldBlanks(2)) {
            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "SIMPLE") {
                    Zone(ZoneLoop).InsideConvectionAlgo = ASHRAESimple;

                } else if ((SELECT_CASE_var == "TARP")) {
                    Zone(ZoneLoop).InsideConvectionAlgo = ASHRAETARP;

                } else if (SELECT_CASE_var == "CEILINGDIFFUSER") {
                    Zone(ZoneLoop).InsideConvectionAlgo = CeilingDiffuser;

                } else if (SELECT_CASE_var == "TROMBEWALL") {
                    Zone(ZoneLoop).InsideConvectionAlgo = TrombeWall;

                } else if (SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM") {
                    Zone(ZoneLoop).InsideConvectionAlgo = AdaptiveConvectionAlgorithm;

                } else if (SELECT_CASE_var == "ASTMC1340") {
                    Zone(ZoneLoop).InsideConvectionAlgo = ASTMC1340;

                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + Zone(ZoneLoop).Name + "\".");
                    ShowContinueError(state, "Invalid value for " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                    // Zone( ZoneLoop ).InsideConvectionAlgo = ASHRAETARP;
                }
            }
        } else {
            // No zone specific algorithm specified, use default Inside Convection Algorithm
            Zone(ZoneLoop).InsideConvectionAlgo = DefaultInsideConvectionAlgo;
        }

        if (NumAlphas > 2 && !lAlphaFieldBlanks(3)) {
            {
                auto const SELECT_CASE_var(cAlphaArgs(3));

                if ((SELECT_CASE_var == "SIMPLECOMBINED")) {
                    Zone(ZoneLoop).OutsideConvectionAlgo = ASHRAESimple;

                } else if ((SELECT_CASE_var == "TARP")) {
                    Zone(ZoneLoop).OutsideConvectionAlgo = ASHRAETARP;

                } else if (SELECT_CASE_var == "MOWITT") {
                    Zone(ZoneLoop).OutsideConvectionAlgo = MoWiTTHcOutside;

                } else if (SELECT_CASE_var == "DOE-2") {
                    Zone(ZoneLoop).OutsideConvectionAlgo = DOE2HcOutside;

                } else if (SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM") {
                    Zone(ZoneLoop).OutsideConvectionAlgo = AdaptiveConvectionAlgorithm;

                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + Zone(ZoneLoop).Name + "\".");
                    ShowContinueError(state, "Invalid value for " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                    // Zone( ZoneLoop ).OutsideConvectionAlgo = AdaptiveConvectionAlgorithm;
                }
            }
        } else {
            // No zone specific algorithm specified, use default Outside Convection Algorithm
            Zone(ZoneLoop).OutsideConvectionAlgo = DefaultOutsideConvectionAlgo;
        }

        // Process the input field:    Part of Total Floor Area
        //   The default value is YES and so only NO needs to be handled
        if (NumAlphas > 3) {
            if (UtilityRoutines::SameString("No", cAlphaArgs(4))) {
                Zone(ZoneLoop).isPartOfTotalArea = false;
            } else if (UtilityRoutines::SameString("Yes", cAlphaArgs(4)) || lAlphaFieldBlanks(4)) {
                Zone(ZoneLoop).isPartOfTotalArea = true;
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + Zone(ZoneLoop).Name + "\".");
                ShowContinueError(state, "Invalid value for " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                ErrorsFound = true;
            }
        }

        // Zone outdoor environmental variables, used for zone infiltration/ventilation
        SetupOutputVariable(state,
                            "Zone Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            Zone(ZoneLoop).OutDryBulbTemp,
                            "Zone",
                            "Average",
                            Zone(ZoneLoop).Name);
        SetupOutputVariable(state,
                            "Zone Outdoor Air Wetbulb Temperature",
                            OutputProcessor::Unit::C,
                            Zone(ZoneLoop).OutWetBulbTemp,
                            "Zone",
                            "Average",
                            Zone(ZoneLoop).Name);
        SetupOutputVariable(
            state, "Zone Outdoor Air Wind Speed", OutputProcessor::Unit::m_s, Zone(ZoneLoop).WindSpeed, "Zone", "Average", Zone(ZoneLoop).Name);
        SetupOutputVariable(
            state, "Zone Outdoor Air Wind Direction", OutputProcessor::Unit::deg, Zone(ZoneLoop).WindDir, "Zone", "Average", Zone(ZoneLoop).Name);
    }

    // End of Get Input subroutines for the HB Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitHeatBalance(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   April 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main driver for initializations within the
        // heat balance.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initialization events.  Some of the files
        //  have been moved to other heat balance managers.  More of these initializations
        //  will have to continue to be re-structured.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace WindowManager;
        using namespace SolarShading;
        using DataLoopNode::Node;
        using DataSystemVariables::DetailedSolarTimestepIntegration;
        using DataSystemVariables::ReportExtShadingSunlitFrac;
        using DaylightingDevices::InitDaylightingDevices;
        using OutAirNodeManager::SetOutAirNodes;
        //  USE DataRoomAirModel, ONLY: IsZoneDV,IsZoneCV,HVACMassFlow, ZoneDVMixedFlag
        using WindowEquivalentLayer::InitEquivalentLayerWindowCalculations;

        int StormWinNum; // Number of StormWindow object
        int SurfNum;     // Surface number
        int ZoneNum;

        if (state.dataGlobal->BeginSimFlag) {
            AllocateHeatBalArrays(state); // Allocate the Module Arrays
            if (DataHeatBalance::AnyCTF || DataHeatBalance::AnyEMPD) {
                DisplayString(state, "Initializing Response Factors");
                InitConductionTransferFunctions(state); // Initialize the response factors
            }

            DisplayString(state, "Initializing Window Optical Properties");
            InitEquivalentLayerWindowCalculations(state); // Initialize the EQL window optical properties
            // InitGlassOpticalCalculations(); // Initialize the window optical properties
            InitWindowOpticalCalculations(state);
            InitDaylightingDevices(state); // Initialize any daylighting devices
            DisplayString(state, "Initializing Solar Calculations");
            InitSolarCalculations(state); // Initialize the shadowing calculations
        }

        if (state.dataGlobal->BeginEnvrnFlag) {

            MaxHeatLoadPrevDay = 0.0;
            MaxCoolLoadPrevDay = 0.0;
            MaxTempPrevDay = 0.0;
            MinTempPrevDay = 0.0;
            MaxHeatLoadZone = -9999.0;
            MaxCoolLoadZone = -9999.0;
            MaxTempZone = -9999.0;
            MinTempZone = 1000.0;
            TempZone = -9999.0;
            LoadZone = -9999.0;
            TempZonePrevDay = 1000.0;
            LoadZonePrevDay = -9999.0;
            TempZoneSecPrevDay = 1000.0;
            TempZoneSecPrevDay = -9999.0;
            WarmupTempDiff = 0.0;
            WarmupLoadDiff = 0.0;
            TempZoneRpt = 0.0;
            LoadZoneRpt = 0.0;
            MaxLoadZoneRpt = 0.0;
            CountWarmupDayPoints = 0;

            for (SurfNum = 1; SurfNum <= TotSurfaces; SurfNum++) {
                DataSurfaces::SurfaceWindow(SurfNum).ThetaFace = 296.15;
                DataSurfaces::SurfWinEffInsSurfTemp(SurfNum) = 23.0;
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            HeatBalanceSurfaceManager::InitEMSControlledConstructions(state);
            HeatBalanceSurfaceManager::InitEMSControlledSurfaceProperties(state);
        }

        if (TotStormWin > 0) {
            if (state.dataGlobal->BeginDayFlag) {
                SetStormWindowControl(state);
                ChangeSet = false;
            } else if (!ChangeSet) {
                StormWinChangeThisDay = false;
                for (StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
                    SurfNum = StormWindow(StormWinNum).BaseWindowNum;
                    DataSurfaces::SurfWinStormWinFlagPrevDay(SurfNum) = DataSurfaces::SurfWinStormWinFlag(SurfNum);
                }
                ChangeSet = true;
            }
        }

        if (state.dataGlobal->BeginSimFlag && state.dataGlobal->DoWeathSim && ReportExtShadingSunlitFrac) {
            OpenShadingFile(state);
        }

        if (state.dataGlobal->BeginDayFlag) {
            if (!state.dataGlobal->WarmupFlag) {
                if (state.dataGlobal->DayOfSim == 1) {
                    MaxHeatLoadZone = -9999.0;
                    MaxCoolLoadZone = -9999.0;
                    MaxTempZone = -9999.0;
                    MinTempZone = 1000.0;
                }
            }
            if (!DetailedSolarTimestepIntegration) {
                PerformSolarCalculations(state);
            }
        }

        if (DetailedSolarTimestepIntegration) { // always redo solar calcs
            PerformSolarCalculations(state);
        }

        if (state.dataGlobal->BeginDayFlag && !state.dataGlobal->WarmupFlag &&
            state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather && ReportExtShadingSunlitFrac) {
            for (int iHour = 1; iHour <= 24; ++iHour) { // Do for all hours.
                for (int TS = 1; TS <= state.dataGlobal->NumOfTimeStepInHour; ++TS) {
                    static constexpr auto ShdFracFmt1(" {:02}/{:02} {:02}:{:02},");
                    if (TS == state.dataGlobal->NumOfTimeStepInHour) {
                        print(state.files.shade, ShdFracFmt1, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, iHour, 0);
                    } else {
                        print(state.files.shade,
                              ShdFracFmt1,
                              state.dataEnvrn->Month,
                              state.dataEnvrn->DayOfMonth,
                              iHour - 1,
                              (60 / state.dataGlobal->NumOfTimeStepInHour) * TS);
                    }
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        static constexpr auto ShdFracFmt2("{:10.8F},");
                        print(state.files.shade, ShdFracFmt2, SunlitFrac(TS, iHour, SurfNum));
                    }
                    print(state.files.shade, "\n");
                }
            }
        }

        // Initialize zone outdoor environmental variables
        // Bulk Initialization for Temperatures & WindSpeed
        // using the zone, modify the zone  Dry/Wet BulbTemps
        SetZoneOutBulbTempAt(state);
        CheckZoneOutBulbTempAt(state);

        // set zone level wind dir to global value
        SetZoneWindSpeedAt(state);
        SetZoneWindDirAt(state);

        // Set zone data to linked air node value if defined.
        if (state.dataGlobal->AnyLocalEnvironmentsInModel) {
            SetOutAirNodes(state);
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).HasLinkedOutAirNode) {
                    if (Node(Zone(ZoneNum).LinkedOutAirNode).OutAirDryBulbSchedNum > 0) {
                        Zone(ZoneNum).OutDryBulbTemp = GetCurrentScheduleValue(state, Node(Zone(ZoneNum).LinkedOutAirNode).OutAirDryBulbSchedNum);
                    } else {
                        Zone(ZoneNum).OutDryBulbTemp = Node(Zone(ZoneNum).LinkedOutAirNode).OutAirDryBulb;
                    }
                    if (Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWetBulbSchedNum > 0) {
                        Zone(ZoneNum).OutWetBulbTemp = GetCurrentScheduleValue(state, Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWetBulbSchedNum);
                    } else {
                        Zone(ZoneNum).OutWetBulbTemp = Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWetBulb;
                    }
                    if (Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindSpeedSchedNum > 0) {
                        Zone(ZoneNum).WindSpeed = GetCurrentScheduleValue(state, Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindSpeedSchedNum);
                    } else {
                        Zone(ZoneNum).WindSpeed = Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindSpeed;
                    }
                    if (Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindDirSchedNum > 0) {
                        Zone(ZoneNum).WindDir = GetCurrentScheduleValue(state, Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindDirSchedNum);
                    } else {
                        Zone(ZoneNum).WindDir = Node(Zone(ZoneNum).LinkedOutAirNode).OutAirWindDir;
                    }
                }
            }
        }

        // Overwriting surface and zone level environmental data with EMS override value
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).OutDryBulbTempEMSOverrideOn) {
                    Zone(ZoneNum).OutDryBulbTemp = Zone(ZoneNum).OutDryBulbTempEMSOverrideValue;
                }
                if (Zone(ZoneNum).OutWetBulbTempEMSOverrideOn) {
                    Zone(ZoneNum).OutWetBulbTemp = Zone(ZoneNum).OutWetBulbTempEMSOverrideValue;
                }
                if (Zone(ZoneNum).WindSpeedEMSOverrideOn) {
                    Zone(ZoneNum).WindSpeed = Zone(ZoneNum).WindSpeedEMSOverrideValue;
                }
                if (Zone(ZoneNum).WindDirEMSOverrideOn) {
                    Zone(ZoneNum).WindDir = Zone(ZoneNum).WindDirEMSOverrideValue;
                }
            }
        }

        if (state.dataGlobal->BeginSimFlag) {
            for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
                int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
                int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    if (DataSurfaces::SurfWinWindowModelType(SurfNum) != DataSurfaces::WindowBSDFModel &&
                        DataSurfaces::SurfWinWindowModelType(SurfNum) != DataSurfaces::WindowEQLModel) {
                        DataSurfaces::SurfWinWindowModelType(SurfNum) = DataSurfaces::Window5DetailedModel;
                    }
                }
            }
        }
    }

    void AllocateHeatBalArrays(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   February 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine allocates the arrays to meet simulation requirements

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger variable allocation.

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na



        // Use the total number of zones or surfaces to allocate variables to avoid a limit
        // Allocate real Variables
        // Following used for Calculations
        //  Allocate variables in DataHeatBalSys
        SumConvHTRadSys.dimension(state.dataGlobal->NumOfZones, 0.0);
        SumLatentHTRadSys.dimension(state.dataGlobal->NumOfZones, 0.0);
        SumConvPool.dimension(state.dataGlobal->NumOfZones, 0.0);
        SumLatentPool.dimension(state.dataGlobal->NumOfZones, 0.0);
        QHTRadSysToPerson.dimension(state.dataGlobal->NumOfZones, 0.0);
        QHWBaseboardToPerson.dimension(state.dataGlobal->NumOfZones, 0.0);
        QSteamBaseboardToPerson.dimension(state.dataGlobal->NumOfZones, 0.0);
        QElecBaseboardToPerson.dimension(state.dataGlobal->NumOfZones, 0.0);
        QCoolingPanelToPerson.dimension(state.dataGlobal->NumOfZones, 0.0);
        XMAT.dimension(state.dataGlobal->NumOfZones, 23.0);
        XM2T.dimension(state.dataGlobal->NumOfZones, 23.0);
        XM3T.dimension(state.dataGlobal->NumOfZones, 23.0);
        XM4T.dimension(state.dataGlobal->NumOfZones, 23.0);
        DSXMAT.dimension(state.dataGlobal->NumOfZones, 23.0);
        DSXM2T.dimension(state.dataGlobal->NumOfZones, 23.0);
        DSXM3T.dimension(state.dataGlobal->NumOfZones, 23.0);
        DSXM4T.dimension(state.dataGlobal->NumOfZones, 23.0);
        XMPT.dimension(state.dataGlobal->NumOfZones, 23.0);
        MCPI.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPTI.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPV.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPTV.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPM.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPTM.dimension(state.dataGlobal->NumOfZones, 0.0);
        MixingMassFlowZone.dimension(state.dataGlobal->NumOfZones, 0.0);
        MixingMassFlowXHumRat.dimension(state.dataGlobal->NumOfZones, 0.0);
        ZoneReOrder.allocate(state.dataGlobal->NumOfZones);
        ZoneMassBalanceFlag.dimension(state.dataGlobal->NumOfZones, false);
        ZoneInfiltrationFlag.dimension(state.dataGlobal->NumOfZones, false);
        ZoneReOrder = 0;
        ZoneLatentGain.dimension(state.dataGlobal->NumOfZones, 0.0);
        ZoneLatentGainExceptPeople.dimension(state.dataGlobal->NumOfZones, 0.0); // Added for hybrid model
        OAMFL.dimension(state.dataGlobal->NumOfZones, 0.0);
        VAMFL.dimension(state.dataGlobal->NumOfZones, 0.0);
        ZTAV.dimension(state.dataGlobal->NumOfZones, 23.0);
        ZTAVComf.dimension(state.dataGlobal->NumOfZones, 23.0);
        ZT.dimension(state.dataGlobal->NumOfZones, 23.0);
        TempTstatAir.dimension(state.dataGlobal->NumOfZones, 23.0);
        MAT.dimension(state.dataGlobal->NumOfZones, 23.0);
        ZoneTMX.dimension(state.dataGlobal->NumOfZones, 23.0);
        ZoneTM2.dimension(state.dataGlobal->NumOfZones, 23.0);
        // Allocate this zone air humidity ratio
        ZoneAirHumRatAvg.dimension(state.dataGlobal->NumOfZones, 0.01);
        ZoneAirHumRatAvgComf.dimension(state.dataGlobal->NumOfZones, 0.01);
        ZoneAirHumRat.dimension(state.dataGlobal->NumOfZones, 0.01);
        ZoneAirHumRatOld.dimension(state.dataGlobal->NumOfZones, 0.01);
        SumHmAW.dimension(state.dataGlobal->NumOfZones, 0.0);
        SumHmARa.dimension(state.dataGlobal->NumOfZones, 0.0);
        SumHmARaW.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPTE.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPE.dimension(state.dataGlobal->NumOfZones, 0.0);
        EAMFL.dimension(state.dataGlobal->NumOfZones, 0.0);
        EAMFLxHumRat.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPTC.dimension(state.dataGlobal->NumOfZones, 0.0);
        MCPC.dimension(state.dataGlobal->NumOfZones, 0.0);
        CTMFL.dimension(state.dataGlobal->NumOfZones, 0.0);
        MDotCPOA.dimension(state.dataGlobal->NumOfZones, 0.0);
        MDotOA.dimension(state.dataGlobal->NumOfZones, 0.0);
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataContaminantBalance->OutdoorCO2 = GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr);
            state.dataContaminantBalance->ZoneAirCO2.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorCO2);
            state.dataContaminantBalance->ZoneAirCO2Temp.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorCO2);
            state.dataContaminantBalance->ZoneAirCO2Avg.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorCO2);
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataContaminantBalance->OutdoorGC =
                GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr);
            state.dataContaminantBalance->ZoneAirGC.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorGC);
            state.dataContaminantBalance->ZoneAirGCTemp.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorGC);
            state.dataContaminantBalance->ZoneAirGCAvg.dimension(state.dataGlobal->NumOfZones, state.dataContaminantBalance->OutdoorGC);
        }
        MaxTempPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        MinTempPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        MaxHeatLoadPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        MaxCoolLoadPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        MaxHeatLoadZone.dimension(state.dataGlobal->NumOfZones, -9999.0);
        MaxCoolLoadZone.dimension(state.dataGlobal->NumOfZones, -9999.0);
        MaxTempZone.dimension(state.dataGlobal->NumOfZones, -9999.0);
        MinTempZone.dimension(state.dataGlobal->NumOfZones, 1000.0);
        TempZonePrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        LoadZonePrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        TempZoneSecPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        LoadZoneSecPrevDay.dimension(state.dataGlobal->NumOfZones, 0.0);
        WarmupTempDiff.dimension(state.dataGlobal->NumOfZones, 0.0);
        WarmupLoadDiff.dimension(state.dataGlobal->NumOfZones, 0.0);
        TempZone.dimension(state.dataGlobal->NumOfZones, 0.0);
        LoadZone.dimension(state.dataGlobal->NumOfZones, 0.0);
        TempZoneRpt.dimension(state.dataGlobal->NumOfZones, state.dataGlobal->NumOfTimeStepInHour * 24, 0.0);
        LoadZoneRpt.dimension(state.dataGlobal->NumOfZones, state.dataGlobal->NumOfTimeStepInHour * 24, 0.0);
        MaxLoadZoneRpt.dimension(state.dataGlobal->NumOfZones, state.dataGlobal->NumOfTimeStepInHour * 24, 0.0);
        WarmupConvergenceValues.allocate(state.dataGlobal->NumOfZones);
        TempZoneRptStdDev.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        LoadZoneRptStdDev.allocate(state.dataGlobal->NumOfTimeStepInHour * 24);
        // MassConservation.allocate( NumOfZones );

        ZoneHeatIndex.dimension(state.dataGlobal->NumOfZones, 0.0);
        ZoneHumidex.dimension(state.dataGlobal->NumOfZones, 0.0);
        ZoneNumOcc.dimension(state.dataGlobal->NumOfZones, 0);
        ZoneHeatIndexHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneHumidexHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneHeatIndexOccuHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneHumidexOccuHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneCO2LevelHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneCO2LevelOccuHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneLightingLevelHourBins.allocate(state.dataGlobal->NumOfZones);
        ZoneLightingLevelOccuHourBins.allocate(state.dataGlobal->NumOfZones);

        ZoneOccPierceSET.dimension(state.dataGlobal->NumOfZones, 0);
        ZoneOccPierceSETLastStep.dimension(state.dataGlobal->NumOfZones, 0);
        ZoneLowSETHours.allocate(state.dataGlobal->NumOfZones);
        ZoneHighSETHours.allocate(state.dataGlobal->NumOfZones);

        CountWarmupDayPoints = 0;
    }

    // End Initialization Section of the Module
    //******************************************************************************

    // Beginning of Record Keeping subroutines for the HB Module
    // *****************************************************************************

    void RecKeepHeatBalance(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   April 1997
        //       MODIFIED       June 2011, Daeho Kang for individual zone maximums & convergence outputs
        //       				July 2016, Rick Strand for movable insulation bug fix

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main driver for record keeping within the
        // heat balance.

        // Using/Aliasing
        using DataSystemVariables::ReportDetailedWarmupConvergence;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        int SurfNum;



        // Record Maxs & Mins for individual zone
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (ZTAV(ZoneNum) > MaxTempZone(ZoneNum)) {
                MaxTempZone(ZoneNum) = ZTAV(ZoneNum);
            }
            if (ZTAV(ZoneNum) < MinTempZone(ZoneNum)) {
                MinTempZone(ZoneNum) = ZTAV(ZoneNum);
            }
            if (SNLoadHeatRate(ZoneNum) > MaxHeatLoadZone(ZoneNum)) {
                MaxHeatLoadZone(ZoneNum) = SNLoadHeatRate(ZoneNum);
            }
            if (SNLoadCoolRate(ZoneNum) > MaxCoolLoadZone(ZoneNum)) {
                MaxCoolLoadZone(ZoneNum) = SNLoadCoolRate(ZoneNum);
            }

            // Record temperature and load for individual zone
            TempZoneSecPrevDay(ZoneNum) = TempZonePrevDay(ZoneNum);
            LoadZoneSecPrevDay(ZoneNum) = LoadZonePrevDay(ZoneNum);
            TempZonePrevDay(ZoneNum) = TempZone(ZoneNum);
            LoadZonePrevDay(ZoneNum) = LoadZone(ZoneNum);
            TempZone(ZoneNum) = ZTAV(ZoneNum);
            LoadZone(ZoneNum) = max(SNLoadHeatRate(ZoneNum), std::abs(SNLoadCoolRate(ZoneNum)));

            // Calculate differences in temperature and load for the last two warmup days
            if (!state.dataGlobal->WarmupFlag && state.dataGlobal->DayOfSim == 1 && !state.dataGlobal->DoingSizing) {
                WarmupTempDiff(ZoneNum) = std::abs(TempZoneSecPrevDay(ZoneNum) - TempZonePrevDay(ZoneNum));
                WarmupLoadDiff(ZoneNum) = std::abs(LoadZoneSecPrevDay(ZoneNum) - LoadZonePrevDay(ZoneNum));
                if (ZoneNum == 1) ++CountWarmupDayPoints;
                TempZoneRpt(ZoneNum, CountWarmupDayPoints) = WarmupTempDiff(ZoneNum);
                LoadZoneRpt(ZoneNum, CountWarmupDayPoints) = WarmupLoadDiff(ZoneNum);
                MaxLoadZoneRpt(ZoneNum, CountWarmupDayPoints) = LoadZone(ZoneNum);

                if (ReportDetailedWarmupConvergence) { // only do this detailed thing when requested by user is on
                    // Write Warmup Convergence Information to the initialization output file
                    if (FirstWarmupWrite) {
                        static constexpr auto Format_732{"! <Warmup Convergence Information>,Zone Name,Time Step,Hour of Day,Warmup Temperature "
                                                         "Difference {{deltaC}},Warmup Load Difference {{W}}\n"};
                        print(state.files.eio, Format_732);
                        FirstWarmupWrite = false;
                    }
                    static constexpr auto Format_731{" Warmup Convergence Information, {},{},{},{:.10R},{:.10R}\n"};
                    print(state.files.eio,
                          Format_731,
                          Zone(ZoneNum).Name,
                          state.dataGlobal->TimeStep,
                          state.dataGlobal->HourOfDay,
                          WarmupTempDiff(ZoneNum),
                          WarmupLoadDiff(ZoneNum));
                }
            }
        }

        // Update interior movable insulation flag--needed at the end of a zone time step so that the interior radiant
        // exchange algorithm knows whether there has been a change in interior movable insulation or not.
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            DataSurfaces::Surface(SurfNum).MovInsulIntPresentPrevTS = DataSurfaces::Surface(SurfNum).MovInsulIntPresent;
        }

        // For non-complex windows, update a report variable so this shows up in the output as something other than zero
        UpdateWindowFaceTempsNonBSDFWin(state);
    }

    void CheckWarmupConvergence(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   April 1997
        //       MODIFIED       June 2011, Daeho Kang for individual zone comparison
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine checks warmup convergence values.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MinLoad(100.0); // Minimum laods for convergence check
        // To avoid big percentage difference in low load situations

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        bool ConvergenceChecksFailed;

        // Convergence criteria for warmup days:
        // Perform another warmup day unless both the % change in loads and
        // absolute change in zone temp min & max are less than their criteria.

        ConvergenceChecksFailed = false;

        if (state.dataGlobal->NumOfZones <= 0) { // if there are no zones, immediate convergence
            state.dataGlobal->WarmupFlag = false;
        } else {
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

                WarmupConvergenceValues(ZoneNum).TestMaxTempValue = std::abs(MaxTempPrevDay(ZoneNum) - MaxTempZone(ZoneNum));
                WarmupConvergenceValues(ZoneNum).TestMinTempValue = std::abs(MinTempPrevDay(ZoneNum) - MinTempZone(ZoneNum));
                if (WarmupConvergenceValues(ZoneNum).TestMaxTempValue <= TempConvergTol) {
                    WarmupConvergenceValues(ZoneNum).PassFlag(1) = 2;
                } else {
                    ConvergenceChecksFailed = true;
                    WarmupConvergenceValues(ZoneNum).PassFlag(1) = 1;
                }

                if (WarmupConvergenceValues(ZoneNum).TestMinTempValue <= TempConvergTol) {
                    WarmupConvergenceValues(ZoneNum).PassFlag(2) = 2;
                } else {
                    ConvergenceChecksFailed = true;
                    WarmupConvergenceValues(ZoneNum).PassFlag(2) = 1;
                }

                if (MaxHeatLoadZone(ZoneNum) > 1.0e-4) { // make sure load big enough to divide
                    MaxHeatLoadZone(ZoneNum) = std::abs(max(MaxHeatLoadZone(ZoneNum), MinLoad));
                    MaxHeatLoadPrevDay(ZoneNum) = std::abs(max(MaxHeatLoadPrevDay(ZoneNum), MinLoad));
                    WarmupConvergenceValues(ZoneNum).TestMaxHeatLoadValue =
                        std::abs((MaxHeatLoadZone(ZoneNum) - MaxHeatLoadPrevDay(ZoneNum)) / MaxHeatLoadZone(ZoneNum));
                    if (WarmupConvergenceValues(ZoneNum).TestMaxHeatLoadValue <= LoadsConvergTol) {
                        WarmupConvergenceValues(ZoneNum).PassFlag(3) = 2;
                    } else {
                        ConvergenceChecksFailed = true;
                        WarmupConvergenceValues(ZoneNum).PassFlag(3) = 1;
                    }
                } else {
                    WarmupConvergenceValues(ZoneNum).PassFlag(3) = 2;
                }

                if (MaxCoolLoadZone(ZoneNum) > 1.0e-4) {
                    MaxCoolLoadZone(ZoneNum) = std::abs(max(MaxCoolLoadZone(ZoneNum), MinLoad));
                    MaxCoolLoadPrevDay(ZoneNum) = std::abs(max(MaxCoolLoadPrevDay(ZoneNum), MinLoad));
                    WarmupConvergenceValues(ZoneNum).TestMaxCoolLoadValue =
                        std::abs((MaxCoolLoadZone(ZoneNum) - MaxCoolLoadPrevDay(ZoneNum)) / MaxCoolLoadZone(ZoneNum));
                    if (WarmupConvergenceValues(ZoneNum).TestMaxCoolLoadValue <= LoadsConvergTol) {
                        WarmupConvergenceValues(ZoneNum).PassFlag(4) = 2;
                    } else {
                        ConvergenceChecksFailed = true;
                        WarmupConvergenceValues(ZoneNum).PassFlag(4) = 1;
                    }
                } else {
                    WarmupConvergenceValues(ZoneNum).PassFlag(4) = 2;
                }

                if (state.dataGlobal->DayOfSim >= MaxNumberOfWarmupDays && state.dataGlobal->WarmupFlag) {
                    // Check convergence for individual zone
                    if (sum(WarmupConvergenceValues(ZoneNum).PassFlag) != 8) { // pass=2 * 4 values for convergence
                        ShowSevereError(state,
                                        format("CheckWarmupConvergence: Loads Initialization, Zone=\"{}\" did not converge after {} warmup days.",
                                               Zone(ZoneNum).Name,
                                               MaxNumberOfWarmupDays));
                        if (!WarmupConvergenceWarning && !state.dataGlobal->DoingSizing) {
                            ShowContinueError(state, "See Warmup Convergence Information in .eio file for details.");
                            WarmupConvergenceWarning = true;
                        } else if (!SizingWarmupConvergenceWarning && state.dataGlobal->DoingSizing) {
                            ShowContinueError(state, "Warmup Convergence failing during sizing.");
                            SizingWarmupConvergenceWarning = true;
                        }
                        if (state.dataEnvrn->RunPeriodEnvironment) {
                            ShowContinueError(state, "...Environment(RunPeriod)=\"" + state.dataEnvrn->EnvironmentName + "\"");
                        } else {
                            ShowContinueError(state, "...Environment(SizingPeriod)=\"" + state.dataEnvrn->EnvironmentName + "\"");
                        }

                        ShowContinueError(state,
                                          format("..Max Temp Comparison = {:.2R} vs Temperature Convergence Tolerance={:.2R} - {} Convergence",
                                                 WarmupConvergenceValues(ZoneNum).TestMaxTempValue,
                                                 TempConvergTol,
                                                 PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(1))));
                        ShowContinueError(state,
                                          format("..Min Temp Comparison = {:.2R} vs Temperature Convergence Tolerance={:.2R} - {} Convergence",
                                                 WarmupConvergenceValues(ZoneNum).TestMinTempValue,
                                                 TempConvergTol,
                                                 PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(2))));
                        ShowContinueError(state,
                                          format("..Max Heat Load Comparison = {:.4R} vs Loads Convergence Tolerance={:.2R} - {} Convergence",
                                                 WarmupConvergenceValues(ZoneNum).TestMaxHeatLoadValue,
                                                 LoadsConvergTol,
                                                 PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(3))));
                        ShowContinueError(state,
                                          format("..Max Cool Load Comparison = {:.4R} vs Loads Convergence Tolerance={:.2R} - {} Convergence",
                                                 WarmupConvergenceValues(ZoneNum).TestMaxCoolLoadValue,
                                                 LoadsConvergTol,
                                                 PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(4))));
                    }
                }

                // Transfer current daily max and min loads and temperatures to the
                // variables containing the last day's values
                MaxHeatLoadPrevDay(ZoneNum) = MaxHeatLoadZone(ZoneNum);
                MaxCoolLoadPrevDay(ZoneNum) = MaxCoolLoadZone(ZoneNum);
                MaxTempPrevDay(ZoneNum) = MaxTempZone(ZoneNum);
                MinTempPrevDay(ZoneNum) = MinTempZone(ZoneNum);

                MaxHeatLoadZone(ZoneNum) = -9999.0;
                MaxCoolLoadZone(ZoneNum) = -9999.0;
                MaxTempZone(ZoneNum) = -9999.0;
                MinTempZone(ZoneNum) = 1000.0;
            }

            // Limit the number of warmup days, regardless of the number of zones
            // in the building, to some arbitrary value based on common sense and
            // experience with the (I)BLAST program.  If too many warmup days were
            // required, notify the program user.

            if ((state.dataGlobal->DayOfSim >= MaxNumberOfWarmupDays) && state.dataGlobal->WarmupFlag && ConvergenceChecksFailed) {
                if (MaxNumberOfWarmupDays < DefaultMaxNumberOfWarmupDays) {
                    ShowSevereError(state,
                                    format("CheckWarmupConvergence: User supplied maximum warmup days={} is insufficient.", MaxNumberOfWarmupDays));
                    ShowContinueError(state, format("Suggest setting maximum number of warmup days to at least {}.", DefaultMaxNumberOfWarmupDays));
                }
            }

            // Set warmup flag to true depending on value of ConvergenceChecksFailed (true=fail)
            // and minimum number of warmup days
            if (!ConvergenceChecksFailed && state.dataGlobal->DayOfSim >= MinNumberOfWarmupDays) {
                state.dataGlobal->WarmupFlag = false;
            } else if (!ConvergenceChecksFailed && state.dataGlobal->DayOfSim < MinNumberOfWarmupDays) {
                state.dataGlobal->WarmupFlag = true;
            }

            // If max warmup days reached and still WarmupFlag, then go to non-warmup state.
            // prior messages will have been displayed
            if ((state.dataGlobal->DayOfSim >= MaxNumberOfWarmupDays) && state.dataGlobal->WarmupFlag) {
                state.dataGlobal->WarmupFlag = false;
            }
        }
    }

    void ReportWarmupConvergence(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // na

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

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
        int ZoneNum;
        Real64 AverageZoneTemp;
        Real64 AverageZoneLoad;
        Real64 StdDevZoneTemp;
        Real64 StdDevZoneLoad;
        std::string EnvHeader;
        int Num; // loop control

        // Formats
        static constexpr auto Format_730("! <Warmup Convergence Information>,Zone Name,Environment Type/Name,Average Warmup Temperature Difference "
                                         "{{deltaC}},Std Dev Warmup Temperature Difference {{deltaC}},Max Temperature Pass/Fail Convergence,Min "
                                         "Temperature Pass/Fail Convergence,Average Warmup Load Difference {{W}},Std Dev Warmup Load Difference "
                                         "{{W}},Heating Load Pass/Fail Convergence,Cooling Load Pass/Fail Convergence\n");

        if (!state.dataGlobal->WarmupFlag) { // Report out average/std dev
            // Write Warmup Convervence Information to the initialization output file
            if (ReportWarmupConvergenceFirstWarmupWrite && state.dataGlobal->NumOfZones > 0) {
                print(state.files.eio, Format_730);
                ReportWarmupConvergenceFirstWarmupWrite = false;
            }

            TempZoneRptStdDev = 0.0;
            LoadZoneRptStdDev = 0.0;

            if (state.dataEnvrn->RunPeriodEnvironment) {
                EnvHeader = "RunPeriod:";
            } else {
                EnvHeader = "SizingPeriod:";
            }

            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                AverageZoneTemp = sum(TempZoneRpt(ZoneNum, {1, CountWarmupDayPoints})) / double(CountWarmupDayPoints);
                for (Num = 1; Num <= CountWarmupDayPoints; ++Num) {
                    if (MaxLoadZoneRpt(ZoneNum, Num) > 1.e-4) {
                        LoadZoneRpt(ZoneNum, Num) /= MaxLoadZoneRpt(ZoneNum, Num);
                    } else {
                        LoadZoneRpt(ZoneNum, Num) = 0.0;
                    }
                }
                AverageZoneLoad = sum(LoadZoneRpt(ZoneNum, {1, CountWarmupDayPoints})) / double(CountWarmupDayPoints);
                StdDevZoneTemp = 0.0;
                StdDevZoneLoad = 0.0;
                for (Num = 1; Num <= CountWarmupDayPoints; ++Num) {
                    TempZoneRptStdDev(Num) = pow_2(TempZoneRpt(ZoneNum, Num) - AverageZoneTemp);
                    LoadZoneRptStdDev(Num) = pow_2(LoadZoneRpt(ZoneNum, Num) - AverageZoneLoad);
                }
                StdDevZoneTemp = std::sqrt(sum(TempZoneRptStdDev({1, CountWarmupDayPoints})) / double(CountWarmupDayPoints));
                StdDevZoneLoad = std::sqrt(sum(LoadZoneRptStdDev({1, CountWarmupDayPoints})) / double(CountWarmupDayPoints));

                static constexpr auto Format_731(" Warmup Convergence Information,{},{},{:.10R},{:.10R},{},{},{:.10R},{:.10R},{},{}\n");
                print(state.files.eio,
                      Format_731,
                      Zone(ZoneNum).Name,
                      EnvHeader + ' ' + state.dataEnvrn->EnvironmentName,
                      AverageZoneTemp,
                      StdDevZoneTemp,
                      PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(1)),
                      PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(2)),
                      AverageZoneLoad,
                      StdDevZoneLoad,
                      PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(3)),
                      PassFail(WarmupConvergenceValues(ZoneNum).PassFlag(4)));
            }
        }
    }

    void UpdateWindowFaceTempsNonBSDFWin(EnergyPlusData &state)
    {

        int SurfNum;

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            auto &thisSurface(DataSurfaces::Surface(SurfNum));
            if (thisSurface.Class == DataSurfaces::SurfaceClass::Window) {
                auto &thisConstruct(thisSurface.Construction);
                if (!state.dataConstruction->Construct(thisConstruct).WindowTypeBSDF) {
                    SurfWinFenLaySurfTempFront(1, SurfNum) = TH(1, 1, SurfNum);
                    SurfWinFenLaySurfTempBack(state.dataConstruction->Construct(thisConstruct).TotLayers, SurfNum) = TH(2, 1, SurfNum);
                }
            }
        }
    }

    //        End of Record Keeping subroutines for the HB Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the HB Module
    // *****************************************************************************

    void ReportHeatBalance(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   July 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main driver for reporting within the heat
        // balance.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger record keeping events.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSystemVariables::ReportDuringWarmup; // added for FMI
        using DataSystemVariables::UpdateDataDuringWarmupExternalInterface;
        using EconomicTariff::UpdateUtilityBills; // added for computing annual utility costs
        using NodeInputManager::CalcMoreNodeInfo;
        using OutputReportTabular::UpdateTabularReports;
        using ScheduleManager::ReportScheduleValues;
        using namespace DataReportingFlags;

        ReportScheduleValues(state);

        if (!state.dataGlobal->WarmupFlag && state.dataGlobal->DoOutputReporting) {
            CalcMoreNodeInfo(state);
            UpdateDataandReport(state, OutputProcessor::TimeStepType::TimeStepZone);
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay ||
                state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign) {
                if (hvacSizingSimulationManager) hvacSizingSimulationManager->UpdateSizingLogsZoneStep(state);
            }

            UpdateTabularReports(state, OutputProcessor::TimeStepType::TimeStepZone);
            UpdateUtilityBills(state);
        } else if (!state.dataGlobal->KickOffSimulation && state.dataGlobal->DoOutputReporting && ReportDuringWarmup) {
            if (state.dataGlobal->BeginDayFlag && !state.dataEnvrn->PrintEnvrnStampWarmupPrinted) {
                state.dataEnvrn->PrintEnvrnStampWarmup = true;
                state.dataEnvrn->PrintEnvrnStampWarmupPrinted = true;
            }
            if (!state.dataGlobal->BeginDayFlag) state.dataEnvrn->PrintEnvrnStampWarmupPrinted = false;
            if (state.dataEnvrn->PrintEnvrnStampWarmup) {
                if (PrintEndDataDictionary && state.dataGlobal->DoOutputReporting) {
                    static constexpr auto EndOfHeaderString("End of Data Dictionary"); // End of data dictionary marker
                    print(state.files.eso, "{}\n", EndOfHeaderString);
                    print(state.files.mtr, "{}\n", EndOfHeaderString);
                    PrintEndDataDictionary = false;
                }
                if (state.dataGlobal->DoOutputReporting) {
                    static constexpr auto EnvironmentStampFormatStr("{},{},{:7.2F},{:7.2F},{:7.2F},{:7.2F}\n"); // Format descriptor for environ stamp
                    print(state.files.eso,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);

                    print(state.files.mtr,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    state.dataEnvrn->PrintEnvrnStampWarmup = false;
                }
            }
            CalcMoreNodeInfo(state);
            UpdateDataandReport(state, OutputProcessor::TimeStepType::TimeStepZone);
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay ||
                state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign) {
                if (hvacSizingSimulationManager) hvacSizingSimulationManager->UpdateSizingLogsZoneStep(state);
            }

        } else if (UpdateDataDuringWarmupExternalInterface) { // added for FMI
            UpdateDataandReport(state, OutputProcessor::TimeStepType::TimeStepZone);
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay ||
                state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign) {
                if (hvacSizingSimulationManager) hvacSizingSimulationManager->UpdateSizingLogsZoneStep(state);
            }
        }
        // There is no hourly reporting in the heat balance.

        // There is no daily reporting in the heat balance.

        // There is no simulation level record keeping in the heat balance.
    }

    //        End of Reporting subroutines for the HB Module

    void OpenShadingFile(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         X Luo
        //       DATE WRITTEN   August 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Open and set up headers for a external shading fraction export file.

        // Using/Aliasing
        using DataSurfaces::Surface;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;

        state.files.shade.ensure_open(state, "OpenOutputFiles", state.files.outputControl.extshd);
        print(state.files.shade, "Surface Name,");
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            print(state.files.shade, "{},", Surface(SurfNum).Name);
        }
        print(state.files.shade, "()\n");
    }
    void GetFrameAndDividerData(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   May 2000
        //       MODIFIED       April 2002 (FCW): get window reveal data
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets input data for window frame and/or divider and/or window
        // inside/outside reveal.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;                            // IO Status when calling get input subroutine
        Array1D_string FrameDividerNames(2);   // Frame/Divider Alpha names
        int FrameDividerNum;                   // Counter to keep track of the frame/divider number
        int FrameDividerNumAlpha;              // Number of frame/divider alpha names being passed
        int FrameDividerNumProp;               // Number of frame/divider properties being passed
        Array1D<Real64> FrameDividerProps(23); // Temporary array to transfer frame/divider properties
        int Loop;

        CurrentModuleObject = "WindowProperty:FrameAndDivider";
        TotFrameDivider = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        FrameDivider.allocate(TotFrameDivider);
        if (TotFrameDivider == 0) return;

        FrameDividerNum = 0;

        for (Loop = 1; Loop <= TotFrameDivider; ++Loop) {

            // Call Input Get routine to retrieve frame/divider data
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          FrameDividerNames,
                                          FrameDividerNumAlpha,
                                          FrameDividerProps,
                                          FrameDividerNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, FrameDividerNames(1), CurrentModuleObject, ErrorsFound)) continue;

            // Load the frame/divider derived type from the input data.
            ++FrameDividerNum;
            FrameDivider(FrameDividerNum).Name = FrameDividerNames(1);
            FrameDivider(FrameDividerNum).FrameWidth = FrameDividerProps(1);
            FrameDivider(FrameDividerNum).FrameProjectionOut = FrameDividerProps(2);
            FrameDivider(FrameDividerNum).FrameProjectionIn = FrameDividerProps(3);
            if (FrameDivider(FrameDividerNum).FrameWidth == 0.0) {
                FrameDivider(FrameDividerNum).FrameProjectionOut = 0.0;
                FrameDivider(FrameDividerNum).FrameProjectionIn = 0.0;
            }
            FrameDivider(FrameDividerNum).FrameConductance = FrameDividerProps(4);
            FrameDivider(FrameDividerNum).FrEdgeToCenterGlCondRatio = FrameDividerProps(5);
            FrameDivider(FrameDividerNum).FrameSolAbsorp = FrameDividerProps(6);
            FrameDivider(FrameDividerNum).FrameVisAbsorp = FrameDividerProps(7);
            FrameDivider(FrameDividerNum).FrameEmis = FrameDividerProps(8);
            if (UtilityRoutines::SameString(FrameDividerNames(2), "DividedLite")) {
                FrameDivider(FrameDividerNum).DividerType = DividedLite;
            } else if (UtilityRoutines::SameString(FrameDividerNames(2), "Suspended")) {
                FrameDivider(FrameDividerNum).DividerType = Suspended;
            } else {
                ShowWarningError(state, CurrentModuleObject + "=\"" + FrameDividerNames(1) + "\", Invalid " + cAlphaFieldNames(2));
                ShowContinueError(state, "Entered=\"" + FrameDividerNames(2) + "\", must be DividedLite or Suspended.  Will be set to DividedLite.");
                FrameDivider(FrameDividerNum).DividerType = DividedLite;
            }
            FrameDivider(FrameDividerNum).DividerWidth = FrameDividerProps(9);
            FrameDivider(FrameDividerNum).HorDividers = FrameDividerProps(10);
            FrameDivider(FrameDividerNum).VertDividers = FrameDividerProps(11);
            FrameDivider(FrameDividerNum).DividerProjectionOut = FrameDividerProps(12);
            FrameDivider(FrameDividerNum).DividerProjectionIn = FrameDividerProps(13);
            if (FrameDivider(FrameDividerNum).DividerWidth == 0.0 || FrameDivider(FrameDividerNum).DividerType == Suspended) {
                FrameDivider(FrameDividerNum).DividerProjectionOut = 0.0;
                FrameDivider(FrameDividerNum).DividerProjectionIn = 0.0;
            }
            FrameDivider(FrameDividerNum).DividerConductance = FrameDividerProps(14);
            FrameDivider(FrameDividerNum).DivEdgeToCenterGlCondRatio = FrameDividerProps(15);
            FrameDivider(FrameDividerNum).DividerSolAbsorp = FrameDividerProps(16);
            FrameDivider(FrameDividerNum).DividerVisAbsorp = FrameDividerProps(17);
            FrameDivider(FrameDividerNum).DividerEmis = FrameDividerProps(18);
            FrameDivider(FrameDividerNum).OutsideRevealSolAbs = FrameDividerProps(19);
            FrameDivider(FrameDividerNum).InsideSillDepth = FrameDividerProps(20);
            FrameDivider(FrameDividerNum).InsideSillSolAbs = FrameDividerProps(21);
            FrameDivider(FrameDividerNum).InsideReveal = FrameDividerProps(22);
            FrameDivider(FrameDividerNum).InsideRevealSolAbs = FrameDividerProps(23);

            if (FrameDivider(FrameDividerNum).DividerWidth > 0.0 &&
                (FrameDivider(FrameDividerNum).HorDividers == 0 && FrameDivider(FrameDividerNum).VertDividers == 0)) {
                ShowWarningError(state,
                                 CurrentModuleObject + ": In FrameAndDivider " + FrameDivider(FrameDividerNum).Name + ' ' + cNumericFieldNames(9) +
                                     " > 0 ");
                ShowContinueError(state, "...but " + cNumericFieldNames(10) + " = 0 and " + cNumericFieldNames(11) + " = 0.");
                ShowContinueError(state, "..." + cNumericFieldNames(9) + " set to 0.");
                FrameDivider(FrameDividerNum).DividerWidth = 0.0;
            }
            // Prevent InsideSillDepth < InsideReveal
            if (FrameDivider(FrameDividerNum).InsideSillDepth < FrameDivider(FrameDividerNum).InsideReveal) {
                ShowWarningError(state,
                                 CurrentModuleObject + ": In FrameAndDivider " + FrameDivider(FrameDividerNum).Name + ' ' + cNumericFieldNames(20) +
                                     " is less than " + cNumericFieldNames(22) + "; it will be set to " + cNumericFieldNames(22) + '.');
                FrameDivider(FrameDividerNum).InsideSillDepth = FrameDivider(FrameDividerNum).InsideReveal;
            }

            //    ! Warn if InsideSillDepth OR InsideReveal > 0.2meters to warn of inaccuracies
            //    IF(FrameDivider(FrameDividerNum)%InsideSillDepth > 0.2d0) THEN
            //      CALL ShowWarningError(state, TRIM(CurrentModuleObject)//': In FrameAndDivider '//TRIM(FrameDivider(FrameDividerNum)%Name)// &
            //        ' '//TRIM(cNumericFieldNames(20))//' is greater than 0.2 meters, which could cause inaccuracies in zone cooling energy.')
            //    END IF
            //    IF(FrameDivider(FrameDividerNum)%InsideReveal > 0.2d0) THEN
            //      CALL ShowWarningError(state, TRIM(CurrentModuleObject)//': In FrameAndDivider '//TRIM(FrameDivider(FrameDividerNum)%Name)// &
            //        ' '//TRIM(cNumericFieldNames(22))//' is greater than 0.2 meters, which could cause inaccuracies in zone cooling energy.')
            //    END IF
        }
    }

    void SearchWindow5DataFile(EnergyPlusData &state,
                               std::string const &DesiredFileName,         // File name that contains the Window5 constructions.
                               std::string const &DesiredConstructionName, // Name that will be searched for in the Window5 data file
                               bool &ConstructionFound,                    // True if DesiredConstructionName is in the Window5 data file
                               bool &EOFonFile,                            // True if EOF during file read
                               bool &ErrorsFound                           // True if there is a problem with the entry requested from the data file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   August 2001
        //       MODIFIED       June 2002, FW: do all reallocation here for constructions found on
        //                        data file; 1 new construction of entry has one glazing system;
        //                        2 new constructions if entry has two glazing systems.
        //                      Nov 2002, FW: skip read of mullion data line if one glazing system;
        //                        add error messages for bad data; increase length of input line
        //                        from 132 to 200 to handle case where Window5 puts in extra blanks
        //                        in gas data line.
        //                      Feb 2007, LKL: Add more checks on Window5DataFile
        //                      Jan 2008, LKL: Change Edge/Cond ratio check.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Searches the WINDOW5 data file for a window with the name "DesiredConstructionName,"
        // which is the name of an idf Construction input using CONSTRUCTION FROM WINDOW5 DATA FILE.
        // (The WINDOW5 data file contains data for one or more complete windows --
        // glazing, frame, mullion, and divider.
        // WINDOW5 writes the data file for export to EnergyPlus so that an annual energy
        // analysis can be done on exactly the same window without having to re-input into
        // EnergyPlus.)

        // If a match is found, a Construction is created and the Material objects associated with
        // the Construction are created. If there is an associated frame or
        // divider in the Window5 data file for this Construction, a FrameAndDivider object will
        // also be created.

        // If the window on the data file has two glazing systems, a second Construction (and its
        // associated materials) corresponding to the second glazing system is created.

        // Using/Aliasing
        using namespace DataStringGlobals;
        using DataSystemVariables::CheckForActualFileName;
        using DataSystemVariables::iUnicode_end;
        using General::POLYF; // POLYF       ! Polynomial in cosine of angle of incidence

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const NumName(5, {"1", "2", "3", "4", "5"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FileLineCount;            // counter for number of lines read (used in some error messages)
        Array1D_string DataLine(100); // Array of data lines
        std::string WindowNameInW5DataFile;
        std::string W5Name;
        Array1D_string GasName(3);      // Gas name from data file
        std::string LayerName;          // Layer name from data file
        std::string MullionOrientation; // Horizontal, vertical or none
        int LineNum;
        Array1D_int NGlass(2);              // Number of glass layers in glazing system
        Array2D_int NumGases(4, 2);         // Number of gases in each gap of a glazing system
        Array2D_int MaterNumSysGlass(5, 2); // Material numbers for glazing system / glass combinations
        Array2D_int MaterNumSysGap(4, 2);   // Material numbers for glazing system / gap combinations
        int TotMaterialsPrev;               // Number of materials before adding ones from W5DataFile
        int TotFrameDividerPrev;            // Number of FrameAndDivider objects before adding ones from W5DataFile
        Array1D_int NGaps(2);               // Number of gaps in window construction
        int NGlSys;                         // Number of glazing systems (normally 1, but 2 for mullioned window
        //  with two different glazing systems
        int loop;      // DO loop counter
        int ILine;     // Line counter
        int ConstrNum; // Construction number
        int IGlass;    // Glass layer counter
        int IGap;      // Gap counter
        int IGas;      // Gas counter
        //  INTEGER            :: ICoeff              ! Gas property coefficient counter
        int IGlSys;   // Glazing system counter
        int MaterNum; // Material number
        int MatNum;
        int FrDivNum;                 // FrameDivider number
        bool exists;                  // True if Window5 data file exists
        Array1D<Real64> WinHeight(2); // Height, width for glazing system (m)
        Array1D<Real64> WinWidth(2);
        Array1D<Real64> UValCenter(2);      // Center of glass U-value (W/m2-K) for glazing system
        Array1D<Real64> SCCenter(2);        // Center of glass shading coefficient for glazing system
        Array1D<Real64> SHGCCenter(2);      // Center of glass solar heat gain coefficient for glazing system
        Array1D<Real64> TVisCenter(2);      // Center of glass visible transmittance for glazing system
        Array1D<Real64> Tsol(11);           // Solar transmittance vs incidence angle; diffuse trans.
        Array2D<Real64> AbsSol(11, 5);      // Solar absorptance vs inc. angle in each glass layer
        Array1D<Real64> Rfsol(11);          // Front solar reflectance vs inc. angle
        Array1D<Real64> Rbsol(11);          // Back solar reflectance vs inc. angle
        Array1D<Real64> Tvis(11);           // Visible transmittance vs inc. angle
        Array1D<Real64> Rfvis(11);          // Front visible reflectance vs inc. angle
        Array1D<Real64> Rbvis(11);          // Back visible reflectance vs inc. angle
        Array1D<Real64> CosPhiIndepVar(10); // Cosine of incidence angle from 0 to 90 deg in 10 deg increments
        int IPhi;                           // Incidence angle counter
        Real64 Phi;                         // Incidence angle (deg)
        Array1D<Real64> CosPhi(10);         // Cosine of incidence angle
        Array1D<Real64> tsolFit(10);        // Fitted solar transmittance vs incidence angle
        Array1D<Real64> tvisFit(10);        // Fitted visible transmittance vs incidence angle
        Array1D<Real64> rfsolFit(10);       // Fitted solar front reflectance vs incidence angle
        Array2D<Real64> solabsFit(5, 10);   // Fitted solar absorptance vs incidence angle for each glass layer
        Array1D_string DividerType(2);      // Divider type: DividedLite or Suspended
        Real64 FrameWidth;
        Real64 MullionWidth;
        Real64 FrameProjectionOut;
        Real64 FrameProjectionIn;
        Real64 FrameConductance;
        Real64 FrEdgeToCenterGlCondRatio;
        Real64 FrameSolAbsorp;
        Real64 FrameVisAbsorp;
        Real64 FrameEmis;
        Array1D_int HorDividers(2);  // For divider: number horizontal for each glazing system
        Array1D_int VertDividers(2); // For divider: number vertical for each glazing system
        Array1D<Real64> DividerWidth(2);
        Array1D<Real64> DividerProjectionOut(2);
        Array1D<Real64> DividerProjectionIn(2);
        Array1D<Real64> DividerConductance(2);
        Array1D<Real64> DivEdgeToCenterGlCondRatio(2);
        Array1D<Real64> DividerSolAbsorp(2);
        Array1D<Real64> DividerVisAbsorp(2);
        Array1D<Real64> DividerEmis(2);
        std::string::size_type endcol;

        // Object Data

        // In the following four gas-related data sets, the first
        //  index is gas type (1=air, 2=Argon, 3=Krypton, 4=Xenon)
        //  and the second index gives a,b,c in the expression
        //  property value = a + bT(K) + cT(K)**2, where T is mean
        //  gap temperature in deg K.

        ConstructionFound = false;
        // ErrorsFound = .FALSE.
        EOFonFile = false;
        std::string contextString = "HeatBalanceManager::SearchWindow5DataFile: ";

        CheckForActualFileName(state, DesiredFileName, exists, state.files.TempFullFileName.fileName, contextString);

        // INQUIRE(FILE=TRIM(DesiredFileName), EXIST=exists)
        if (!exists) {
            ShowFatalError(state, "Program terminates due to these conditions.");
        }

        auto W5DataFile = state.files.TempFullFileName.open(state, "SearchWindow5DataFile");
        auto NextLine = W5DataFile.readLine();
        endcol = len(NextLine.data);
        if (endcol > 0) {
            if (int(NextLine.data[endcol - 1]) == iUnicode_end) {
                ShowSevereError(state,
                                "SearchWindow5DataFile: For \"" + DesiredConstructionName + "\" in " + DesiredFileName +
                                    " fiile, appears to be a Unicode or binary file.");
                ShowContinueError(state, "...This file cannot be read by this program. Please save as PC or Unix file and try again");
                ShowFatalError(state, "Program terminates due to previous condition.");
            }
        }
        W5DataFile.rewind();
        FileLineCount = 0;

        NextLine = W5DataFile.readLine();
        if (NextLine.eof) goto Label1000;
        ++FileLineCount;
        if (!has_prefixi(NextLine.data, "WINDOW5")) {
            ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Data File=" + DesiredFileName);
            ShowFatalError(
                state, "Error reading Window5 Data File: first word of window entry is \"" + NextLine.data.substr(0, 7) + "\", should be Window5.");
        }

    Label10:;
        for (LineNum = 2; LineNum <= 5; ++LineNum) {
            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            DataLine(LineNum) = NextLine.data;
            ++FileLineCount;
        }

        // Get window name and check for match
        readItem(DataLine(4).substr(19), W5Name);
        WindowNameInW5DataFile = UtilityRoutines::MakeUPPERCase(W5Name);
        if (DesiredConstructionName != WindowNameInW5DataFile) {
            // Doesn't match; read through file until next window entry is found
        Label20:;
            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            if (!has_prefixi(NextLine.data, "WINDOW5")) goto Label20;
            // Beginning of next window entry found
            goto Label10;
        } else {
            // Match found
            ConstructionFound = true;

            // Create Material:WindowGlass, Material:WindowGas, Construction
            // and WindowFrameAndDividerObjects for this window

            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            readItem(NextLine.data.substr(19), NGlSys);
            if (NGlSys <= 0 || NGlSys > 2) {
                ShowFatalError(
                    state,
                    format("Construction={} from the Window5 data file cannot be used: it has {} glazing systems; only 1 or 2 are allowed.",
                           DesiredConstructionName,
                           NGlSys));
            }
            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                NextLine = W5DataFile.readLine();
                if (NextLine.eof) goto Label1000;
                ++FileLineCount;

                const auto succeeded = readList(NextLine.data.substr(19),
                                                WinHeight(IGlSys),
                                                WinWidth(IGlSys),
                                                NGlass(IGlSys),
                                                UValCenter(IGlSys),
                                                SCCenter(IGlSys),
                                                SHGCCenter(IGlSys),
                                                TVisCenter(IGlSys));
                if (!succeeded) {
                    ShowSevereError(
                        state,
                        format("HeatBalanceManager: SearchWindow5DataFile: Error in Read of glazing system values. For glazing system={}", IGlSys));
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount, NextLine.data.substr(0, 100)));
                    ErrorsFound = true;
                }
                if (WinHeight(IGlSys) == 0.0 || WinWidth(IGlSys) == 0.0) {
                    ShowSevereError(state,
                                    format("HeatBalanceManager: SearchWindow5DataFile: Construction={} from the Window5 data file cannot be used: it "
                                           "has window height or width = 0 for glazing system {}",
                                           DesiredConstructionName,
                                           IGlSys));
                    ErrorsFound = true;
                }
                if (NGlass(IGlSys) <= 0 || NGlass(IGlSys) > 4) {
                    ShowSevereError(state,
                                    format("HeatBalanceManager: SearchWindow5DataFile: Construction={} from the Window5 data file cannot be used: it "
                                           "has 0 or more than 4 glass layers in glazing system {}",
                                           DesiredConstructionName,
                                           IGlSys));
                    ErrorsFound = true;
                }
                if (UValCenter(IGlSys) <= 0.0) {
                    ShowSevereError(state,
                                    format("HeatBalanceManager: SearchWindow5DataFile: Construction={} from the Window5 data file cannot be used: it "
                                           "has Center-of-Glass U-value <= 0 in glazing system {}",
                                           DesiredConstructionName,
                                           IGlSys));
                    ErrorsFound = true;
                }
                if (SCCenter(IGlSys) <= 0.0) {
                    ShowSevereError(state,
                                    format("HeatBalanceManager: SearchWindow5DataFile: Construction={} from the Window5 data file cannot be used: it "
                                           "has Shading Coefficient <= 0 in glazing system {}",
                                           DesiredConstructionName,
                                           IGlSys));
                    ErrorsFound = true;
                }
                if (SHGCCenter(IGlSys) <= 0.0) {
                    ShowSevereError(state,
                                    format("HeatBalanceManager: SearchWindow5DataFile: Construction={} from the Window5 data file cannot be used: it "
                                           "has SHGC <= 0 in glazing system {}",
                                           DesiredConstructionName,
                                           IGlSys));
                    ErrorsFound = true;
                }
                WinHeight(IGlSys) *= 0.001;
                WinWidth(IGlSys) *= 0.001;
            }
            for (LineNum = 1; LineNum <= 11; ++LineNum) {
                NextLine = W5DataFile.readLine();
                if (NextLine.eof) goto Label1000;
                DataLine(LineNum) = NextLine.data;
            }

            // Mullion width and orientation
            MullionWidth = 0.0;
            MullionOrientation = "Vertical";
            if (NGlSys == 2) {
                if (!readItem(DataLine(10).substr(19), MullionWidth)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Mullion Width.");
                    ShowContinueError(state,
                                      format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 10, DataLine(10).substr(0, 100)));
                    ErrorsFound = true;
                }
                MullionWidth *= 0.001;
                if (!readItem(DataLine(10).substr(88), MullionOrientation)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Mullion Orientation.");
                    ShowContinueError(state,
                                      format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 10, DataLine(10).substr(0, 100)));
                    ErrorsFound = true;
                }
            }

            // Frame data; if there are two glazing systems, the frame is assumed to be
            // the same for both.
            FrameWidth = 0.0;
            FrameProjectionOut = 0.0;
            FrameProjectionIn = 0.0;
            FrameConductance = 0.0;
            FrEdgeToCenterGlCondRatio = 0.0;
            FrameSolAbsorp = 0.0;
            FrameVisAbsorp = 0.0;
            FrameEmis = 0.0;
            const bool succeeded = readList(DataLine(11).substr(19),
                                            FrameWidth,
                                            FrameProjectionOut,
                                            FrameProjectionIn,
                                            FrameConductance,
                                            FrEdgeToCenterGlCondRatio,
                                            FrameSolAbsorp,
                                            FrameVisAbsorp,
                                            FrameEmis);
            if (!succeeded) {
                ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of frame data values.");
                ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 11, DataLine(11).substr(0, 100)));
                ErrorsFound = true;
            }
            if (FrameWidth > 0.0) {
                if (FrameConductance <= 0.0) {
                    ShowSevereError(state,
                                    "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                        " from the Window5 data file cannot be used: it has Frame Conductance <= 0.0");
                    ErrorsFound = true;
                }
                // Relax this check for Window5 data: 1/28/2008.
                //        IF(FrEdgeToCenterGlCondRatio < 1.0) THEN
                //            CALL ShowSevereError(state, 'HeatBalanceManager: SearchWindow5DataFile: Construction='//TRIM(DesiredConstructionName)//
                //            & ' from the Window5 data file cannot be used: it has Frame Edge-of-Glass Conduction Ratio < 1.0')
                //          ErrorsFound = .TRUE.
                //        END IF
                if (FrameSolAbsorp < 0.0 || FrameSolAbsorp > 1.0) {
                    ShowSevereError(state,
                                    "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                        " from the Window5 data file cannot be used: it has Frame Solar Absorptance < 0.0 or > 1.0");
                    ErrorsFound = true;
                }
                if (FrameEmis <= 0.0 || FrameEmis >= 1.0) {
                    ShowSevereError(state,
                                    "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                        " from the Window5 data file cannot be used: it has Frame Emissivity <= 0.0 or >= 1.0");
                    ErrorsFound = true;
                }
            }
            FrameWidth *= 0.001;
            FrameProjectionOut *= 0.001;
            FrameProjectionIn *= 0.001;
            FileLineCount += 11;

            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;

            // Divider data for each glazing system
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                NextLine = W5DataFile.readLine();
                if (NextLine.eof) goto Label1000;
                ++FileLineCount;

                const bool dividerReadSucceeded = readList(NextLine.data.substr(19),
                                                           DividerWidth(IGlSys),
                                                           DividerProjectionOut(IGlSys),
                                                           DividerProjectionIn(IGlSys),
                                                           DividerConductance(IGlSys),
                                                           DivEdgeToCenterGlCondRatio(IGlSys),
                                                           DividerSolAbsorp(IGlSys),
                                                           DividerVisAbsorp(IGlSys),
                                                           DividerEmis(IGlSys),
                                                           DividerType(IGlSys),
                                                           HorDividers(IGlSys),
                                                           VertDividers(IGlSys));
                if (!dividerReadSucceeded) {
                    ShowSevereError(
                        state,
                        format("HeatBalanceManager: SearchWindow5DataFile: Error in Read of divider data values. For Glazing System={}", IGlSys));
                    ShowContinueError(state,
                                      format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 11, NextLine.data.substr(0, 100)));
                    ErrorsFound = true;
                }
                uppercase(DividerType(IGlSys));
                if (DividerWidth(IGlSys) > 0.0) {
                    if (HorDividers(IGlSys) == 0 && VertDividers(IGlSys) == 0) {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(
                            state, format("glazing system {} has a divider but number of horizontal and vertical divider elements = 0", IGlSys));
                        ErrorsFound = true;
                    }
                    if (DividerConductance(IGlSys) <= 0.0) {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(state, format("glazing system {} has Divider Conductance <= 0.0", IGlSys));
                        ErrorsFound = true;
                    }
                    if (DivEdgeToCenterGlCondRatio(IGlSys) < 1.0) {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(state, format("glazing system {} has Divider Edge-Of-Glass Conduction Ratio < 1.0", IGlSys));
                        ErrorsFound = true;
                    }
                    if (DividerSolAbsorp(IGlSys) < 0.0 || DividerSolAbsorp(IGlSys) > 1.0) {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(state, format("glazing system {} has Divider Solar Absorptance < 0.0 or > 1.0", IGlSys));
                        ErrorsFound = true;
                    }
                    if (DividerEmis(IGlSys) <= 0.0 || DividerEmis(IGlSys) >= 1.0) {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(state, format("glazing system {} has Divider Emissivity <= 0.0 or >= 1.0", IGlSys));
                        ErrorsFound = true;
                    }
                    if (DividerType(IGlSys) != "DIVIDEDLITE" && DividerType(IGlSys) != "SUSPENDED") {
                        ShowSevereError(state,
                                        "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                            " from the Window5 data file cannot be used:");
                        ShowContinueError(
                            state,
                            format("glazing system {} has Divider Type = {}; it should be DIVIDEDLITE or SUSPENDED.", IGlSys, DividerType(IGlSys)));
                        ErrorsFound = true;
                    }
                }
                DividerWidth(IGlSys) *= 0.001;
                if (DividerType(IGlSys) == "DIVIDEDLITE") {
                    DividerProjectionOut(IGlSys) *= 0.001;
                    DividerProjectionIn(IGlSys) *= 0.001;
                } else {
                    DividerProjectionOut(IGlSys) = 0.0;
                    DividerProjectionIn(IGlSys) = 0.0;
                }
            }

            if (ErrorsFound)
                ShowFatalError(state,
                               "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                   " from the Window5 data file cannot be used because of above errors");

            TotMaterialsPrev = TotMaterials;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                NGaps(IGlSys) = NGlass(IGlSys) - 1;
                TotMaterials += NGlass(IGlSys) + NGaps(IGlSys);
            }

            // Create Material objects

            // reallocate Material type

            state.dataMaterial->Material.redimension(TotMaterials);
            NominalR.redimension(TotMaterials, 0.0);

            // Initialize new materials
            for (loop = TotMaterialsPrev + 1; loop <= TotMaterials; ++loop) {
                state.dataMaterial->Material(loop).Name = "";
                state.dataMaterial->Material(loop).Group = -1;
                state.dataMaterial->Material(loop).Roughness = 0;
                state.dataMaterial->Material(loop).Conductivity = 0.0;
                state.dataMaterial->Material(loop).Density = 0.0;
                state.dataMaterial->Material(loop).IsoMoistCap = 0.0;
                state.dataMaterial->Material(loop).Porosity = 0.0;
                state.dataMaterial->Material(loop).Resistance = 0.0;
                state.dataMaterial->Material(loop).SpecHeat = 0.0;
                state.dataMaterial->Material(loop).ThermGradCoef = 0.0;
                state.dataMaterial->Material(loop).Thickness = 0.0;
                state.dataMaterial->Material(loop).VaporDiffus = 0.0;
                state.dataMaterial->Material(loop).AbsorpSolar = 0.0;
                state.dataMaterial->Material(loop).AbsorpThermal = 0.0;
                state.dataMaterial->Material(loop).AbsorpVisible = 0.0;
                state.dataMaterial->Material(loop).ReflectShade = 0.0;
                state.dataMaterial->Material(loop).Trans = 0.0;
                state.dataMaterial->Material(loop).ReflectShadeVis = 0.0;
                state.dataMaterial->Material(loop).TransVis = 0.0;
                state.dataMaterial->Material(loop).GlassTransDirtFactor = 1.0;
                state.dataMaterial->Material(loop).SolarDiffusing = false;
                state.dataMaterial->Material(loop).AbsorpThermalBack = 0.0;
                state.dataMaterial->Material(loop).AbsorpThermalFront = 0.0;
                state.dataMaterial->Material(loop).ReflectSolBeamBack = 0.0;
                state.dataMaterial->Material(loop).ReflectSolBeamFront = 0.0;
                state.dataMaterial->Material(loop).ReflectSolDiffBack = 0.0;
                state.dataMaterial->Material(loop).ReflectSolDiffFront = 0.0;
                state.dataMaterial->Material(loop).ReflectVisBeamBack = 0.0;
                state.dataMaterial->Material(loop).ReflectVisBeamFront = 0.0;
                state.dataMaterial->Material(loop).ReflectVisDiffBack = 0.0;
                state.dataMaterial->Material(loop).ReflectVisDiffFront = 0.0;
                state.dataMaterial->Material(loop).TransSolBeam = 0.0;
                state.dataMaterial->Material(loop).TransThermal = 0.0;
                state.dataMaterial->Material(loop).TransVisBeam = 0.0;
                state.dataMaterial->Material(loop).GlassSpectralDataPtr = 0;
                state.dataMaterial->Material(loop).NumberOfGasesInMixture = 0;
                state.dataMaterial->Material(loop).GasCon = 0.0;
                state.dataMaterial->Material(loop).GasVis = 0.0;
                state.dataMaterial->Material(loop).GasCp = 0.0;
                state.dataMaterial->Material(loop).GasType = 0;
                state.dataMaterial->Material(loop).GasWght = 0.0;
                state.dataMaterial->Material(loop).GasSpecHeatRatio = 0.0;
                state.dataMaterial->Material(loop).GasFract = 0.0;
                state.dataMaterial->Material(loop).WinShadeToGlassDist = 0.0;
                state.dataMaterial->Material(loop).WinShadeTopOpeningMult = 0.0;
                state.dataMaterial->Material(loop).WinShadeBottomOpeningMult = 0.0;
                state.dataMaterial->Material(loop).WinShadeLeftOpeningMult = 0.0;
                state.dataMaterial->Material(loop).WinShadeRightOpeningMult = 0.0;
                state.dataMaterial->Material(loop).WinShadeAirFlowPermeability = 0.0;
                state.dataMaterial->Material(loop).BlindDataPtr = 0;
                state.dataMaterial->Material(loop).EMPDmu = 0.0;
                state.dataMaterial->Material(loop).MoistACoeff = 0.0;
                state.dataMaterial->Material(loop).MoistBCoeff = 0.0;
                state.dataMaterial->Material(loop).MoistCCoeff = 0.0;
                state.dataMaterial->Material(loop).MoistDCoeff = 0.0;
                state.dataMaterial->Material(loop).EMPDSurfaceDepth = 0.0;
                state.dataMaterial->Material(loop).EMPDDeepDepth = 0.0;
                state.dataMaterial->Material(loop).EMPDmuCoating = 0.0;
                state.dataMaterial->Material(loop).EMPDCoatingThickness = 0.0;
            }

            // Glass objects
            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            MaterNum = TotMaterialsPrev;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                for (IGlass = 1; IGlass <= NGlass(IGlSys); ++IGlass) {
                    ++MaterNum;
                    MaterNumSysGlass(IGlass, IGlSys) = MaterNum;
                    state.dataMaterial->Material(MaterNum).Group = WindowGlass;
                    NextLine = W5DataFile.readLine();
                    ++FileLineCount;

                    readList(NextLine.data.substr(25),
                             state.dataMaterial->Material(MaterNum).Thickness,
                             state.dataMaterial->Material(MaterNum).Conductivity,
                             state.dataMaterial->Material(MaterNum).Trans,
                             state.dataMaterial->Material(MaterNum).ReflectSolBeamFront,
                             state.dataMaterial->Material(MaterNum).ReflectSolBeamBack,
                             state.dataMaterial->Material(MaterNum).TransVis,
                             state.dataMaterial->Material(MaterNum).ReflectVisBeamFront,
                             state.dataMaterial->Material(MaterNum).ReflectVisBeamBack,
                             state.dataMaterial->Material(MaterNum).TransThermal,
                             state.dataMaterial->Material(MaterNum).AbsorpThermalFront,
                             state.dataMaterial->Material(MaterNum).AbsorpThermalBack,
                             LayerName);

                    state.dataMaterial->Material(MaterNum).Thickness *= 0.001;
                    if (state.dataMaterial->Material(MaterNum).Thickness <= 0.0) {
                    }
                    if (NGlSys == 1) {
                        state.dataMaterial->Material(MaterNum).Name = "W5:" + DesiredConstructionName + ":GLASS" + NumName(IGlass);
                    } else {
                        state.dataMaterial->Material(MaterNum).Name =
                            "W5:" + DesiredConstructionName + ':' + NumName(IGlSys) + ":GLASS" + NumName(IGlass);
                    }
                    state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;
                    state.dataMaterial->Material(MaterNum).AbsorpThermal = state.dataMaterial->Material(MaterNum).AbsorpThermalBack;
                    if (state.dataMaterial->Material(MaterNum).Thickness <= 0.0) {
                        ShowSevereError(state,
                                        "SearchWindow5DataFile: Material=\"" + state.dataMaterial->Material(MaterNum).Name +
                                            "\" has thickness of 0.0.  Will be set to thickness = .001 but inaccuracies may result.");
                        ShowContinueError(state, "Line being read=" + NextLine.data);
                        ShowContinueError(state, "Thickness field starts at column 26=" + NextLine.data.substr(25));
                        state.dataMaterial->Material(MaterNum).Thickness = 0.001;
                    }
                }
            }

            // Gap objects
            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                for (IGap = 1; IGap <= NGaps(IGlSys); ++IGap) {
                    ++MaterNum;
                    MaterNumSysGap(IGap, IGlSys) = MaterNum;
                    NextLine = W5DataFile.readLine();
                    ++FileLineCount;
                    readList(NextLine.data.substr(23), state.dataMaterial->Material(MaterNum).Thickness, NumGases(IGap, IGlSys));
                    if (NGlSys == 1) {
                        state.dataMaterial->Material(MaterNum).Name = "W5:" + DesiredConstructionName + ":GAP" + NumName(IGap);
                    } else {
                        state.dataMaterial->Material(MaterNum).Name =
                            "W5:" + DesiredConstructionName + ':' + NumName(IGlSys) + ":GAP" + NumName(IGap);
                    }
                    state.dataMaterial->Material(MaterNum).Thickness *= 0.001;
                    state.dataMaterial->Material(MaterNum).Roughness = MediumRough; // Unused
                }
            }

            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                for (IGap = 1; IGap <= NGaps(IGlSys); ++IGap) {
                    MaterNum = MaterNumSysGap(IGap, IGlSys);
                    state.dataMaterial->Material(MaterNum).NumberOfGasesInMixture = NumGases(IGap, IGlSys);
                    state.dataMaterial->Material(MaterNum).Group = WindowGas;
                    if (NumGases(IGap, IGlSys) > 1) state.dataMaterial->Material(MaterNum).Group = WindowGasMixture;
                    for (IGas = 1; IGas <= NumGases(IGap, IGlSys); ++IGas) {
                        NextLine = W5DataFile.readLine();
                        ++FileLineCount;
                        readList(NextLine.data.substr(19),
                                 GasName(IGas),
                                 state.dataMaterial->Material(MaterNum).GasFract(IGas),
                                 state.dataMaterial->Material(MaterNum).GasWght(IGas),
                                 state.dataMaterial->Material(MaterNum).GasCon(_, IGas),
                                 state.dataMaterial->Material(MaterNum).GasVis(_, IGas),
                                 state.dataMaterial->Material(MaterNum).GasCp(_, IGas));
                        // Nominal resistance of gap at room temperature (based on first gas in mixture)
                        NominalR(MaterNum) =
                            state.dataMaterial->Material(MaterNum).Thickness /
                            (state.dataMaterial->Material(MaterNum).GasCon(1, 1) + state.dataMaterial->Material(MaterNum).GasCon(2, 1) * 300.0 +
                             state.dataMaterial->Material(MaterNum).GasCon(3, 1) * 90000.0);
                    }
                }
            }

            // Construction objects

            // reallocate Construct types
            TotConstructs += NGlSys;
            state.dataConstruction->Construct.redimension(TotConstructs);
            NominalRforNominalUCalculation.redimension(TotConstructs);
            NominalU.redimension(TotConstructs);

            NextLine = W5DataFile.readLine();
            if (NextLine.eof) goto Label1000;
            ++FileLineCount;

            // Pre-calculate constants
            for (IPhi = 1; IPhi <= 10; ++IPhi) {
                CosPhiIndepVar(IPhi) = std::cos((IPhi - 1) * 10.0 * DataGlobalConstants::DegToRadians);
            }

            // Pre-calculate constants
            for (IPhi = 1; IPhi <= 10; ++IPhi) {
                Phi = double(IPhi - 1) * 10.0;
                CosPhi(IPhi) = std::cos(Phi * DataGlobalConstants::DegToRadians);
                if (std::abs(CosPhi(IPhi)) < 0.0001) CosPhi(IPhi) = 0.0;
            }

            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                ConstrNum = TotConstructs - NGlSys + IGlSys;
                if (IGlSys == 1) {
                    state.dataConstruction->Construct(ConstrNum).Name = DesiredConstructionName;
                } else {
                    state.dataConstruction->Construct(ConstrNum).Name = DesiredConstructionName + ":2";
                }
                for (loop = 1; loop <= Construction::MaxLayersInConstruct; ++loop) {
                    state.dataConstruction->Construct(ConstrNum).LayerPoint(loop) = 0;
                }
                state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar = 0.0;
                state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar = 0.0;
                state.dataConstruction->Construct(ConstrNum).DayltPropPtr = 0;
                state.dataConstruction->Construct(ConstrNum).CTFCross = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFFlux = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFInside = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFOutside = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFSourceIn = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFSourceOut = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTimeStep = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTSourceOut = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTSourceIn = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTSourceQ = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTUserOut = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTUserIn = 0.0;
                state.dataConstruction->Construct(ConstrNum).CTFTUserSource = 0.0;
                state.dataConstruction->Construct(ConstrNum).NumHistories = 0;
                state.dataConstruction->Construct(ConstrNum).NumCTFTerms = 0;
                state.dataConstruction->Construct(ConstrNum).UValue = 0.0;
                state.dataConstruction->Construct(ConstrNum).SourceSinkPresent = false;
                state.dataConstruction->Construct(ConstrNum).SolutionDimensions = 0;
                state.dataConstruction->Construct(ConstrNum).SourceAfterLayer = 0;
                state.dataConstruction->Construct(ConstrNum).TempAfterLayer = 0;
                state.dataConstruction->Construct(ConstrNum).ThicknessPerpend = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiff = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiffBack = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiffShade = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade = 0.0;
                state.dataConstruction->Construct(ConstrNum).ShadeAbsorpThermal = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsBeamCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsBeamBackCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsBeamShadeCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiffIn = 0.0;
                state.dataConstruction->Construct(ConstrNum).AbsDiffOut = 0.0;
                state.dataConstruction->Construct(ConstrNum).TransDiff = 0.0;
                state.dataConstruction->Construct(ConstrNum).TransDiffVis = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront = 0.0;
                state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).ReflSolBeamBackCoef = 0.0;
                state.dataConstruction->Construct(ConstrNum).W5FrameDivider = 0;
                state.dataConstruction->Construct(ConstrNum).TotLayers = NGlass(IGlSys) + NGaps(IGlSys);
                state.dataConstruction->Construct(ConstrNum).TotGlassLayers = NGlass(IGlSys);
                state.dataConstruction->Construct(ConstrNum).TotSolidLayers = NGlass(IGlSys);

                for (IGlass = 1; IGlass <= NGlass(IGlSys); ++IGlass) {
                    state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * IGlass - 1) = MaterNumSysGlass(IGlass, IGlSys);
                    if (IGlass < NGlass(IGlSys)) state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * IGlass) = MaterNumSysGap(IGlass, IGlSys);
                }

                state.dataConstruction->Construct(ConstrNum).OutsideRoughness = VerySmooth;
                state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal =
                    state.dataMaterial->Material(TotMaterialsPrev + NGlass(IGlSys)).AbsorpThermalBack;
                state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
                    state.dataMaterial->Material(TotMaterialsPrev + 1).AbsorpThermalFront;
                state.dataConstruction->Construct(ConstrNum).TypeIsWindow = true;
                state.dataConstruction->Construct(ConstrNum).FromWindow5DataFile = true;
                state.dataConstruction->Construct(ConstrNum).W5FileGlazingSysHeight = WinHeight(IGlSys);
                state.dataConstruction->Construct(ConstrNum).W5FileGlazingSysWidth = WinWidth(IGlSys);
                if (UtilityRoutines::SameString(MullionOrientation, "Vertical")) {
                    state.dataConstruction->Construct(ConstrNum).W5FileMullionOrientation = Vertical;
                } else if (UtilityRoutines::SameString(MullionOrientation, "Horizontal")) {
                    state.dataConstruction->Construct(ConstrNum).W5FileMullionOrientation = Horizontal;
                } else {
                }
                state.dataConstruction->Construct(ConstrNum).W5FileMullionWidth = MullionWidth;

                // Fill Construct with system transmission, reflection and absorption properties

                NextLine = W5DataFile.readLine();
                if (NextLine.eof) goto Label1000;
                ++FileLineCount;
                if (IGlSys == 1) {
                    NextLine = W5DataFile.readLine();
                    if (NextLine.eof) goto Label1000;
                    ++FileLineCount;
                }
                NextLine = W5DataFile.readLine();
                if (NextLine.eof) goto Label1000;
                ++FileLineCount;
                if (!readItem(NextLine.data.substr(5), Tsol)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of TSol values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount, NextLine.data.substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Tsol, 0.0) || any_gt(Tsol, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of TSol values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount, NextLine.data.substr(0, 100)));
                    ErrorsFound = true;
                }
                for (IGlass = 1; IGlass <= NGlass(IGlSys); ++IGlass) {
                    NextLine = W5DataFile.readLine();
                    ++FileLineCount;
                    if (!readItem(NextLine.data.substr(5), AbsSol(_, IGlass))) {
                        ShowSevereError(state,
                                        format("HeatBalanceManager: SearchWindow5DataFile: Error in Read of AbsSol values. For Glass={}", IGlass));
                        ShowContinueError(state,
                                          format("Line (~{}) in error (first 100 characters)={}", FileLineCount, NextLine.data.substr(0, 100)));
                        ErrorsFound = true;
                    } else if (any_lt(AbsSol(_, IGlass), 0.0) || any_gt(AbsSol(_, IGlass), 1.0)) {
                        ShowSevereError(
                            state,
                            format("HeatBalanceManager: SearchWindow5DataFile: Error in Read of AbsSol values. (out of range [0,1]) For Glass={}",
                                   IGlass));
                        ShowContinueError(state,
                                          format("Line (~{}) in error (first 100 characters)={}", FileLineCount, NextLine.data.substr(0, 100)));
                        ErrorsFound = true;
                    }
                }
                for (ILine = 1; ILine <= 5; ++ILine) {
                    NextLine = W5DataFile.readLine();
                    DataLine(ILine) = NextLine.data;
                }

                if (!readItem(DataLine(1).substr(5), Rfsol)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RfSol values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 1, DataLine(1).substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Rfsol, 0.0) || any_gt(Rfsol, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RfSol values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 1, DataLine(1).substr(0, 100)));
                    ErrorsFound = true;
                }

                if (!readItem(DataLine(2).substr(5), Rbsol)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RbSol values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 2, DataLine(2).substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Rbsol, 0.0) || any_gt(Rbsol, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RbSol values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 2, DataLine(2).substr(0, 100)));
                    ErrorsFound = true;
                }
                if (!readItem(DataLine(3).substr(5), Tvis)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Tvis values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 3, DataLine(3).substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Tvis, 0.0) || any_gt(Tvis, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Tvis values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 3, DataLine(3).substr(0, 100)));
                    ErrorsFound = true;
                }
                if (!readItem(DataLine(4).substr(5), Rfvis)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rfvis values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 4, DataLine(4).substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Rfvis, 0.0) || any_gt(Rfvis, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rfvis values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 4, DataLine(4).substr(0, 100)));
                    ErrorsFound = true;
                }
                if (!readItem(DataLine(5).substr(5), Rbvis)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rbvis values.");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 5, DataLine(5).substr(0, 100)));
                    ErrorsFound = true;
                } else if (any_lt(Rbvis, 0.0) || any_gt(Rbvis, 1.0)) {
                    ShowSevereError(state, "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rbvis values. (out of range [0,1])");
                    ShowContinueError(state, format("Line (~{}) in error (first 100 characters)={}", FileLineCount + 5, DataLine(5).substr(0, 100)));
                    ErrorsFound = true;
                }
                FileLineCount += 5;

                if (ErrorsFound)
                    ShowFatalError(state,
                                   "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName +
                                       " from the Window5 data file cannot be used because of above errors");

                // Hemis
                state.dataConstruction->Construct(ConstrNum).TransDiff = Tsol(11);
                state.dataConstruction->Construct(ConstrNum).TransDiffVis = Tvis(11);
                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront = Rfsol(11);
                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack = Rbsol(11);
                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront = Rfvis(11);
                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack = Rbvis(11);

                W5LsqFit(CosPhiIndepVar, Tsol, 6, 1, 10, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                W5LsqFit(CosPhiIndepVar, Tvis, 6, 1, 10, state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef);
                W5LsqFit(CosPhiIndepVar, Rfsol, 6, 1, 10, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                for (IGlass = 1; IGlass <= NGlass(IGlSys); ++IGlass) {
                    W5LsqFit(CosPhiIndepVar, AbsSol(_, IGlass), 6, 1, 10, state.dataConstruction->Construct(ConstrNum).AbsBeamCoef(_, IGlass));
                }

                // For comparing fitted vs. input distribution in incidence angle
                for (IPhi = 1; IPhi <= 10; ++IPhi) {
                    tsolFit(IPhi) = POLYF(CosPhi(IPhi), state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                    tvisFit(IPhi) = POLYF(CosPhi(IPhi), state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef);
                    rfsolFit(IPhi) = POLYF(CosPhi(IPhi), state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                    for (IGlass = 1; IGlass <= NGlass(IGlSys); ++IGlass) {
                        solabsFit(IGlass, IPhi) = POLYF(CosPhi(IPhi), state.dataConstruction->Construct(ConstrNum).AbsBeamCoef({1, 6}, IGlass));
                    }
                }
                // end

                // NominalRforNominalUCalculation of this construction (actually the total resistance of all of its layers; gas layer
                // conductivity here ignores convective effects in gap.)
                NominalRforNominalUCalculation(ConstrNum) = 0.0;
                for (loop = 1; loop <= NGlass(IGlSys) + NGaps(IGlSys); ++loop) {
                    MatNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(loop);
                    if (state.dataMaterial->Material(MatNum).Group == WindowGlass) {
                        NominalRforNominalUCalculation(ConstrNum) +=
                            state.dataMaterial->Material(MatNum).Thickness / state.dataMaterial->Material(MatNum).Conductivity;
                    } else if (state.dataMaterial->Material(MatNum).Group == WindowGas ||
                               state.dataMaterial->Material(MatNum).Group == WindowGasMixture) {
                        // If mixture, use conductivity of first gas in mixture
                        NominalRforNominalUCalculation(ConstrNum) +=
                            state.dataMaterial->Material(MatNum).Thickness /
                            (state.dataMaterial->Material(MatNum).GasCon(1, 1) + state.dataMaterial->Material(MatNum).GasCon(2, 1) * 300.0 +
                             state.dataMaterial->Material(MatNum).GasCon(3, 1) * 90000.0);
                    }
                }

            } // End of loop over glazing systems

            // WindowFrameAndDivider objects

            TotFrameDividerPrev = TotFrameDivider;
            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                if (FrameWidth > 0.0 || DividerWidth(IGlSys) > 0.0) {
                    ++TotFrameDivider;
                    state.dataConstruction->Construct(TotConstructs - NGlSys + IGlSys).W5FrameDivider = TotFrameDivider;
                }
            }

            if (TotFrameDivider > TotFrameDividerPrev) {
                FrameDivider.redimension(TotFrameDivider);
            }

            for (IGlSys = 1; IGlSys <= NGlSys; ++IGlSys) {
                if (FrameWidth > 0.0 || DividerWidth(IGlSys) > 0.0) {
                    FrDivNum = state.dataConstruction->Construct(TotConstructs - NGlSys + IGlSys).W5FrameDivider;
                    FrameDivider(FrDivNum).FrameWidth = FrameWidth;
                    FrameDivider(FrDivNum).FrameProjectionOut = FrameProjectionOut;
                    FrameDivider(FrDivNum).FrameProjectionIn = FrameProjectionIn;
                    FrameDivider(FrDivNum).FrameConductance = FrameConductance;
                    FrameDivider(FrDivNum).FrEdgeToCenterGlCondRatio = FrEdgeToCenterGlCondRatio;
                    FrameDivider(FrDivNum).FrameSolAbsorp = FrameSolAbsorp;
                    FrameDivider(FrDivNum).FrameVisAbsorp = FrameVisAbsorp;
                    FrameDivider(FrDivNum).FrameEmis = FrameEmis;
                    FrameDivider(FrDivNum).FrameEdgeWidth = 0.06355; // 2.5 in
                    if (UtilityRoutines::SameString(MullionOrientation, "Vertical")) {
                        FrameDivider(FrDivNum).MullionOrientation = Vertical;
                    } else if (UtilityRoutines::SameString(MullionOrientation, "Horizontal")) {
                        FrameDivider(FrDivNum).MullionOrientation = Horizontal;
                    }
                    if (UtilityRoutines::SameString(DividerType(IGlSys), "DividedLite")) {
                        FrameDivider(FrDivNum).DividerType = DividedLite;
                    } else if (UtilityRoutines::SameString(DividerType(IGlSys), "Suspended")) {
                        FrameDivider(FrDivNum).DividerType = Suspended;
                    }
                    FrameDivider(FrDivNum).DividerWidth = DividerWidth(IGlSys);
                    FrameDivider(FrDivNum).HorDividers = HorDividers(IGlSys);
                    FrameDivider(FrDivNum).VertDividers = VertDividers(IGlSys);
                    FrameDivider(FrDivNum).DividerProjectionOut = DividerProjectionOut(IGlSys);
                    FrameDivider(FrDivNum).DividerProjectionIn = DividerProjectionIn(IGlSys);
                    FrameDivider(FrDivNum).DividerConductance = DividerConductance(IGlSys);
                    FrameDivider(FrDivNum).DivEdgeToCenterGlCondRatio = DivEdgeToCenterGlCondRatio(IGlSys);
                    FrameDivider(FrDivNum).DividerSolAbsorp = DividerSolAbsorp(IGlSys);
                    FrameDivider(FrDivNum).DividerVisAbsorp = DividerVisAbsorp(IGlSys);
                    FrameDivider(FrDivNum).DividerEmis = DividerEmis(IGlSys);
                    FrameDivider(FrDivNum).DividerEdgeWidth = 0.06355; // 2.5 in
                    if (NGlSys == 1) {
                        FrameDivider(FrDivNum).Name = "W5:" + DesiredConstructionName;
                    } else {
                        FrameDivider(FrDivNum).Name = "W5:" + DesiredConstructionName + ':' + NumName(IGlSys);
                    }
                }
            }

            if (FrameWidth > 0.0 && DividerWidth(1) > 0.0) {
                DisplayString(state, "--Construction and associated frame and divider found");
            } else if (FrameWidth > 0.0) {
                DisplayString(state, "--Construction and associated frame found");
            } else if (DividerWidth(1) > 0.0) {
                DisplayString(state, "--Construction and associated divider found");
            } else {
                DisplayString(state, "--Construction without frame or divider found");
            }
        }

        return;

    Label1000:;
        EOFonFile = true;
    }

    void SetStormWindowControl(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Jan 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Sets the storm window flag for each window, which is:
        //  -1: if storm window is not applicable (this will always be the value for interior
        //        windows since storm windows can only be applied to exterior windows
        //   0: if the window has a storm window but it is off
        //   1: if the window has a storm window and it is on

        // A "storm window" is a single layer of exterior glass separated from the main window by air gap.
        // Whether the storm window is in place is determined by the following values, which
        // which are specified in the Storm Window object for the window:
        //  -Month that Storm Window Is Put On
        //  -Day of Month that Storm Window Is Put On
        //  -Month that Storm Window Is Taken Off
        //  -Day of Month that Storm Window Is Taken Off

        // REFERENCES:na
        // Using/Aliasing
        using DataSurfaces::SurfWinStormWinFlag;
        using DataSurfaces::SurfWinStormWinFlagPrevDay;
        using General::BetweenDates;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:na

        // INTERFACE BLOCK SPECIFICATIONS:na

        // DERIVED TYPE DEFINITIONS:na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int SurfNum;      // Surface number
        int StormWinNum;  // Number of storm window object
        int StormWinFlag; // Storm window flag; this routine sets the following values:
        //   0: if the storm window is off this time step
        //   1: if the storm window is on this time step
        int DateOff; // Date Off for calculation

        StormWinChangeThisDay = false;

        for (StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
            SurfNum = StormWindow(StormWinNum).BaseWindowNum;
            SurfWinStormWinFlagPrevDay(SurfNum) = SurfWinStormWinFlag(SurfNum);
            DateOff = StormWindow(StormWinNum).DateOff - 1;
            // Note: Dateon = Dateoff is not allowed and will have produced an error in getinput.
            if (DateOff == 0) DateOff = 366;
            if (BetweenDates(state.dataEnvrn->DayOfYear_Schedule, StormWindow(StormWinNum).DateOn, DateOff)) {
                StormWinFlag = 1;
            } else {
                StormWinFlag = 0;
            }
            SurfWinStormWinFlag(SurfNum) = StormWinFlag;
            if (state.dataGlobal->BeginSimFlag) SurfWinStormWinFlagPrevDay(SurfNum) = StormWinFlag;
            if (SurfWinStormWinFlag(SurfNum) != SurfWinStormWinFlagPrevDay(SurfNum)) StormWinChangeThisDay = true;
        }
    }

    void CreateFCfactorConstructions(EnergyPlusData &state,
                                     int &ConstrNum,   // Counter for Constructions
                                     bool &ErrorsFound // If errors found in input
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tianzhen Hong
        //       DATE WRITTEN   July 2009
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine goes through each construction defined with Ffactor or Cfactor method,
        // and creates a construction (concrete + insulation) used in the heat transfer calculation.
        // This subroutine only gets called once in the GetConstructionData subroutine

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // ASHRAE Handbook Fundamental 2005
        // Thermal resistance of the inside air film, m2.K/W. Average of 0.14 (heat flow up) and 0.11 (heat flow down)
        Real64 const Rfilm_in(0.125);
        // Thermal resistance of the outside air film used in calculating the Ffactor, m2.K/W. 0.17/5.678
        Real64 const Rfilm_out(0.03);

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstructNumAlpha;             // Number of construction alpha names being passed
        int DummyNumProp;                  // dummy variable for properties being passed
        int IOStat;                        // IO Status when calling get input subroutine
        Array1D_string ConstructAlphas(1); // Construction Alpha names defined
        Array1D<Real64> DummyProps(4);     // Temporary array to transfer construction properties
        int Loop;

        int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
        int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

        Real64 Ffactor;          // Ffactor in W/m-K, applies to deltaT of outside - indoor air temperature
        Real64 Cfactor;          // Cfactor in W/m2-K, does not include soil or air films
        Real64 Area;             // floor area in m2
        Real64 PerimeterExposed; // perimeter exposed in m
        Real64 Height;           // Height of the underground wall in m

        Real64 Reff;          // Effective thermal resistance, m2.K/W
        Real64 Rcon;          // Concrete layer thermal resistance, m2.K/W
        Real64 Rfic;          // Thermal resistance of the fictitious material, m2.K/W
        int MaterNum;         // Material index
        Real64 Rsoilequ;      // Effective R-value of soil for underground walls
        int iFCConcreteLayer; // Layer pointer to the materials array

        // First get the concrete layer
        iFCConcreteLayer = UtilityRoutines::FindItemInList("~FC_Concrete", state.dataMaterial->Material);
        Rcon = state.dataMaterial->Material(iFCConcreteLayer).Resistance;

        // Count number of constructions defined with Ffactor or Cfactor method
        TotFfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:FfactorGroundFloor");
        TotCfactorConstructs = inputProcessor->getNumObjectsFound(state, "Construction:CfactorUndergroundWall");

        if (TotFfactorConstructs > 0) {
            NoFfactorConstructionsUsed = false;
        }

        if (TotCfactorConstructs > 0) {
            NoCfactorConstructionsUsed = false;
        }

        // First create ground floor constructions defined with F factor method if any
        CurrentModuleObject = "Construction:FfactorGroundFloor";

        // Loop through all constructs defined with Ffactor method
        for (Loop = 1; Loop <= TotFfactorConstructs; ++Loop) {

            // Get the object names for each construction from the input processor
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          ConstructAlphas,
                                          ConstructNumAlpha,
                                          DummyProps,
                                          DummyNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueConstructNames, ConstructAlphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            ++ConstrNum;

            state.dataConstruction->Construct(ConstrNum).Name = ConstructAlphas(1);
            state.dataConstruction->Construct(ConstrNum).TypeIsFfactorFloor = true;

            Ffactor = DummyProps(1);
            Area = DummyProps(2);
            PerimeterExposed = DummyProps(3);

            state.dataConstruction->Construct(ConstrNum).Area = Area;
            state.dataConstruction->Construct(ConstrNum).PerimeterExposed = PerimeterExposed;
            state.dataConstruction->Construct(ConstrNum).FFactor = Ffactor;

            if (Ffactor <= 0.0) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + ConstructAlphas(1) + "\" has " + cNumericFieldNames(1) + " <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Entered value=[{:.2R}]", Ffactor));
                ErrorsFound = true;
            }

            if (Area <= 0.0) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + ConstructAlphas(1) + "\" has " + cNumericFieldNames(2) + " <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Entered value=[{:.2R}]", Area));
                ErrorsFound = true;
            }

            if (PerimeterExposed < 0.0) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + ConstructAlphas(1) + "\" has " + cNumericFieldNames(3) + " <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Entered value=[{:.2R}]", PerimeterExposed));
                ErrorsFound = true;
            }

            // The construction has two layers which have been created in GetMaterialData
            state.dataConstruction->Construct(ConstrNum).TotLayers = 2;

            // The concrete is the inside layer
            state.dataConstruction->Construct(ConstrNum).LayerPoint(2) = iFCConcreteLayer;

            // The fictitious insulation is the outside layer
            MaterNum = UtilityRoutines::FindItemInList(format("~FC_Insulation_{}", Loop), state.dataMaterial->Material);
            state.dataConstruction->Construct(ConstrNum).LayerPoint(1) = MaterNum;

            // Calculate the thermal resistance of the fictitious insulation layer
            // effective thermal resistance excludes inside and outside air films
            if (PerimeterExposed > 0.0) {
                Reff = Area / (PerimeterExposed * Ffactor) - Rfilm_in - Rfilm_out;
            } else { // PerimeterExposed = 0 for underground floor, assume R-1000 (IP)
                Reff = 177.0;
            }

            Rfic = Reff - Rcon;
            if (Rfic <= 0.0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + ConstructAlphas(1) + "\" has calculated R value <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Calculated value=[{:.2R}] Check definition.", Rfic));
                ErrorsFound = true;
            }

            state.dataMaterial->Material(MaterNum).Resistance = Rfic;
            NominalR(MaterNum) = Rfic;

            // excluding thermal resistance of inside or outside air film
            // 1/Reff gets reported as the "U-Factor no Film" in the summary report Envelope Summary | Opaque Exterior
            NominalRforNominalUCalculation(ConstrNum) = Reff;
        }

        // Then create underground wall constructions defined with C factor method if any
        CurrentModuleObject = "Construction:CfactorUndergroundWall";

        for (Loop = 1; Loop <= TotCfactorConstructs; ++Loop) { // Loop through all constructs defined with Ffactor method

            // Get the object names for each construction from the input processor
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          Loop,
                                          ConstructAlphas,
                                          ConstructNumAlpha,
                                          DummyProps,
                                          DummyNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueConstructNames, ConstructAlphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            ++ConstrNum;

            state.dataConstruction->Construct(ConstrNum).Name = ConstructAlphas(1);
            state.dataConstruction->Construct(ConstrNum).TypeIsCfactorWall = true;

            Cfactor = DummyProps(1);
            Height = DummyProps(2);

            state.dataConstruction->Construct(ConstrNum).Height = Height;
            state.dataConstruction->Construct(ConstrNum).CFactor = Cfactor;

            if (Cfactor <= 0.0) {
                ShowSevereError(state, CurrentModuleObject + ' ' + ConstructAlphas(1) + " has " + cNumericFieldNames(1) + " <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Entered value=[{:.2R}]", Cfactor));
                ErrorsFound = true;
            }

            if (Height <= 0.0) {
                ShowSevereError(state, CurrentModuleObject + ' ' + ConstructAlphas(1) + " has " + cNumericFieldNames(2) + " <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Entered value=[{:.2R}]", Height));
                ErrorsFound = true;
            }

            // The construction has two layers which have been created in GetMaterialData
            state.dataConstruction->Construct(ConstrNum).TotLayers = 2;

            // The concrete is the inside layer
            state.dataConstruction->Construct(ConstrNum).LayerPoint(2) = iFCConcreteLayer;

            // The fictitious insulation is the outside layer
            MaterNum = UtilityRoutines::FindItemInList("~FC_Insulation_" + fmt::to_string(Loop + TotFfactorConstructs), state.dataMaterial->Material);
            state.dataConstruction->Construct(ConstrNum).LayerPoint(1) = MaterNum;

            // CR 8886 Rsoil should be in SI unit. From ASHRAE 90.1-2010 SI
            if (Height <= 0.25) {
                Rsoilequ = 0.12; // m2K/W
            } else if (Height >= 2.5) {
                Rsoilequ = 0.92;
            } else { // regression from ASHRAE 90.1-2010 SI TABLE C6.10.1 Effective R-Value of Soil, R2 = 0.9967
                Rsoilequ = 0.0607 + 0.3479 * Height;
            }

            // effective thermal resistance excludes inside and outside air films
            Reff = 1.0 / Cfactor + Rsoilequ; // Cfactor does not include air films

            Rfic = Reff - Rcon;
            if (Rfic <= 0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + ConstructAlphas(1) + "\" has calculated R value <= 0.0, must be > 0.0.");
                ShowContinueError(state, format("Calculated value=[{:.2R}] Check definition.", Rfic));
                ErrorsFound = true;
            }

            state.dataMaterial->Material(MaterNum).Resistance = Rfic;
            NominalR(MaterNum) = Rfic;

            // Reff includes the wall itself and soil, but excluding thermal resistance of inside or outside air film
            // 1/Reff gets reported as the "U-Factor no Film" in the summary report Envelope Summary | Opaque Exterior
            NominalRforNominalUCalculation(ConstrNum) = Reff;
        }
    }

    void CreateAirBoundaryConstructions(EnergyPlusData &state,
                                        int &constrNum,   // Counter for Constructions
                                        bool &errorsFound // If errors found in input
    )
    {
        cCurrentModuleObject = "Construction:AirBoundary";
        std::string RoutineName = "CreateAirBoundaryConstructions";
        int numAirBoundaryConstructs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (numAirBoundaryConstructs > 0) {
            auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
            if (instances == inputProcessor->epJSON.end()) {
                // Cannot imagine how you would have numAirBoundaryConstructs > 0 and yet the instances is empty
                // this would indicate a major problem in the input processor, not a problem here
                // I'll still catch this with errorsFound but I cannot make a unit test for it so excluding the line from coverage
                ShowSevereError(state,                                                                                  // LCOV_EXCL_LINE
                                cCurrentModuleObject + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
                errorsFound = true;                                                                                     // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                if (GlobalNames::VerifyUniqueInterObjectName(
                        state, UniqueConstructNames, thisObjectName, cCurrentModuleObject, "Name", errorsFound)) {
                    continue;
                }

                ++constrNum;
                auto &thisConstruct = state.dataConstruction->Construct(constrNum);

                thisConstruct.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisConstruct.TypeIsAirBoundary = true;
                thisConstruct.IsUsedCTF = false;

                // Air Exchange Method
                std::string const airMethod = fields.at("air_exchange_method");
                if (UtilityRoutines::SameString(airMethod, "SimpleMixing")) {
                    thisConstruct.TypeIsAirBoundaryMixing = true;
                    if (fields.find("simple_mixing_air_changes_per_hour") != fields.end()) {
                        thisConstruct.AirBoundaryACH = fields.at("simple_mixing_air_changes_per_hour");
                    } else {
                        if (!inputProcessor->getDefaultValue(
                                state, cCurrentModuleObject, "simple_mixing_air_changes_per_hour", thisConstruct.AirBoundaryACH)) {
                            errorsFound = true;
                        }
                    }
                    if (fields.find("simple_mixing_schedule_name") != fields.end()) {
                        auto &schedName = fields.at("simple_mixing_schedule_name");
                        thisConstruct.AirBoundaryMixingSched = ScheduleManager::GetScheduleIndex(state, UtilityRoutines::MakeUPPERCase(schedName));
                        if (thisConstruct.AirBoundaryMixingSched == 0) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + thisConstruct.Name + "\", invalid (not found) " +
                                                "Simple Mixing Schedule Name" + "=\"" + schedName.get<std::string>() + "\".");
                            errorsFound = true;
                        }
                    } else {
                        thisConstruct.AirBoundaryMixingSched = DataGlobalConstants::ScheduleAlwaysOn;
                    }
                }
            }
        }
    }

    void GetScheduledSurfaceGains(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   June 2013
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Loads scheduled surface gains for solar incident on interior side of the surfaces and absorbed solar energy in
        // window layers

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::TotConstructs;
        using DataSurfaces::FenLayAbsSSG;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfIncSolSSG;
        using DataSurfaces::TotFenLayAbsSSG;
        using DataSurfaces::TotSurfaces;
        using DataSurfaces::TotSurfIncSolSSG;

        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetScheduledSurfaceGains: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumArgs;
        int NumAlpha;
        int NumNumeric;
        int Loop;
        int IOStat;
        int SurfNum;
        int ConstrNum;
        int ScheduleNum;
        int i;
        int NumOfScheduledLayers;
        bool NumOfLayersMatch;
        int iZone;

        //-----------------------------------------------------------------------
        //                SurfaceProperty:SolarIncidentInside
        //-----------------------------------------------------------------------
        cCurrentModuleObject = "SurfaceProperty:SolarIncidentInside";

        // Check if IDD definition is correct
        inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumeric);
        if (NumAlpha != 4) {
            ShowSevereError(
                state,
                format("{}{}: Object Definition indicates not = 4 Alpha Objects, Number Indicated={}", RoutineName, cCurrentModuleObject, NumAlpha));
            ErrorsFound = true;
        }

        TotSurfIncSolSSG = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotSurfIncSolSSG > 0) {
            if (!allocated(SurfIncSolSSG)) {
                SurfIncSolSSG.allocate(TotSurfIncSolSSG);
            }

            for (Loop = 1; Loop <= TotSurfIncSolSSG; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlpha,
                                              rNumericArgs,
                                              NumNumeric,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                    ShowContinueError(
                        state, "...each SurfaceProperty:SolarIncidentInside name must not duplicate other SurfaceProperty:SolarIncidentInside name");
                    continue;
                }

                SurfIncSolSSG(Loop).Name = cAlphaArgs(1);

                // Assign surface number
                SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Surface);
                if (SurfNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                          "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    SurfIncSolSSG(Loop).SurfPtr = SurfNum;
                }

                // Assign construction number
                ConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct);
                if (ConstrNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
                                          "\" no corresponding construction (ref Construction) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    SurfIncSolSSG(Loop).ConstrPtr = ConstrNum;
                }

                // Assign schedule number
                ScheduleNum = GetScheduleIndex(state, cAlphaArgs(4));
                if (ScheduleNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(4) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(4) + " entered value = \"" + cAlphaArgs(4) +
                                          "\" no corresponding schedule has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    SurfIncSolSSG(Loop).SchedPtr = ScheduleNum;
                }
            }
        }

        //-----------------------------------------------------------------------
        //                SurfaceProperty:SolarIncidentInside
        //-----------------------------------------------------------------------
        cCurrentModuleObject = "ComplexFenestrationProperty:SolarAbsorbedLayers";

        TotFenLayAbsSSG = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotFenLayAbsSSG > 0) {
            if (!allocated(FenLayAbsSSG)) {
                FenLayAbsSSG.allocate(TotFenLayAbsSSG);
            }

            for (Loop = 1; Loop <= TotFenLayAbsSSG; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlpha,
                                              rNumericArgs,
                                              NumNumeric,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                    ShowContinueError(state,
                                      "...each ComplexFenestrationProperty:SolarAbsorbedLayers name must not duplicate other "
                                      "ComplexFenestrationProperty:SolarAbsorbedLayers name");
                    continue;
                }

                FenLayAbsSSG(Loop).Name = cAlphaArgs(1);

                // Assign surface number
                SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Surface);
                if (SurfNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                          "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    FenLayAbsSSG(Loop).SurfPtr = SurfNum;
                }

                // Assign construction number
                ConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct);
                if (ConstrNum == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
                                          "\" no corresponding construction (ref Construction) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    FenLayAbsSSG(Loop).ConstrPtr = ConstrNum;
                    NumOfScheduledLayers = NumAlpha - 3;
                    NumOfLayersMatch = false;
                    // Check if number of layers in construction matches number of layers in schedule surface gains object
                    if (NumOfScheduledLayers == state.dataConstruction->Construct(ConstrNum).TotSolidLayers) {
                        NumOfLayersMatch = true;
                    }

                    if (!NumOfLayersMatch) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                            ", object. Number of scheduled surface gains for each layer does not match number of layers in "
                                            "referenced construction.");
                        ShowContinueError(state,
                                          format("{} have {} schedule layers and {} have {} layers.",
                                                 cAlphaArgs(1),
                                                 NumOfScheduledLayers,
                                                 cAlphaArgs(3),
                                                 state.dataConstruction->Construct(ConstrNum).TotSolidLayers));
                        ErrorsFound = true;
                    }

                    if (!allocated(FenLayAbsSSG(Loop).SchedPtrs)) {
                        FenLayAbsSSG(Loop).SchedPtrs.allocate(NumOfScheduledLayers);
                    }

                    FenLayAbsSSG(Loop).NumOfSched = NumOfScheduledLayers;

                    for (i = 1; i <= NumOfScheduledLayers; ++i) {
                        ScheduleNum = GetScheduleIndex(state, cAlphaArgs(i + 3));
                        if (ScheduleNum == 0) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                                cAlphaFieldNames(NumOfScheduledLayers + 3) + " has been found.");
                            ShowContinueError(state,
                                              cAlphaFieldNames(NumOfScheduledLayers + 3) + " entered value = \"" +
                                                  cAlphaArgs(NumOfScheduledLayers + 3) +
                                                  "\" no corresponding schedule has been found in the input file.");
                            ErrorsFound = true;
                        } else {
                            FenLayAbsSSG(Loop).SchedPtrs(i) = ScheduleNum;
                        }
                    }
                }
            }
        }

        // Check if scheduled surface gains are assigined to each surface in every zone.  If not then warning message to user will be
        // issued
        if ((TotSurfIncSolSSG > 0) || (TotFenLayAbsSSG > 0)) {
            for (iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                CheckScheduledSurfaceGains(state, iZone);
            }
        }
    }

    void CheckScheduledSurfaceGains(EnergyPlusData &state, int const ZoneNum) // Zone number for which error check will be performed
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2013
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Check if all surfaces within zone are scheduled with surface gains. If not all surfaces within zone are scheduled,
        // warning message will be issued and program will continue to execute.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using SolarShading::SurfaceScheduledSolarInc;
        using SolarShading::WindowScheduledSolarAbs;
        using namespace DataSurfaces;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int iSurf;
        int iConst;
        int SchedPtr;         // scheduled surface gains pointer
        bool ZoneUnscheduled; // true if all surfaces in the zone are unscheduled
        bool ZoneScheduled;   // true if all surfaces in the zone are scheduled

        ZoneUnscheduled = false;
        ZoneScheduled = false;

        for (iSurf = Zone(ZoneNum).SurfaceFirst; iSurf <= Zone(ZoneNum).SurfaceLast; ++iSurf) {
            iConst = Surface(iSurf).Construction;
            if (Surface(iSurf).Class == SurfaceClass::Window) {
                SchedPtr = WindowScheduledSolarAbs(iSurf, iConst);
            } else {
                SchedPtr = SurfaceScheduledSolarInc(iSurf, iConst);
            }
            if (iSurf == Zone(ZoneNum).SurfaceFirst) {
                if (SchedPtr != 0) {
                    ZoneScheduled = true;
                    ZoneUnscheduled = false;
                } else {
                    ZoneScheduled = false;
                    ZoneUnscheduled = true;
                }
            } else {
                if (SchedPtr != 0) {
                    ZoneUnscheduled = false;
                } else {
                    ZoneScheduled = false;
                }
            }

            if ((!ZoneScheduled) && (!ZoneUnscheduled)) {
                // zone is nor scheduled nor unscheduled
                ShowWarningError(state, "Zone " + Zone(ZoneNum).Name + " does not have all surfaces scheduled with surface gains.");
                ShowContinueError(state,
                                  "If at least one surface in the zone is scheduled with surface gains, then all other surfaces within the same zone "
                                  "should be scheduled as well.");
                break;
            }
        }

        if ((!ZoneScheduled) && (!ZoneUnscheduled)) {
            for (iSurf = Zone(ZoneNum).SurfaceFirst; iSurf <= Zone(ZoneNum).SurfaceLast; ++iSurf) {
                iConst = Surface(iSurf).Construction;
                if (Surface(iSurf).Class == SurfaceClass::Window) {
                    SchedPtr = WindowScheduledSolarAbs(iSurf, iConst);
                } else {
                    SchedPtr = SurfaceScheduledSolarInc(iSurf, iConst);
                }

                if (SchedPtr == 0) {
                    ShowContinueError(state, "Surface " + Surface(iSurf).Name + " does not have scheduled surface gains.");
                }
            }
        }
    }

    void CreateTCConstructions(EnergyPlusData &state, [[maybe_unused]] bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tianzhen Hong
        //       DATE WRITTEN   January 2009
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine goes through each TC master construction and creates a complete series
        // of the slave thermochromic constructions.
        // This subroutine only gets called once in the GetHeatBalanceInput subroutine
        //  after materials, constructions and building geometry data are read.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Using/Aliasing
        using namespace DataStringGlobals;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int Loop;
        static int iTC(0);
        static int iMat(0);
        static int NumNewConst(0);
        static int iTCG(0);

        NumNewConst = 0;
        for (Loop = 1; Loop <= TotConstructs; ++Loop) {
            if (state.dataConstruction->Construct(Loop).TCFlag == 1) {
                iTCG = state.dataMaterial->Material(state.dataConstruction->Construct(Loop).TCLayer).TCParent;
                if (iTCG == 0) continue; // hope this was caught already
                iMat = TCGlazings(iTCG).NumGlzMat;
                for (iTC = 1; iTC <= iMat; ++iTC) {
                    ++NumNewConst;
                }
            }
        }

        if (NumNewConst == 0) return; // no need to go further

        // Increase Construct() and copy the extra constructions
        state.dataConstruction->Construct.redimension(TotConstructs + NumNewConst);
        NominalRforNominalUCalculation.redimension(TotConstructs + NumNewConst);
        NominalU.redimension(TotConstructs + NumNewConst);

        NumNewConst = TotConstructs;
        for (Loop = 1; Loop <= TotConstructs; ++Loop) {
            if (state.dataConstruction->Construct(Loop).TCFlag == 1) {
                iTCG = state.dataMaterial->Material(state.dataConstruction->Construct(Loop).TCLayer).TCParent;
                if (iTCG == 0) continue; // hope this was caught already
                iMat = TCGlazings(iTCG).NumGlzMat;
                for (iTC = 1; iTC <= iMat; ++iTC) {
                    ++NumNewConst;
                    state.dataConstruction->Construct(NumNewConst) = state.dataConstruction->Construct(Loop); // copy data
                    state.dataConstruction->Construct(NumNewConst).Name =
                        format("{}_TC_{:.0R}", state.dataConstruction->Construct(Loop).Name, TCGlazings(iTCG).SpecTemp(iTC));
                    state.dataConstruction->Construct(NumNewConst).TCLayer = TCGlazings(iTCG).LayerPoint(iTC);
                    state.dataConstruction->Construct(NumNewConst).LayerPoint(state.dataConstruction->Construct(Loop).TCLayerID) =
                        state.dataConstruction->Construct(NumNewConst).TCLayer;
                    state.dataConstruction->Construct(NumNewConst).TCFlag = 1;
                    state.dataConstruction->Construct(NumNewConst).TCMasterConst = Loop;
                    state.dataConstruction->Construct(NumNewConst).TCLayerID = state.dataConstruction->Construct(Loop).TCLayerID;
                    state.dataConstruction->Construct(NumNewConst).TCGlassID = state.dataConstruction->Construct(Loop).TCGlassID;
                    state.dataConstruction->Construct(NumNewConst).TypeIsWindow = true;
                }
            }
        }
        TotConstructs = NumNewConst;
    }

    void SetupSimpleWindowGlazingSystem(EnergyPlusData &state, int &MaterNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   January 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Convert simple window performance indices into all the properties needed to
        // describe a single, equivalent glass layer

        // METHODOLOGY EMPLOYED:
        // The simple window indices are converted to a single materal layer using a "block model"

        // REFERENCES:
        // draft paper by Arasteh, Kohler, and Griffith

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
        static Real64 Riw(0.0);            // thermal resistance of interior film coefficient under winter conditions (m2-K/W)
        static Real64 Row(0.0);            // theraml resistance of exterior film coefficient under winter conditions (m2-K/W)
        static Real64 Rlw(0.0);            // thermal resistance of block model layer (m2-K/W)
        static Real64 Ris(0.0);            // thermal resistance of interior film coefficient under summer conditions (m2-K/W)
        static Real64 Ros(0.0);            // theraml resistance of exterior film coefficient under summer conditions (m2-K/W)
        static Real64 InflowFraction(0.0); // inward flowing fraction for SHGC, intermediate value non dimensional
        static Real64 SolarAbsorb(0.0);    // solar aborptance
        bool ErrorsFound(false);
        static Real64 TsolLowSide(0.0);      // intermediate solar transmission for interpolating
        static Real64 TsolHiSide(0.0);       // intermediate solar transmission for interpolating
        static Real64 DeltaSHGCandTsol(0.0); // intermediate difference
        static Real64 RLowSide(0.0);
        static Real64 RHiSide(0.0);

        // first fill out defaults
        state.dataMaterial->Material(MaterNum).GlassSpectralDataPtr = 0;
        state.dataMaterial->Material(MaterNum).SolarDiffusing = false;
        state.dataMaterial->Material(MaterNum).Roughness = VerySmooth;
        state.dataMaterial->Material(MaterNum).TransThermal = 0.0;
        state.dataMaterial->Material(MaterNum).AbsorpThermalBack = 0.84;
        state.dataMaterial->Material(MaterNum).AbsorpThermalFront = 0.84;
        state.dataMaterial->Material(MaterNum).AbsorpThermal = state.dataMaterial->Material(MaterNum).AbsorpThermalBack;

        // step 1. Determine U-factor without film coefficients
        // Simple window model has its own correlation for film coefficients (m2-K/W) under Winter conditions as function of U-factor
        if (state.dataMaterial->Material(MaterNum).SimpleWindowUfactor < 5.85) {
            Riw = 1.0 / (0.359073 * std::log(state.dataMaterial->Material(MaterNum).SimpleWindowUfactor) + 6.949915);
        } else {
            Riw = 1.0 / (1.788041 * state.dataMaterial->Material(MaterNum).SimpleWindowUfactor - 2.886625);
        }
        Row = 1.0 / (0.025342 * state.dataMaterial->Material(MaterNum).SimpleWindowUfactor + 29.163853);

        // determine 1/U without film coefficients
        Rlw = (1.0 / state.dataMaterial->Material(MaterNum).SimpleWindowUfactor) - Riw - Row;
        if (Rlw <= 0.0) { // U factor of film coefficients is better than user input.
            Rlw = max(Rlw, 0.001);
            ShowWarningError(state,
                             "WindowMaterial:SimpleGlazingSystem: " + state.dataMaterial->Material(MaterNum).Name +
                                 " has U-factor higher than that provided by surface film resistances, Check value of U-factor");
        }

        // Step 2. determine layer thickness.

        if ((1.0 / Rlw) > 7.0) {
            state.dataMaterial->Material(MaterNum).Thickness = 0.002;
        } else {
            state.dataMaterial->Material(MaterNum).Thickness = 0.05914 - (0.00714 / Rlw);
        }

        // Step 3. determine effective conductivity

        state.dataMaterial->Material(MaterNum).Conductivity = state.dataMaterial->Material(MaterNum).Thickness / Rlw;
        if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
            NominalR(MaterNum) = Rlw;
            state.dataMaterial->Material(MaterNum).Resistance = Rlw;
        } else {
            ErrorsFound = true;
            ShowSevereError(state,
                            "WindowMaterial:SimpleGlazingSystem: " + state.dataMaterial->Material(MaterNum).Name +
                                " has Conductivity <= 0.0, must be >0.0, Check value of U-factor");
        }

        // step 4. determine solar transmission (revised to 10-1-2009 version from LBNL.)

        if (state.dataMaterial->Material(MaterNum).SimpleWindowUfactor > 4.5) {

            if (state.dataMaterial->Material(MaterNum).SimpleWindowSHGC < 0.7206) {

                state.dataMaterial->Material(MaterNum).Trans = 0.939998 * pow_2(state.dataMaterial->Material(MaterNum).SimpleWindowSHGC) +
                                                               0.20332 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC;
            } else { // >= 0.7206

                state.dataMaterial->Material(MaterNum).Trans = 1.30415 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - 0.30515;
            }

        } else if (state.dataMaterial->Material(MaterNum).SimpleWindowUfactor < 3.4) {

            if (state.dataMaterial->Material(MaterNum).SimpleWindowSHGC <= 0.15) {
                state.dataMaterial->Material(MaterNum).Trans = 0.41040 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC;
            } else { // > 0.15
                state.dataMaterial->Material(MaterNum).Trans = 0.085775 * pow_2(state.dataMaterial->Material(MaterNum).SimpleWindowSHGC) +
                                                               0.963954 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - 0.084958;
            }
        } else { // interpolate. 3.4 <= Ufactor <= 4.5

            if (state.dataMaterial->Material(MaterNum).SimpleWindowSHGC < 0.7206) {
                TsolHiSide = 0.939998 * pow_2(state.dataMaterial->Material(MaterNum).SimpleWindowSHGC) +
                             0.20332 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC;
            } else { // >= 0.7206
                TsolHiSide = 1.30415 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - 0.30515;
            }

            if (state.dataMaterial->Material(MaterNum).SimpleWindowSHGC <= 0.15) {
                TsolLowSide = 0.41040 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC;
            } else { // > 0.15
                TsolLowSide = 0.085775 * pow_2(state.dataMaterial->Material(MaterNum).SimpleWindowSHGC) +
                              0.963954 * state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - 0.084958;
            }

            state.dataMaterial->Material(MaterNum).Trans =
                ((state.dataMaterial->Material(MaterNum).SimpleWindowUfactor - 3.4) / (4.5 - 3.4)) * (TsolHiSide - TsolLowSide) + TsolLowSide;
        }
        if (state.dataMaterial->Material(MaterNum).Trans < 0.0) state.dataMaterial->Material(MaterNum).Trans = 0.0;

        // step 5.  determine solar reflectances

        DeltaSHGCandTsol = state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - state.dataMaterial->Material(MaterNum).Trans;

        if (state.dataMaterial->Material(MaterNum).SimpleWindowUfactor > 4.5) {

            Ris = 1.0 / (29.436546 * pow_3(DeltaSHGCandTsol) - 21.943415 * pow_2(DeltaSHGCandTsol) + 9.945872 * DeltaSHGCandTsol + 7.426151);
            Ros = 1.0 / (2.225824 * DeltaSHGCandTsol + 20.577080);
        } else if (state.dataMaterial->Material(MaterNum).SimpleWindowUfactor < 3.4) {

            Ris = 1.0 / (199.8208128 * pow_3(DeltaSHGCandTsol) - 90.639733 * pow_2(DeltaSHGCandTsol) + 19.737055 * DeltaSHGCandTsol + 6.766575);
            Ros = 1.0 / (5.763355 * DeltaSHGCandTsol + 20.541528);
        } else { // interpolate. 3.4 <= Ufactor <= 4.5
            // inside first
            RLowSide = 1.0 / (199.8208128 * pow_3(DeltaSHGCandTsol) - 90.639733 * pow_2(DeltaSHGCandTsol) + 19.737055 * DeltaSHGCandTsol + 6.766575);
            RHiSide = 1.0 / (29.436546 * pow_3(DeltaSHGCandTsol) - 21.943415 * pow_2(DeltaSHGCandTsol) + 9.945872 * DeltaSHGCandTsol + 7.426151);
            Ris = ((state.dataMaterial->Material(MaterNum).SimpleWindowUfactor - 3.4) / (4.5 - 3.4)) * (RLowSide - RHiSide) + RLowSide;
            // then outside
            RLowSide = 1.0 / (5.763355 * DeltaSHGCandTsol + 20.541528);
            RHiSide = 1.0 / (2.225824 * DeltaSHGCandTsol + 20.577080);
            Ros = ((state.dataMaterial->Material(MaterNum).SimpleWindowUfactor - 3.4) / (4.5 - 3.4)) * (RLowSide - RHiSide) + RLowSide;
        }

        InflowFraction = (Ros + 0.5 * Rlw) / (Ros + Rlw + Ris);

        SolarAbsorb = (state.dataMaterial->Material(MaterNum).SimpleWindowSHGC - state.dataMaterial->Material(MaterNum).Trans) / InflowFraction;
        state.dataMaterial->Material(MaterNum).ReflectSolBeamBack = 1.0 - state.dataMaterial->Material(MaterNum).Trans - SolarAbsorb;
        state.dataMaterial->Material(MaterNum).ReflectSolBeamFront = state.dataMaterial->Material(MaterNum).ReflectSolBeamBack;

        // step 6. determine visible properties.
        if (state.dataMaterial->Material(MaterNum).SimpleWindowVTinputByUser) {
            state.dataMaterial->Material(MaterNum).TransVis = state.dataMaterial->Material(MaterNum).SimpleWindowVisTran;
            state.dataMaterial->Material(MaterNum).ReflectVisBeamBack = -0.7409 * pow_3(state.dataMaterial->Material(MaterNum).TransVis) +
                                                                        1.6531 * pow_2(state.dataMaterial->Material(MaterNum).TransVis) -
                                                                        1.2299 * state.dataMaterial->Material(MaterNum).TransVis + 0.4545;
            if (state.dataMaterial->Material(MaterNum).TransVis + state.dataMaterial->Material(MaterNum).ReflectVisBeamBack >= 1.0) {
                state.dataMaterial->Material(MaterNum).ReflectVisBeamBack = 0.999 - state.dataMaterial->Material(MaterNum).TransVis;
            }

            state.dataMaterial->Material(MaterNum).ReflectVisBeamFront = -0.0622 * pow_3(state.dataMaterial->Material(MaterNum).TransVis) +
                                                                         0.4277 * pow_2(state.dataMaterial->Material(MaterNum).TransVis) -
                                                                         0.4169 * state.dataMaterial->Material(MaterNum).TransVis + 0.2399;
            if (state.dataMaterial->Material(MaterNum).TransVis + state.dataMaterial->Material(MaterNum).ReflectVisBeamFront >= 1.0) {
                state.dataMaterial->Material(MaterNum).ReflectVisBeamFront = 0.999 - state.dataMaterial->Material(MaterNum).TransVis;
            }
        } else {
            state.dataMaterial->Material(MaterNum).TransVis = state.dataMaterial->Material(MaterNum).Trans;
            state.dataMaterial->Material(MaterNum).ReflectVisBeamBack = state.dataMaterial->Material(MaterNum).ReflectSolBeamBack;
            state.dataMaterial->Material(MaterNum).ReflectVisBeamFront = state.dataMaterial->Material(MaterNum).ReflectSolBeamFront;
        }

        // step 7. The dependence on incident angle is in subroutine TransAndReflAtPhi

        // step 8.  Hemispherical terms are averaged using standard method

        if (ErrorsFound) {
            ShowFatalError(state, "Program halted because of input problem(s) in WindowMaterial:SimpleGlazingSystem");
        }
    }

    void SetupComplexFenestrationMaterialInput(EnergyPlusData &state,
                                               int &MaterNum, // num of material items thus far
                                               bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   March 2012
        //       MODIFIED       May 2013 (Simon Vidanovic)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get input for complex fenestration materials

        // METHODOLOGY EMPLOYED:
        // usual GetInput processing.

        // REFERENCES:
        // na

        // Using/Aliasing

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS
        static std::string const RoutineName("SetupComplexFenestrationMaterialInput: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string MaterialNames(5);   // Number of Material Alpha names defined
        Array1D<Real64> MaterialProps(27); // Temporary array to transfer material properties
        int Loop;
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;   // Used in GetObjectItem

        // Reading WindowGap:SupportPillar
        cCurrentModuleObject = "WindowGap:SupportPillar";
        W7SupportPillars = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        SupportPillar.allocate(W7SupportPillars);
        for (Loop = 1; Loop <= W7SupportPillars; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), CurrentModuleObject, ErrorsFound)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cAlphaFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            SupportPillar(Loop).Name = cAlphaArgs(1);
            SupportPillar(Loop).Spacing = rNumericArgs(1);
            SupportPillar(Loop).Radius = rNumericArgs(2);

            if (rNumericArgs(1) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
            }

            if (rNumericArgs(2) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(2) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(2), rNumericArgs(2)));
            }
        }

        // Reading WindowGap:DeflectionState
        cCurrentModuleObject = "WindowGap:DeflectionState";
        W7DeflectionStates = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        DeflectionState.allocate(W7DeflectionStates);
        for (Loop = 1; Loop <= W7DeflectionStates; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), CurrentModuleObject, ErrorsFound)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cAlphaFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            DeflectionState(Loop).Name = cAlphaArgs(1);
            DeflectionState(Loop).DeflectedThickness = rNumericArgs(1);
            if (rNumericArgs(1) < 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >= 0, entered value = {:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
            }
        }

        // Reading WindowMaterial:Gap

        cCurrentModuleObject = "WindowMaterial:Gap";
        W7MaterialGaps = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        // ALLOCATE(DeflectionState(W7DeflectionStates))
        for (Loop = 1; Loop <= W7MaterialGaps; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueMaterialNames, cAlphaArgs(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = ComplexWindowGap;
            state.dataMaterial->Material(MaterNum).Roughness = Rough;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            state.dataMaterial->Material(MaterNum).Name = cAlphaArgs(1);

            state.dataMaterial->Material(MaterNum).Thickness = rNumericArgs(1);
            if (rNumericArgs(1) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered {:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
            }

            state.dataMaterial->Material(MaterNum).Pressure = rNumericArgs(2);
            if (rNumericArgs(2) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(2) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered {:.2R}", cNumericFieldNames(2), rNumericArgs(2)));
            }

            if (!lAlphaFieldBlanks(2)) {
                state.dataMaterial->Material(MaterNum).GasPointer = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataMaterial->Material);
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cAlphaFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, cCurrentModuleObject + " does not have assigned WindowMaterial:Gas or WindowMaterial:GasMixutre.");
            }
            if (!lAlphaFieldBlanks(3)) {
                state.dataMaterial->Material(MaterNum).DeflectionStatePtr = UtilityRoutines::FindItemInList(cAlphaArgs(3), DeflectionState);
            }
            if (!lAlphaFieldBlanks(4)) {
                state.dataMaterial->Material(MaterNum).SupportPillarPtr = UtilityRoutines::FindItemInList(cAlphaArgs(4), SupportPillar);
            }
        }

        // Reading WindowMaterial:ComplexShade
        cCurrentModuleObject = "WindowMaterial:ComplexShade";
        TotComplexShades = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotComplexShades > 0) {
            ComplexShade.allocate(TotComplexShades); // Allocate the array Size to the number of complex shades
        }

        for (Loop = 1; Loop <= TotComplexShades; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), CurrentModuleObject, ErrorsFound)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cAlphaFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
                continue;
            }

            ++MaterNum;
            state.dataMaterial->Material(MaterNum).Group = ComplexWindowShade;
            state.dataMaterial->Material(MaterNum).Roughness = Rough;
            state.dataMaterial->Material(MaterNum).ROnly = true;

            // Assign pointer to ComplexShade
            state.dataMaterial->Material(MaterNum).ComplexShadePtr = Loop;

            state.dataMaterial->Material(MaterNum).Name = cAlphaArgs(1);
            ComplexShade(Loop).Name = cAlphaArgs(1);

            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "OTHERSHADINGTYPE") {
                    ComplexShade(Loop).LayerType = csOtherShadingType;
                } else if (SELECT_CASE_var == "VENETIANHORIZONTAL") {
                    ComplexShade(Loop).LayerType = csVenetianHorizontal;
                } else if (SELECT_CASE_var == "VENETIANVERTICAL") {
                    ComplexShade(Loop).LayerType = csVenetianVertical;
                } else if (SELECT_CASE_var == "WOVEN") {
                    ComplexShade(Loop).LayerType = csWoven;
                } else if (SELECT_CASE_var == "PERFORATED") {
                    ComplexShade(Loop).LayerType = csPerforated;
                } else if (SELECT_CASE_var == "BSDF") {
                    ComplexShade(Loop).LayerType = csBSDF;
                } else {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                          "\" should be OtherShadingType, Venetian, Woven, Perforated or BSDF.");
                }
            }

            ComplexShade(Loop).Thickness = rNumericArgs(1);
            state.dataMaterial->Material(MaterNum).Thickness = rNumericArgs(1);
            ComplexShade(Loop).Conductivity = rNumericArgs(2);
            state.dataMaterial->Material(MaterNum).Conductivity = rNumericArgs(2);
            ComplexShade(Loop).IRTransmittance = rNumericArgs(3);
            ComplexShade(Loop).FrontEmissivity = rNumericArgs(4);
            ComplexShade(Loop).BackEmissivity = rNumericArgs(5);

            // Simon: in heat balance radiation exchange routines AbsorpThermal is used
            // and program will crash if value is not assigned.  Not sure if this is correct
            // or some additional calculation is necessary. Simon TODO
            state.dataMaterial->Material(MaterNum).AbsorpThermal = rNumericArgs(5);
            state.dataMaterial->Material(MaterNum).AbsorpThermalFront = rNumericArgs(4);
            state.dataMaterial->Material(MaterNum).AbsorpThermalBack = rNumericArgs(5);

            ComplexShade(Loop).TopOpeningMultiplier = rNumericArgs(6);
            ComplexShade(Loop).BottomOpeningMultiplier = rNumericArgs(7);
            ComplexShade(Loop).LeftOpeningMultiplier = rNumericArgs(8);
            ComplexShade(Loop).RightOpeningMultiplier = rNumericArgs(9);
            ComplexShade(Loop).FrontOpeningMultiplier = rNumericArgs(10);

            ComplexShade(Loop).SlatWidth = rNumericArgs(11);
            ComplexShade(Loop).SlatSpacing = rNumericArgs(12);
            ComplexShade(Loop).SlatThickness = rNumericArgs(13);
            ComplexShade(Loop).SlatAngle = rNumericArgs(14);
            ComplexShade(Loop).SlatConductivity = rNumericArgs(15);
            ComplexShade(Loop).SlatCurve = rNumericArgs(16);

            // IF (dataMaterial.Material(MaterNum)%Conductivity > 0.0) THEN
            //  NominalR(MaterNum)=dataMaterial.Material(MaterNum)%Thickness/dataMaterial.Material(MaterNum)%Conductivity
            // ELSE
            //  NominalR(MaterNum)=1.0
            // ENDIF

            if (rNumericArgs(1) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
            }

            if (rNumericArgs(2) <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(2) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(2), rNumericArgs(2)));
            }

            if ((rNumericArgs(3) < 0.0) || (rNumericArgs(3) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(3) +
                                    " has been found.");
                ShowContinueError(state, format("{} value must be >= 0 and <= 1, entered value = {:.2R}", cNumericFieldNames(3), rNumericArgs(3)));
            }

            if ((rNumericArgs(4) <= 0.0) || (rNumericArgs(4) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(4) +
                                    " has been found.");
                ShowContinueError(state, format("{} value must be >= 0 and <= 1, entered value = {:.2R}", cNumericFieldNames(4), rNumericArgs(4)));
            }

            if ((rNumericArgs(5) <= 0.0) || (rNumericArgs(5) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(5) +
                                    " has been found.");
                ShowContinueError(state, format("{} value must be >= 0 and <= 1, entered value = {:.2R}", cNumericFieldNames(5), rNumericArgs(5)));
            }

            if ((rNumericArgs(6) < 0.0) || (rNumericArgs(6) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(6) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >= 0 or <= 1, entered value = {:.2R}", cNumericFieldNames(6), rNumericArgs(6)));
            }

            if ((rNumericArgs(7) < 0.0) || (rNumericArgs(7) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(7) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >=0 or <=1, entered {:.2R}", cNumericFieldNames(7), rNumericArgs(7)));
            }

            if ((rNumericArgs(8) < 0.0) || (rNumericArgs(8) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(8) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >=0 or <=1, entered value = {:.2R}", cNumericFieldNames(8), rNumericArgs(8)));
            }

            if ((rNumericArgs(9) < 0.0) || (rNumericArgs(9) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(9) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >=0 or <=1, entered value = {:.2R}", cNumericFieldNames(9), rNumericArgs(9)));
            }

            if ((rNumericArgs(10) < 0.0) || (rNumericArgs(10) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(10) +
                                    " has been found.");
                ShowContinueError(state, format("{} must be >=0 or <=1, entered value = {:.2R}", cNumericFieldNames(10), rNumericArgs(10)));
            }

            if (ComplexShade(Loop).LayerType == csVenetianHorizontal || ComplexShade(Loop).LayerType == csVenetianVertical) {
                if (rNumericArgs(11) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(11) + " has been found.");
                    ShowContinueError(state, format("{} must be >0, entered value = {:.2R}", cNumericFieldNames(11), rNumericArgs(11)));
                }

                if (rNumericArgs(12) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(12) + " has been found.");
                    ShowContinueError(state, format("{} must be >0, entered value = {:.2R}", cNumericFieldNames(12), rNumericArgs(12)));
                }

                if (rNumericArgs(13) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(13) + " has been found.");
                    ShowContinueError(state, format("{} must be >0, entered value = {:.2R}", cNumericFieldNames(13), rNumericArgs(13)));
                }

                if ((rNumericArgs(14) < -90.0) || (rNumericArgs(14) > 90.0)) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(14) + " has been found.");
                    ShowContinueError(state, format("{} must be >=-90 and <=90, entered value = {:.2R}", cNumericFieldNames(14), rNumericArgs(14)));
                }

                if (rNumericArgs(15) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(15) + " has been found.");
                    ShowContinueError(state, format("{} must be >0, entered value = {:.2R}", cNumericFieldNames(15), rNumericArgs(15)));
                }

                if ((rNumericArgs(16) < 0.0) || ((rNumericArgs(16) > 0.0) && (rNumericArgs(16) < (rNumericArgs(11) / 2)))) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(16) + " has been found.");
                    ShowContinueError(
                        state, format("{} must be =0 or greater than SlatWidth/2, entered value = {:.2R}", cNumericFieldNames(16), rNumericArgs(16)));
                }
            }

            if (ErrorsFound) ShowFatalError(state, "Error in complex fenestration material input.");
        }
    }

    void SetupComplexFenestrationStateInput(EnergyPlusData &state,
                                            int &ConstrNum, // num of construction items thus far
                                            bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2010
        //       MODIFIED       January 2012 (Simon Vidanovic)
        //       MODIFIED       May 2012 (Simon Vidanovic)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get input for complex fenestration construction

        // METHODOLOGY EMPLOYED:
        // usual GetInput processing.  Matrix input from MatrixDataManager

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace MatrixDataManager;
        using namespace DataBSDFWindow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SetupComlexFenestrationStateInput: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // The following moved to DataBSDFWindow module:
        // INTEGER :: TotComplexFenStates   ! Number of complex fenestration construction definitions
        int I;          // do loop index
        int Loop;       // do loop counter
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int TotalArgs;  // Number of fields for each GetObjectItem call
        int IOStatus;   // Used in GetObjectItem
        int iMatGlass;  // number of glass layers
        int NumRows;    // temporary size of matrix
        int NumCols;    // temporary size of matrix
        int NBasis;     // temporary number of elements in basis
        int Layer;      // loop counter for material layers
        int AlphaIndex;
        int ThermalModelNum;     // number of thermal model parameters object
        int NumOfTotalLayers;    // total number of layers in the construction
        int NumOfOpticalLayers;  // number of optical layers in the construction (excluding gasses and gas mixtures)
        int currentOpticalLayer; // current optical layer number.  This is important since optical structures should
        // be loaded only with optical layers

        // When reading Construction:ComplexFenestrationState, there is a call of GetMatrix2D which also uses same
        // variables from DataIPShortCuts.  Since this can cause some errors in reading, it is important
        // to declare local variables for reading Construction:ComplexFenestrationState object(s)
        Array1D_string locAlphaFieldNames;
        Array1D_string locNumericFieldNames;
        Array1D_bool locNumericFieldBlanks;
        Array1D_bool locAlphaFieldBlanks;
        Array1D_string locAlphaArgs;
        Array1D<Real64> locNumericArgs;
        std::string locCurrentModuleObject;

        // Reading WindowThermalModel:Params
        cCurrentModuleObject = "WindowThermalModel:Params";
        state.dataBSDFWindow->TotThermalModels = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        WindowThermalModel.allocate(state.dataBSDFWindow->TotThermalModels);

        for (Loop = 1; Loop <= state.dataBSDFWindow->TotThermalModels; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) continue;

            WindowThermalModel(Loop).Name = cAlphaArgs(1);

            WindowThermalModel(Loop).SDScalar = rNumericArgs(1);
            if ((rNumericArgs(1) < 0.0) || (rNumericArgs(1) > 1.0)) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " + cNumericFieldNames(1) +
                                    " has been found.");
                ShowContinueError(state, format("{} should be >= 0.0 and <= 1.0, entered value = {:.2R}", cNumericFieldNames(1), rNumericArgs(1)));
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(2));
                if (SELECT_CASE_var == "ISO15099") {
                    WindowThermalModel(Loop).CalculationStandard = csISO15099;
                } else if (SELECT_CASE_var == "EN673DECLARED") {
                    WindowThermalModel(Loop).CalculationStandard = csEN673Declared;
                } else if (SELECT_CASE_var == "EN673DESIGN") {
                    WindowThermalModel(Loop).CalculationStandard = csEN673Design;
                } else {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(
                        state, cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) + "\" should be ISO15099, EN673Declared or EN673Design.");
                }
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(3));
                if (SELECT_CASE_var == "ISO15099") {
                    WindowThermalModel(Loop).ThermalModel = tmISO15099;
                } else if (SELECT_CASE_var == "SCALEDCAVITYWIDTH") {
                    WindowThermalModel(Loop).ThermalModel = tmScaledCavityWidth;
                } else if (SELECT_CASE_var == "CONVECTIVESCALARMODEL_NOSDTHICKNESS") {
                    WindowThermalModel(Loop).ThermalModel = tmConvectiveScalarModel_NoSDThickness;
                } else if (SELECT_CASE_var == "CONVECTIVESCALARMODEL_WITHSDTHICKNESS") {
                    WindowThermalModel(Loop).ThermalModel = tmConvectiveScalarModel_WithSDThickness;
                } else {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
                                          "\" should be ISO15099, ScaledCavityWidth, ConvectiveScalarModel_NoSDThickness or "
                                          "ConvectiveScalarModel_WithSDThickness.");
                }
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(4));
                if (SELECT_CASE_var == "NODEFLECTION") {
                    WindowThermalModel(Loop).DeflectionModel = dmNoDeflection;
                } else if (SELECT_CASE_var == "TEMPERATUREANDPRESSUREINPUT") {
                    WindowThermalModel(Loop).DeflectionModel = dmTemperatureAndPressureInput;
                } else if (SELECT_CASE_var == "MEASUREDDEFLECTION") {
                    WindowThermalModel(Loop).DeflectionModel = dmMeasuredDeflection;
                } else {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(4) + " has been found.");
                    ShowContinueError(state,
                                      cAlphaFieldNames(4) + " entered value = \"" + cAlphaArgs(4) +
                                          "\" should be NoDeflection, TemperatureAndPressureInput or MeasuredDeflection.");
                }
            }

            if (WindowThermalModel(Loop).DeflectionModel == dmTemperatureAndPressureInput) {
                WindowThermalModel(Loop).VacuumPressureLimit = rNumericArgs(2);
                if (rNumericArgs(2) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(2) + " has been found.");
                    ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(2), rNumericArgs(2)));
                }

                WindowThermalModel(Loop).InitialTemperature = rNumericArgs(3);
                if (rNumericArgs(3) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(3) + " has been found.");
                    ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(3), rNumericArgs(3)));
                }

                WindowThermalModel(Loop).InitialPressure = rNumericArgs(4);
                if (rNumericArgs(4) <= 0.0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cNumericFieldNames(4) + " has been found.");
                    ShowContinueError(state, format("{} must be > 0, entered value = {:.2R}", cNumericFieldNames(4), rNumericArgs(4)));
                }
            }

        } // DO Loop = 1, TotThermalModels

        // Reading Construction:ComplexFenestrationState
        locCurrentModuleObject = "Construction:ComplexFenestrationState";
        state.dataBSDFWindow->TotComplexFenStates = inputProcessor->getNumObjectsFound(state, locCurrentModuleObject);

        inputProcessor->getObjectDefMaxArgs(state, locCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
        if (!allocated(locAlphaFieldNames)) locAlphaFieldNames.allocate(NumAlphas);
        if (!allocated(locNumericFieldNames)) locNumericFieldNames.allocate(NumNumbers);
        if (!allocated(locNumericFieldBlanks)) locNumericFieldBlanks.allocate(NumNumbers);
        if (!allocated(locAlphaFieldBlanks)) locAlphaFieldBlanks.allocate(NumAlphas);
        if (!allocated(locAlphaArgs)) locAlphaArgs.allocate(NumAlphas);
        if (!allocated(locNumericArgs)) locNumericArgs.allocate(NumNumbers);

        state.dataBSDFWindow->FirstBSDF = ConstrNum + 1; // Location of first BSDF construction input (They will be consecutive)
        for (Loop = 1; Loop <= state.dataBSDFWindow->TotComplexFenStates; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          locCurrentModuleObject,
                                          Loop,
                                          locAlphaArgs,
                                          NumAlphas,
                                          locNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          locNumericFieldBlanks,
                                          _,
                                          locAlphaFieldNames,
                                          locNumericFieldNames);
            if (GlobalNames::VerifyUniqueInterObjectName(
                    state, UniqueConstructNames, locAlphaArgs(1), CurrentModuleObject, locAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }
            ++ConstrNum;
            // Glass layer counter
            iMatGlass = 0;
            // Simon TODO: This is to be confirmed.  If this is just initial value, then we might want to make better guess
            NominalRforNominalUCalculation(ConstrNum) = 0.1;
            // Simon TODO: If I do not put this, then it is considered that surface is NOT window
            state.dataConstruction->Construct(ConstrNum).TransDiff = 0.1; // This is a place holder to flag
            // the construction as a window until
            // the correct value is entered in WindowComplexManager

            // Now override the deraults as appropriate
            state.dataConstruction->Construct(ConstrNum).Name = locAlphaArgs(1);

            //    ALLOCATE(Construct(ConstrNum)%BSDFInput)

            // Construct(ConstrNum)%BSDFInput%ThermalConstruction = ThConstNum

            {
                auto const SELECT_CASE_var(locAlphaArgs(2)); // Basis Type Keyword
                if (SELECT_CASE_var == "LBNLWINDOW") {
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisType = DataBSDFWindow::BasisType_WINDOW;
                } else if (SELECT_CASE_var == "USERDEFINED") {
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisType = DataBSDFWindow::BasisType_Custom;
                } else {
                    // throw error
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal value for " +
                                        locAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state,
                                      locAlphaFieldNames(2) + " entered value=\"" + locAlphaArgs(2) + "\" should be LBNLWindow or UserDefined.");
                }
            }

            {
                auto const SELECT_CASE_var(locAlphaArgs(3)); // Basis Symmetry Keyword
                if (SELECT_CASE_var == "AXISYMMETRIC") {
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisSymmetryType = DataBSDFWindow::BasisSymmetry_Axisymmetric;
                } else if (SELECT_CASE_var == "NONE") {
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisSymmetryType = DataBSDFWindow::BasisSymmetry_None;
                } else {
                    // throw error
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal value for " +
                                        locAlphaFieldNames(3) + " has been found.");
                    ShowContinueError(state, locAlphaFieldNames(3) + " entered value = \"" + locAlphaArgs(3) + "\" should be Axisymmetric or None.");
                }
            }

            // Simon: Assign thermal model number
            ThermalModelNum = UtilityRoutines::FindItemInList(locAlphaArgs(4), WindowThermalModel);
            if (ThermalModelNum == 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal value for " +
                                    locAlphaFieldNames(4) + " has been found.");
                ShowContinueError(state,
                                  locAlphaFieldNames(4) + " entered value = \"" + locAlphaArgs(4) +
                                      "\" no corresponding thermal model (WindowThermalModel:Params) found in the input file.");
            } else {
                state.dataConstruction->Construct(ConstrNum).BSDFInput.ThermalModel = ThermalModelNum;
            }

            // ***************************************************************************************
            // Basis matrix
            // ***************************************************************************************
            state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMatIndex = MatrixIndex(state, locAlphaArgs(5));
            Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMatIndex, NumRows, NumCols);
            state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMatNrows = NumRows;
            state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMatNcols = NumCols;

            if (NumCols != 2 && NumCols != 1) {
                ErrorsFound = true;
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal value for " +
                                    locAlphaFieldNames(5) + " has been found.");
                ShowContinueError(state,
                                  locAlphaFieldNames(5) + " entered value=\"" + locAlphaArgs(5) +
                                      "\" invalid matrix dimensions.  Basis matrix dimension can only be 2 x 1.");
            }
            state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMat.allocate(NumCols, NumRows);
            Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMatIndex,
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisMat);
            if (state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisType == DataBSDFWindow::BasisType_WINDOW)
                CalculateBasisLength(state,
                                     state.dataConstruction->Construct(ConstrNum).BSDFInput,
                                     ConstrNum,
                                     state.dataConstruction->Construct(ConstrNum).BSDFInput.NBasis);

            // determine number of layers and optical layers
            NumOfTotalLayers = (NumAlphas - 9) / 3;
            state.dataConstruction->Construct(ConstrNum).TotLayers = NumOfTotalLayers;

            NumOfOpticalLayers = NumOfTotalLayers / 2 + 1;

            state.dataConstruction->Construct(ConstrNum).BSDFInput.NumLayers = NumOfOpticalLayers;
            state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer.allocate(NumOfOpticalLayers);

            // check for incomplete field set
            if (mod((NumAlphas - 9), 3) != 0) {
                // throw warning if incomplete field set
                ErrorsFound = true;
                ShowSevereError(state, RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Incomplete field set found.");
                ShowContinueError(state, locAlphaArgs(1) + " is missing some of the layers or/and gaps.");
            }

            if (state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisSymmetryType == DataBSDFWindow::BasisSymmetry_None) {
                // Non-Symmetric basis

                NBasis = state.dataConstruction->Construct(ConstrNum).BSDFInput.NBasis;

                // *******************************************************************************
                // Solar front transmittance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex = MatrixIndex(state, locAlphaArgs(6));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransNrows = NumRows;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransNcols = NumCols;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Solar front transmittance matrix \"" + locAlphaArgs(6) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state,
                                      "Solar front transmittance matrix \"" + locAlphaArgs(6) + "\" must have the same number of rows and columns.");
                }

                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.BasisType == DataBSDFWindow::BasisType_Custom) {
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.NBasis = NumRows; // For custom basis, no rows in transmittance
                                                                                             // matrix defines the basis length
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTrans.allocate(NumCols, NumRows);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Solar front transmittance Matrix:TwoDimension = \"" + locAlphaArgs(6) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex,
                                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTrans);
                }

                // *******************************************************************************
                // Solar back reflectance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex = MatrixIndex(state, locAlphaArgs(7));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflNrows = NumRows;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflNcols = NumCols;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Solar back reflectance matrix \"" + locAlphaArgs(7) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state,
                                      "Solar bakc reflectance matrix \"" + locAlphaArgs(7) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkRefl.allocate(NumCols, NumRows);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Solar back reflectance Matrix:TwoDimension = \"" + locAlphaArgs(7) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex,
                                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkRefl);
                }

                // *******************************************************************************
                // Visible front transmittance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex = MatrixIndex(state, locAlphaArgs(8));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransNrows = NumRows;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransNcols = NumCols;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Visible front transmittance matrix \"" + locAlphaArgs(8) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(
                        state, "Visible front transmittance matrix \"" + locAlphaArgs(8) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTrans.allocate(NumCols, NumRows);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(
                        state, "Visible front transmittance Matrix:TwoDimension = \"" + locAlphaArgs(8) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex,
                                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTrans);
                }

                // *******************************************************************************
                // Visible back reflectance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex = MatrixIndex(state, locAlphaArgs(9));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflNrows = NumRows;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflNcols = NumCols;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Visible back reflectance matrix \"" + locAlphaArgs(9) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state, "Visible back reflectance \"" + locAlphaArgs(9) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkRefl.allocate(NumCols, NumRows);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Visble back reflectance Matrix:TwoDimension = \"" + locAlphaArgs(9) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex,
                                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkRefl);
                }

                // ALLOCATE(Construct(ConstrNum)%BSDFInput%Layer(NumOfOpticalLayers))
                for (Layer = 1; Layer <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Layer) {
                    AlphaIndex = 9 + (Layer * 3) - 2;
                    currentOpticalLayer = int(Layer / 2) + 1;
                    // Material info is contained in the thermal construct
                    state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) =
                        UtilityRoutines::FindItemInList(locAlphaArgs(AlphaIndex), state.dataMaterial->Material);

                    // Simon: Load only if optical layer
                    if (mod(Layer, 2) != 0) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).MaterialIndex =
                            state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);

                        ++AlphaIndex;
                        // *******************************************************************************
                        // Front absorptance matrix
                        // *******************************************************************************
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex =
                            MatrixIndex(state, locAlphaArgs(AlphaIndex));
                        Get2DMatrixDimensions(
                            state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex, NumRows, NumCols);

                        if (NumRows != 1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have only one row.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        }

                        if (NumCols != NBasis) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have same number of columns "
                                                     "as it is defined by basis matrix.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                            ShowContinueError(
                                state,
                                format("Matrix has {} number of columns, while basis definition specifies {} number of columns.", NumCols, NBasis));
                        }

                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).AbsNcols = NumCols;
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbs.allocate(NumCols, NumRows);
                        if (state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex == 0) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                                ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} is missing from the input file.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        } else {
                            Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex,
                                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbs);
                        }

                        ++AlphaIndex;
                        // *******************************************************************************
                        // Back absorptance matrix
                        // *******************************************************************************
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex =
                            MatrixIndex(state, locAlphaArgs(AlphaIndex));
                        Get2DMatrixDimensions(
                            state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex, NumRows, NumCols);

                        if (NumRows != 1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have only one row.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        }

                        if (NumCols != NBasis) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have same number of columns as "
                                                     "it is defined by basis matrix.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                            ShowContinueError(
                                state,
                                format("Matrix has {} number of columns, while basis definition specifies {} number of columns.", NumCols, NBasis));
                        }

                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbs.allocate(NumCols, NumRows);
                        if (state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex == 0) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                                ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} is missing from the input file.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        } else {
                            Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex,
                                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbs);
                        }
                    } // if (Mod(Layer, 2) <> 0) then
                }
            } else {
                // Axisymmetric basis
                NBasis = state.dataConstruction->Construct(ConstrNum).BSDFInput.NBasis; // Basis length has already been calculated
                state.dataBSDFWindow->BSDFTempMtrx.allocate(NBasis, 1);

                // *******************************************************************************
                // Solar front transmittance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex = MatrixIndex(state, locAlphaArgs(6));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransNrows = NBasis;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransNcols = NBasis;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Solar front transmittance matrix \"" + locAlphaArgs(6) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state,
                                      "Solar front transmittance matrix \"" + locAlphaArgs(6) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTrans.allocate(NBasis, NBasis);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Solar front transmittance Matrix:TwoDimension = \"" + locAlphaArgs(6) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTransIndex, state.dataBSDFWindow->BSDFTempMtrx);

                    state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTrans = 0.0;
                    for (I = 1; I <= NBasis; ++I) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.SolFrtTrans(I, I) = state.dataBSDFWindow->BSDFTempMtrx(I, 1);
                    }
                }

                // *******************************************************************************
                // Solar back reflectance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex = MatrixIndex(state, locAlphaArgs(7));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflNrows = NBasis;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflNcols = NBasis;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Solar back reflectance matrix \"" + locAlphaArgs(7) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state,
                                      "Solar back reflectance matrix \"" + locAlphaArgs(7) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkRefl.allocate(NBasis, NBasis);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Solar back reflectance Matrix:TwoDimension = \"" + locAlphaArgs(7) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkReflIndex, state.dataBSDFWindow->BSDFTempMtrx);
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkRefl = 0.0;
                    for (I = 1; I <= NBasis; ++I) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.SolBkRefl(I, I) = state.dataBSDFWindow->BSDFTempMtrx(I, 1);
                    }
                }

                // *******************************************************************************
                // Visible front transmittance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex = MatrixIndex(state, locAlphaArgs(8));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransNrows = NBasis;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransNcols = NBasis;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Visible front transmittance matrix \"" + locAlphaArgs(8) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(
                        state, "Visible front transmittance matrix \"" + locAlphaArgs(8) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTrans.allocate(NBasis, NBasis);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(
                        state, "Visible front transmittance Matrix:TwoDimension = \"" + locAlphaArgs(8) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTransIndex, state.dataBSDFWindow->BSDFTempMtrx);
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTrans = 0.0;
                    for (I = 1; I <= NBasis; ++I) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.VisFrtTrans(I, I) = state.dataBSDFWindow->BSDFTempMtrx(I, 1);
                    }
                }

                // *******************************************************************************
                // Visible back reflectance
                // *******************************************************************************
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex = MatrixIndex(state, locAlphaArgs(9));
                Get2DMatrixDimensions(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex, NumRows, NumCols);
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflNrows = NBasis;
                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflNcols = NBasis;

                if (NumRows != NBasis) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + ", object. Illegal matrix size has been found.");
                    ShowContinueError(
                        state,
                        "Visible back reflectance matrix \"" + locAlphaArgs(9) +
                            "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" +
                            locAlphaArgs(5) + "\".");
                }

                if (NumRows != NumCols) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) + "\", object. Invalid BSDF matrix dimensions.");
                    ShowContinueError(state,
                                      "Visible back reflectance matrix \"" + locAlphaArgs(9) + "\" must have the same number of rows and columns.");
                }

                state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkRefl.allocate(NBasis, NBasis);
                if (state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                        ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                    ShowContinueError(state,
                                      "Visible back reflectance Matrix:TwoDimension = \"" + locAlphaArgs(9) + "\" is missing from the input file.");
                } else {
                    Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkReflIndex, state.dataBSDFWindow->BSDFTempMtrx);
                    state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkRefl = 0.0;
                    for (I = 1; I <= NBasis; ++I) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.VisBkRefl(I, I) = state.dataBSDFWindow->BSDFTempMtrx(I, 1);
                    }
                }

                // determine number of layers
                // Construct(ConstrNum)%TotLayers = (NumAlphas - 9)/3

                // check for incomplete field set
                // IF (Mod((NumAlphas - 9), 3) /= 0) Then
                // throw warning if incomplete field set
                //  CALL ShowWarningError(state, 'Construction:ComplexFenestrationState: Axisymmetric properties have incomplete field &
                //   & set')
                // ENDIF

                // ALLOCATE(Construct(ConstrNum)%BSDFInput%Layer(NumOfOpticalLayers))
                for (Layer = 1; Layer <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Layer) {
                    AlphaIndex = 9 + (Layer * 3) - 2;
                    currentOpticalLayer = int(Layer / 2) + 1;

                    state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer) =
                        UtilityRoutines::FindItemInList(locAlphaArgs(AlphaIndex), state.dataMaterial->Material);

                    if (mod(Layer, 2) != 0) {
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).MaterialIndex =
                            state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);

                        // *******************************************************************************
                        // Front absorptance matrix
                        // *******************************************************************************
                        ++AlphaIndex;
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex =
                            MatrixIndex(state, locAlphaArgs(AlphaIndex));
                        Get2DMatrixDimensions(
                            state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex, NumRows, NumCols);

                        if (NumRows != 1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have only one row.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        }

                        if (NumCols != NBasis) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have same number of columns "
                                                     "as it is defined by basis matrix.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                            ShowContinueError(
                                state,
                                format("Matrix has {} number of columns, while basis definition specifies {} number of columns.", NumCols, NBasis));
                        }

                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).AbsNcols = NumCols;
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbs.allocate(NumCols, NumRows);

                        if (state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex == 0) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                                ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                            ShowContinueError(state,
                                              format("Front absorbtance Matrix:TwoDimension = \"{}\" for layer {} is missing from the input file.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        } else {
                            Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbsIndex,
                                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).FrtAbs);
                        }

                        // *******************************************************************************
                        // Back absorptance matrix
                        // *******************************************************************************
                        ++AlphaIndex;
                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex =
                            MatrixIndex(state, locAlphaArgs(AlphaIndex));
                        Get2DMatrixDimensions(
                            state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex, NumRows, NumCols);

                        if (NumRows != 1) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have only one row.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        }

                        if (NumCols != NBasis) {
                            ErrorsFound = true;
                            ShowSevereError(
                                state, RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs(1) + "\", object. Incorrect matrix dimension.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} must have same number of columns as "
                                                     "it is defined by basis matrix.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                            ShowContinueError(
                                state,
                                format("Matrix has {} number of columns, while basis definition specifies {} number of columns.", NumCols, NBasis));
                        }

                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbs.allocate(NumCols, NumRows);

                        if (state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex == 0) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs(1) +
                                                ", object. Referenced Matrix:TwoDimension is missing from the input file.");
                            ShowContinueError(state,
                                              format("Back absorbtance Matrix:TwoDimension = \"{}\" for layer {} is missing from the input file.",
                                                     locAlphaArgs(AlphaIndex),
                                                     currentOpticalLayer));
                        } else {
                            Get2DMatrix(state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbsIndex,
                                        state.dataConstruction->Construct(ConstrNum).BSDFInput.Layer(currentOpticalLayer).BkAbs);
                        }
                    } // if (Mod(Layer, 2) <> 0) then
                }

                state.dataBSDFWindow->BSDFTempMtrx.deallocate();
            }
            state.dataConstruction->Construct(ConstrNum).TypeIsWindow = true;
            state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF = true;
        }

        // Do not forget to deallocate localy allocated variables
        if (allocated(locAlphaFieldNames)) locAlphaFieldNames.deallocate();
        if (allocated(locNumericFieldNames)) locNumericFieldNames.deallocate();
        if (allocated(locNumericFieldBlanks)) locNumericFieldBlanks.deallocate();
        if (allocated(locAlphaFieldBlanks)) locAlphaFieldBlanks.deallocate();
        if (allocated(locAlphaArgs)) locAlphaArgs.deallocate();
        if (allocated(locNumericArgs)) locNumericArgs.deallocate();

        if (ErrorsFound) ShowFatalError(state, "Error in complex fenestration input.");
    }

    void InitConductionTransferFunctions(EnergyPlusData &state)
    {
        bool ErrorsFound(false); // Flag for input error condition
        bool DoCTFErrorReport(false);
        for (auto &construction : state.dataConstruction->Construct) {
            construction.calculateTransferFunction(state, ErrorsFound, DoCTFErrorReport);
        }

        bool InitCTFDoReport;
        General::ScanForReports(state, "Constructions", InitCTFDoReport, "Constructions");
        if (InitCTFDoReport || DoCTFErrorReport) {
            print(state.files.eio,
                  "! <Construction CTF>,Construction Name,Index,#Layers,#CTFs,Time Step {{hours}},ThermalConductance "
                  "{{w/m2-K}},OuterThermalAbsorptance,InnerThermalAbsorptance,OuterSolarAbsorptance,InnerSolarAbsorptance,Roughness\n");
            print(state.files.eio,
                  "! <Material CTF Summary>,Material Name,Thickness {{m}},Conductivity {{w/m-K}},Density {{kg/m3}},Specific Heat "
                  "{{J/kg-K}},ThermalResistance {{m2-K/w}}\n");
            print(state.files.eio, "! <Material:Air>,Material Name,ThermalResistance {{m2-K/w}}\n");
            print(state.files.eio, "! <CTF>,Time,Outside,Cross,Inside,Flux (except final one)\n");

            int cCounter = 0; // just used to keep construction index in output report
            for (auto &construction : state.dataConstruction->Construct) {
                cCounter++;
                if (!construction.IsUsedCTF) continue;
                construction.reportTransferFunction(state, cCounter);
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Program terminated for reasons listed (InitConductionTransferFunctions)");
        }
    }

} // namespace HeatBalanceManager

} // namespace EnergyPlus
