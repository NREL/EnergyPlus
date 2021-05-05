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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ThermalChimney.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ThermalChimney {
    // Module containing the data for Thermal Chimney system

    // MODULE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   April 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithyms required to manage the ThermalChimney System Component

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // 1. N. K. Bansal, R. Mathur and M. S. Bhandari, "Solar Chimney for Enhanced Stack Ventilation",
    // Building and Environment, 28, pp. 373-377, 1993
    // 2. K. S. Ong, "A Mathematical Model of a Solar Chimney", Renewable Energy, 28, pp. 1047-1060, 2003
    // 3. J. Marti-Herrero and M. R. Heras-Celemin, "Dynamic Physical Model for a Solar Chimney",
    // Solar Energy, 81, pp. 614-622, 2007

    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using namespace DataHeatBalSurface;

    // Use statements for access to subroutines in other modules
    using namespace Psychrometrics;

    void ManageThermalChimney(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   April 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the simulation of ThermalChimney unit.
        // This driver manages the calls to all of
        // the other drivers and simulation algorithms.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false);

        // Obtains and Allocates heat balance related parameters from input file
        if (state.dataThermalChimneys->ThermalChimneyGetInputFlag) {
            GetThermalChimney(state, ErrorsFound);
            state.dataThermalChimneys->ThermalChimneyGetInputFlag = false;
        }

        if (state.dataThermalChimneys->TotThermalChimney == 0) return;

        CalcThermalChimney(state);

        ReportThermalChimney(state);
    }

    void GetThermalChimney(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   April 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine obtains input data for ThermalChimney units and
        // stores it in the ThermalChimney data structure.

        // Using/Aliasing

        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const FlowFractionTolerance(0.0001); // Smallest deviation from unity for the sum of all fractions

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumber;
        Real64 AllRatiosSummed;
        int TCZoneNum;  // Thermal chimney zone counter
        int TCZoneNum1; // Thermal chimney zone counter
        int IOStat;
        int Loop;
        int Loop1;
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        // Following used for reporting
        state.dataThermalChimneys->ZnRptThermChim.allocate(state.dataGlobal->NumOfZones);

        cCurrentModuleObject = "ZoneThermalChimney";
        state.dataThermalChimneys->TotThermalChimney = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        state.dataThermalChimneys->ThermalChimneySys.allocate(state.dataThermalChimneys->TotThermalChimney);
        state.dataThermalChimneys->ThermalChimneyReport.allocate(state.dataThermalChimneys->TotThermalChimney);

        for (Loop = 1; Loop <= state.dataThermalChimneys->TotThermalChimney; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Loop,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                continue;
            }

            // First Alpha is Thermal Chimney Name
            state.dataThermalChimneys->ThermalChimneySys(Loop).Name = state.dataIPShortCut->cAlphaArgs(1);

            // Second Alpha is Zone Name
            state.dataThermalChimneys->ThermalChimneySys(Loop).RealZonePtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
            if (state.dataThermalChimneys->ThermalChimneySys(Loop).RealZonePtr == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid Zone");
                ShowContinueError(
                    state, "invalid - not found " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            } else if (!state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).RealZonePtr).HasWindow) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid Zone");
                ShowContinueError(state,
                                  "...invalid - no window(s) in " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ShowContinueError(state, "...thermal chimney zones must have window(s).");
                ErrorsFound = true;
            }
            state.dataThermalChimneys->ThermalChimneySys(Loop).RealZoneName = state.dataIPShortCut->cAlphaArgs(2);

            state.dataThermalChimneys->ThermalChimneySys(Loop).SchedName = state.dataIPShortCut->cAlphaArgs(3);
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                state.dataThermalChimneys->ThermalChimneySys(Loop).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataThermalChimneys->ThermalChimneySys(Loop).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                if (state.dataThermalChimneys->ThermalChimneySys(Loop).SchedPtr == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-not found " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) +
                                          "\".");
                    ErrorsFound = true;
                }
            }

            state.dataThermalChimneys->ThermalChimneySys(Loop).AbsorberWallWidth = state.dataIPShortCut->rNumericArgs(1);
            if (state.dataThermalChimneys->ThermalChimneySys(Loop).AbsorberWallWidth < 0.0) {
                ShowSevereError(state,
                                format("{}=\"{} invalid {} must be >= 0, entered value=[{:.2R}].",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(1),
                                       state.dataIPShortCut->rNumericArgs(1)));
                ErrorsFound = true;
            }

            state.dataThermalChimneys->ThermalChimneySys(Loop).AirOutletCrossArea = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataThermalChimneys->ThermalChimneySys(Loop).AirOutletCrossArea < 0.0) {
                ShowSevereError(state,
                                format("{}=\"{} invalid {} must be >= 0, entered value=[{:.2R}].",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(2),
                                       state.dataIPShortCut->rNumericArgs(2)));
                ErrorsFound = true;
            }

            state.dataThermalChimneys->ThermalChimneySys(Loop).DischargeCoeff = state.dataIPShortCut->rNumericArgs(3);
            if ((state.dataThermalChimneys->ThermalChimneySys(Loop).DischargeCoeff <= 0.0) ||
                (state.dataThermalChimneys->ThermalChimneySys(Loop).DischargeCoeff > 1.0)) {
                ShowSevereError(state,
                                format("{}=\"{} invalid {} must be > 0 and <=1.0, entered value=[{:.2R}].",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(3),
                                       state.dataIPShortCut->rNumericArgs(3)));
                ErrorsFound = true;
            }

            state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib = NumAlpha - 3;
            state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr.allocate(state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib);
            state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName.allocate(state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib);
            state.dataThermalChimneys->ThermalChimneySys(Loop).DistanceThermChimInlet.allocate(
                state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib);
            state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow.allocate(
                state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib);
            state.dataThermalChimneys->ThermalChimneySys(Loop).EachAirInletCrossArea.allocate(
                state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib);

            AllRatiosSummed = 0.0;
            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) = state.dataIPShortCut->cAlphaArgs(TCZoneNum + 3);
                state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(TCZoneNum + 3), state.dataHeatBal->Zone);
                state.dataThermalChimneys->ThermalChimneySys(Loop).DistanceThermChimInlet(TCZoneNum) =
                    state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 1);
                state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) =
                    state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 2);
                if (state.dataIPShortCut->lNumericFieldBlanks(3 * TCZoneNum + 2))
                    state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) = 1.0;
                state.dataThermalChimneys->ThermalChimneySys(Loop).EachAirInletCrossArea(TCZoneNum) =
                    state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 3);

                //!! Error trap for zones that do not exist or zones not in the zone the thermal chimney is in
                if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) == 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(TCZoneNum + 3) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(TCZoneNum + 3) + "\" not found.");
                    ErrorsFound = true;
                } else if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                           state.dataThermalChimneys->ThermalChimneySys(Loop).RealZonePtr) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid reference " +
                                        state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2));
                    ShowContinueError(state,
                                      "...must not have same zone as reference= " + state.dataIPShortCut->cAlphaFieldNames(TCZoneNum + 3) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(TCZoneNum + 3) + "\".");
                    ErrorsFound = true;
                }

                if (state.dataThermalChimneys->ThermalChimneySys(Loop).DistanceThermChimInlet(TCZoneNum) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{} invalid {} must be >= 0, entered value=[{:.2R}].",
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cNumericFieldNames(3 * TCZoneNum + 1),
                                           state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 1)));
                    ErrorsFound = true;
                }

                if ((state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) <= 0.0) ||
                    (state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) > 1.0)) {
                    ShowSevereError(state,
                                    format("{}=\"{} invalid {} must be > 0 and <=1.0, entered value=[{:.2R}].",
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cNumericFieldNames(3 * TCZoneNum + 2),
                                           state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 2)));
                    ErrorsFound = true;
                }

                if (state.dataThermalChimneys->ThermalChimneySys(Loop).EachAirInletCrossArea(TCZoneNum) < 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{} invalid {} must be >= 0, entered value=[{:.2R}].",
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cNumericFieldNames(3 * TCZoneNum + 3),
                                           state.dataIPShortCut->rNumericArgs(3 * TCZoneNum + 3)));
                    ErrorsFound = true;
                }

                AllRatiosSummed += state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum);

            } // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib

            // Error trap if the sum of fractions is not equal to 1.0
            if (std::abs(AllRatiosSummed - 1.0) > FlowFractionTolerance) {
                ShowSevereError(state,
                                format("{}=\"{} invalid sum of fractions, must be =1.0, entered value (summed from entries)=[{:.4R}].",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       AllRatiosSummed));
                ErrorsFound = true;
            }

        } // DO Loop=1, TotThermalChimney

        // check infiltration output
        // setup zone-level infiltration reports
        Array1D_bool RepVarSet;
        RepVarSet.dimension(state.dataGlobal->NumOfZones, true);
        for (Loop = 1; Loop <= state.dataHeatBal->TotInfiltration; ++Loop) {
            if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0 && !state.dataHeatBal->Infiltration(Loop).QuadratureSum) {
                RepVarSet(state.dataHeatBal->Infiltration(Loop).ZonePtr) = false;
            }
        }
        // Set up the output variables for thermal chimneys
        for (Loop = 1; Loop <= state.dataThermalChimneys->TotThermalChimney; ++Loop) {
            SetupOutputVariable(state,
                                "Zone Thermal Chimney Current Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlow,
                                "System",
                                "Average",
                                state.dataThermalChimneys->ThermalChimneySys(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermal Chimney Standard Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlowStd,
                                "System",
                                "Average",
                                state.dataThermalChimneys->ThermalChimneySys(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermal Chimney Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCMassFlow,
                                "System",
                                "Average",
                                state.dataThermalChimneys->ThermalChimneySys(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Thermal Chimney Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataThermalChimneys->ThermalChimneyReport(Loop).OutletAirTempThermalChim,
                                "System",
                                "Average",
                                state.dataThermalChimneys->ThermalChimneySys(Loop).Name);

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "Zone Thermal Chimney",
                                 state.dataThermalChimneys->ThermalChimneySys(Loop).Name,
                                 "Air Exchange Flow Rate",
                                 "[m3/s]",
                                 state.dataThermalChimneys->ThermalChimneySys(Loop).EMSOverrideOn,
                                 state.dataThermalChimneys->ThermalChimneySys(Loop).EMSAirFlowRateValue);
            }

            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                SetupOutputVariable(state,
                                    "Zone Thermal Chimney Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataThermalChimneys->ZnRptThermChim(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum))
                                        .ThermalChimneyHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Chimney Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataThermalChimneys->ZnRptThermChim(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum))
                                        .ThermalChimneyHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Chimney Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataThermalChimneys->ZnRptThermChim(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum))
                                        .ThermalChimneyVolume,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                SetupOutputVariable(state,
                                    "Zone Thermal Chimney Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataThermalChimneys->ZnRptThermChim(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum))
                                        .ThermalChimneyMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                if (RepVarSet(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum))) {
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Sensible Heat Loss Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilHeatLoss,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Sensible Heat Gain Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilHeatGain,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Latent Heat Loss Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilLatentLoss,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Latent Heat Gain Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilLatentGain,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Total Heat Loss Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilTotalLoss,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Total Heat Gain Energy",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilTotalGain,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Current Density Volume Flow Rate",
                        OutputProcessor::Unit::m3_s,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilVdotCurDensity,
                        "System",
                        "Average",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Standard Density Volume Flow Rate",
                        OutputProcessor::Unit::m3_s,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilVdotStdDensity,
                        "System",
                        "Average",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Current Density Volume",
                        OutputProcessor::Unit::m3,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilVolumeCurDensity,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Standard Density Volume",
                        OutputProcessor::Unit::m3,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilVolumeStdDensity,
                        "System",
                        "Sum",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(state,
                                        "Zone Infiltration Mass",
                                        OutputProcessor::Unit::kg,
                                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilMass,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(state,
                                        "Zone Infiltration Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilMdot,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    SetupOutputVariable(
                        state,
                        "Zone Infiltration Air Change Rate",
                        OutputProcessor::Unit::ach,
                        state.dataHeatBal->ZnAirRpt(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).InfilAirChangeRate,
                        "System",
                        "Average",
                        state.dataHeatBal->Zone(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)).Name);
                    RepVarSet(state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum)) = false;
                }
            } // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
        }     // DO Loop=1, TotThermalChimney

        //! LKL-more renaming effort and code review might be possible here
        // Check to make sure there is only one thermal chimney statement per zone
        for (Loop = 1; Loop <= state.dataThermalChimneys->TotThermalChimney; ++Loop) {
            if (state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib > 1) {
                for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {

                    if (state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib >= (TCZoneNum + 1)) {
                        for (TCZoneNum1 = TCZoneNum + 1; TCZoneNum1 <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib;
                             ++TCZoneNum1) {
                            if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum1)) {
                                ShowSevereError(state,
                                                "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                    state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                    " has two ZoneThermalChimney objects associated with it");
                                ErrorsFound = true;
                            }
                        }
                        for (TCZoneNum1 = 1; TCZoneNum1 <= TCZoneNum - 1; ++TCZoneNum1) {
                            if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum1)) {
                                ShowSevereError(state,
                                                "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                    state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                    " has two ZoneThermalChimney objects associated with it");
                                ErrorsFound = true;
                            }
                        }
                    } else { // IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN
                        for (TCZoneNum1 = 1; TCZoneNum1 <= TCZoneNum - 1; ++TCZoneNum1) {
                            if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum1)) {
                                ShowSevereError(state,
                                                "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                    state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                    " has two ZoneThermalChimney objects associated with it");
                                ErrorsFound = true;
                            }
                        }
                    } // IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN

                } // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
            }     // IF (ThermalChimneySys(Loop)%TotZoneToDistrib > 1) THEN
        }         // DO Loop = 1, TotThermalChimney

        // Check to make sure there is only one thermal chimney statement per zone
        if (state.dataThermalChimneys->TotThermalChimney > 1) {
            for (Loop = 1; Loop <= state.dataThermalChimneys->TotThermalChimney; ++Loop) {

                if (state.dataThermalChimneys->TotThermalChimney >= (Loop + 1)) {
                    for (Loop1 = Loop + 1; Loop1 <= state.dataThermalChimneys->TotThermalChimney; ++Loop1) {
                        for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                            for (TCZoneNum1 = 1; TCZoneNum1 <= state.dataThermalChimneys->ThermalChimneySys(Loop1).TotZoneToDistrib; ++TCZoneNum1) {
                                if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                    state.dataThermalChimneys->ThermalChimneySys(Loop1).ZonePtr(TCZoneNum1)) {
                                    ShowSevereError(state,
                                                    "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                        state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                        " has two ZoneThermalChimney objects associated with it");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                    for (Loop1 = 1; Loop1 <= Loop - 1; ++Loop1) {
                        for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                            for (TCZoneNum1 = 1; TCZoneNum1 <= state.dataThermalChimneys->ThermalChimneySys(Loop1).TotZoneToDistrib; ++TCZoneNum1) {
                                if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                    state.dataThermalChimneys->ThermalChimneySys(Loop1).ZonePtr(TCZoneNum1)) {
                                    ShowSevereError(state,
                                                    "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                        state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                        " has two ZoneThermalChimney objects associated with it");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                } else { // IF ( TotThermalChimney >= (Loop+1) ) THEN
                    for (Loop1 = 1; Loop1 <= Loop - 1; ++Loop1) {
                        for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                            for (TCZoneNum1 = 1; TCZoneNum1 <= state.dataThermalChimneys->ThermalChimneySys(Loop1).TotZoneToDistrib; ++TCZoneNum1) {
                                if (state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum) ==
                                    state.dataThermalChimneys->ThermalChimneySys(Loop1).ZonePtr(TCZoneNum1)) {
                                    ShowSevereError(state,
                                                    "Only one ZoneThermalChimney object allowed per zone but zone " +
                                                        state.dataThermalChimneys->ThermalChimneySys(Loop).ZoneName(TCZoneNum) +
                                                        " has two ZoneThermalChimney objects associated with it");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                } // IF ( TotThermalChimney >= (Loop+1) ) THEN

            } // DO Loop = 1, TotThermalChimney
        }     // IF (TotThermalChimney > 1) THEN

        if (ErrorsFound) {
            ShowFatalError(state, cCurrentModuleObject + " Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    void CalcThermalChimney(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   April 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the components making up the ThermalChimney.

        using ScheduleManager::GetCurrentScheduleValue;

        int const NTC(15); // Number of subregions in thermal chimney air channel for FINITE DIFFERENCE

        // To be obtained from other modules and subroutines
        Real64 SurfTempAbsorberWall;     // Absorber wall surface temperature (K)
        Real64 SurfTempGlassCover;       // Glass cover surface temperature (K)
        Real64 ConvTransCoeffWallFluid;  // Absorber wall convection trasnfer coefficient
        Real64 ConvTransCoeffGlassFluid; // Glass cover convection trasnfer coefficient

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // Real local vaiables
        int Loop;    // DO loop counter
        int SurfNum; // DO loop counter for surfaces
        int ZoneNum; // DO loop counter for zones
        int TCZoneNumCounter;
        int TCZoneNum;
        Real64 minorW; // width of enclosure (narrow dimension)
        Real64 majorW; // width of major surface
        Real64 TempmajorW;

        Real64 RoomAirTemp;
        Real64 AirSpecHeatThermalChim; // (J/kg*C) or (J/kg*K)
        Real64 AbsorberWallWidthTC;
        Real64 TCVolumeAirFlowRate; // (m^3/s)
        Real64 TCMassAirFlowRate;   // (kg/s)
        Real64 DischargeCoeffTC;
        Real64 AirOutletCrossAreaTC;
        Real64 AirInletCrossArea;
        Real64 AirRelativeCrossArea;
        // REAL(r64)                    :: OutletAirTempThermalChim
        Real64 OverallThermalChimLength;
        Real64 ThermChimTolerance;
        Array1D<Real64> TempTCMassAirFlowRate(10);   // Temporary Value of Thermal Chimney Mass Flow Rate ()
        Array1D<Real64> TempTCVolumeAirFlowRate(10); // Temporary Value of Thermal Chimney Volume Flow Rate ()
        int IterationLoop;
        Real64 Process1; // Temporary Variable Used in the Middle of the Calculation
        Real64 Process2; // Temporary Variable Used in the Middle of the Calculation
        Real64 Process3; // Temporary Variable Used in the Middle of the Calculation
        // unused1208  REAL(r64)   :: Process4                            ! Temporary Variable Used in the Middle of the Calculation
        Real64 AirDensityThermalChim; // (kg/m^3)
        Real64 AirDensity;            // (kg/m^3)
        Real64 CpAir;
        Real64 TemporaryWallSurfTemp;

        Real64 DeltaL; // OverallThermalChimLength / NTC
        int ThermChimLoop1;
        int ThermChimLoop2;
        Array2D<Real64> EquaCoef(NTC, NTC);    // Coefficients in Linear Algebraic Euqation for FINITE DIFFERENCE
        Array1D<Real64> EquaConst(NTC);        // Constants in Linear Algebraic Equation for FINITE DIFFERENCE
        Array1D<Real64> ThermChimSubTemp(NTC); // Air temperature of each thermal chimney air channel subregion

        for (Loop = 1; Loop <= state.dataThermalChimneys->TotThermalChimney; ++Loop) {

            ZoneNum = state.dataThermalChimneys->ThermalChimneySys(Loop).RealZonePtr;
            // start off with first surface in zone widths
            majorW = state.dataSurface->Surface(state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst).Width;
            minorW = majorW;
            TempmajorW = 0.0;
            TemporaryWallSurfTemp = -10000.0;

            // determine major width and minor width
            for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst + 1; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast;
                 ++SurfNum) {
                if (state.dataSurface->Surface(SurfNum).Class != SurfaceClass::Wall) continue;

                if (state.dataSurface->Surface(SurfNum).Width > majorW) {
                    majorW = state.dataSurface->Surface(SurfNum).Width;
                }

                if (state.dataSurface->Surface(SurfNum).Width < minorW) {
                    minorW = state.dataSurface->Surface(SurfNum).Width;
                }
            }

            for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
                if (state.dataSurface->Surface(SurfNum).Width == majorW) {
                    if (state.dataHeatBalSurf->TempSurfIn(SurfNum) > TemporaryWallSurfTemp) {
                        TemporaryWallSurfTemp = state.dataHeatBalSurf->TempSurfIn(SurfNum);
                        ConvTransCoeffWallFluid = state.dataHeatBal->HConvIn(SurfNum);
                        SurfTempAbsorberWall = state.dataHeatBalSurf->TempSurfIn(SurfNum) + DataGlobalConstants::KelvinConv;
                    }
                }
            }

            for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
                if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {
                    if (state.dataSurface->Surface(SurfNum).Width > TempmajorW) {
                        TempmajorW = state.dataSurface->Surface(SurfNum).Width;
                        ConvTransCoeffGlassFluid = state.dataHeatBal->HConvIn(SurfNum);
                        SurfTempGlassCover = state.dataHeatBalSurf->TempSurfIn(SurfNum) + DataGlobalConstants::KelvinConv;
                    }
                }
            }

            AbsorberWallWidthTC = majorW;
            if (state.dataThermalChimneys->ThermalChimneySys(Loop).AbsorberWallWidth != majorW) {
                AbsorberWallWidthTC = state.dataThermalChimneys->ThermalChimneySys(Loop).AbsorberWallWidth;
            }

            AirDensityThermalChim = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            AirSpecHeatThermalChim = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            AirOutletCrossAreaTC = state.dataThermalChimneys->ThermalChimneySys(Loop).AirOutletCrossArea;
            DischargeCoeffTC = state.dataThermalChimneys->ThermalChimneySys(Loop).DischargeCoeff;

            AirInletCrossArea = 0.0;
            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                AirInletCrossArea += state.dataThermalChimneys->ThermalChimneySys(Loop).EachAirInletCrossArea(TCZoneNum);
            }

            RoomAirTemp = 0.0;
            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                TCZoneNumCounter = state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum);
                RoomAirTemp += state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) *
                               state.dataHeatBalFanSys->MAT(TCZoneNumCounter);
            }
            RoomAirTemp += DataGlobalConstants::KelvinConv;

            Process1 = 0.0;
            Process2 = 0.0;
            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                TCZoneNumCounter = state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum);
                Process1 += PsyHFnTdbW(state.dataHeatBalFanSys->MAT(TCZoneNumCounter), state.dataHeatBalFanSys->ZoneAirHumRat(TCZoneNumCounter)) *
                            state.dataThermalChimneys->ThermalChimneySys(Loop).DistanceThermChimInlet(TCZoneNum) *
                            state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum);
                Process2 += state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum) *
                            PsyHFnTdbW(state.dataHeatBalFanSys->MAT(TCZoneNumCounter), state.dataHeatBalFanSys->ZoneAirHumRat(TCZoneNumCounter));
            }
            OverallThermalChimLength = Process1 / Process2;

            DeltaL = OverallThermalChimLength / NTC;

            // Starting the iteration for mass and volumetric flow rate calculation
            ThermChimTolerance = 10000000.0; // An impossibly big tolerance
            for (IterationLoop = 1; IterationLoop <= 10; ++IterationLoop) {

                if (IterationLoop == 1) {
                    TempTCMassAirFlowRate(IterationLoop) = 0.05; // Inital Guess

                } else {
                    TempTCMassAirFlowRate(IterationLoop) = TempTCVolumeAirFlowRate(IterationLoop - 1) * AirDensityThermalChim;

                    if (std::abs(TempTCMassAirFlowRate(IterationLoop) - TempTCMassAirFlowRate(IterationLoop - 1)) < ThermChimTolerance) {
                        ThermChimTolerance = std::abs(TempTCMassAirFlowRate(IterationLoop) - TempTCMassAirFlowRate(IterationLoop - 1));
                        TCMassAirFlowRate = TempTCMassAirFlowRate(IterationLoop);
                        TCVolumeAirFlowRate = TempTCVolumeAirFlowRate(IterationLoop);
                    }

                } // IF (IterationLoop == 1) THEN

                // Calculation of Thermal Chimney Discharge Air Temperature
                Process1 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid -
                           2.0 * TempTCMassAirFlowRate(IterationLoop) * AirSpecHeatThermalChim;
                Process2 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid +
                           2.0 * TempTCMassAirFlowRate(IterationLoop) * AirSpecHeatThermalChim;
                Process3 = 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid * SurfTempGlassCover +
                           2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid * SurfTempAbsorberWall;

                for (ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1) {
                    for (ThermChimLoop2 = 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2) {
                        EquaCoef(ThermChimLoop2, ThermChimLoop1) = 0.0;
                    }
                }

                EquaCoef(1, 1) = Process2;
                EquaConst(1) = Process3 - Process1 * RoomAirTemp;
                for (ThermChimLoop1 = 2; ThermChimLoop1 <= NTC; ++ThermChimLoop1) {
                    EquaCoef((ThermChimLoop1 - 1), ThermChimLoop1) = Process1;
                    EquaCoef(ThermChimLoop1, ThermChimLoop1) = Process2;
                    EquaConst(ThermChimLoop1) = Process3;
                }

                GaussElimination(EquaCoef, EquaConst, ThermChimSubTemp, NTC);

                AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea;
                if (ThermChimSubTemp(NTC) <= RoomAirTemp) {
                    TempTCVolumeAirFlowRate(IterationLoop) = 0.0;
                } else {
                    TempTCVolumeAirFlowRate(IterationLoop) = DischargeCoeffTC * AirOutletCrossAreaTC *
                                                             std::sqrt(2.0 * ((ThermChimSubTemp(NTC) - RoomAirTemp) / RoomAirTemp) * 9.8 *
                                                                       OverallThermalChimLength / pow_2(1.0 + AirRelativeCrossArea));
                }

            } // DO IterationLoop = 1,10

            // Calculation of Thermal Chimney Discharge Temperature
            Process1 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid -
                       2.0 * TCMassAirFlowRate * AirSpecHeatThermalChim;
            Process2 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid +
                       2.0 * TCMassAirFlowRate * AirSpecHeatThermalChim;
            Process3 = 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid * SurfTempGlassCover +
                       2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid * SurfTempAbsorberWall;

            for (ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1) {
                for (ThermChimLoop2 = 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2) {
                    EquaCoef(ThermChimLoop2, ThermChimLoop1) = 0.0;
                }
            }

            EquaCoef(1, 1) = Process2;
            EquaConst(1) = Process3 - Process1 * RoomAirTemp;
            for (ThermChimLoop1 = 2; ThermChimLoop1 <= NTC; ++ThermChimLoop1) {
                EquaCoef((ThermChimLoop1 - 1), ThermChimLoop1) = Process1;
                EquaCoef(ThermChimLoop1, ThermChimLoop1) = Process2;
                EquaConst(ThermChimLoop1) = Process3;
            }

            GaussElimination(EquaCoef, EquaConst, ThermChimSubTemp, NTC);

            AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea;
            if (ThermChimSubTemp(NTC) <= RoomAirTemp) {
                TCVolumeAirFlowRate = 0.0;
            } else {
                TCVolumeAirFlowRate = DischargeCoeffTC * AirOutletCrossAreaTC *
                                      std::sqrt(2.0 * ((ThermChimSubTemp(NTC) - RoomAirTemp) / RoomAirTemp) * 9.8 * OverallThermalChimLength /
                                                pow_2(1.0 + AirRelativeCrossArea));
                if (state.dataThermalChimneys->ThermalChimneySys(Loop).EMSOverrideOn) {
                    TCVolumeAirFlowRate = state.dataThermalChimneys->ThermalChimneySys(Loop).EMSAirFlowRateValue;
                }
            }

            // Now assignment of the overall mass flow rate into each zone
            for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                TCZoneNumCounter = state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum);
                AirDensity = PsyRhoAirFnPbTdbW(state,
                                               state.dataEnvrn->OutBaroPress,
                                               state.dataHeatBalFanSys->MAT(TCZoneNumCounter),
                                               state.dataHeatBalFanSys->ZoneAirHumRat(TCZoneNumCounter));
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(TCZoneNumCounter));
                state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) =
                    TCVolumeAirFlowRate * AirDensity * CpAir * state.dataThermalChimneys->ThermalChimneySys(Loop).RatioThermChimAirFlow(TCZoneNum);
                if (state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) <= 0.0) {
                    state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) = 0.0;
                }
                state.dataHeatBalFanSys->ThermChimAMFL(TCZoneNumCounter) = state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) / CpAir;
                state.dataHeatBalFanSys->MCPTThermChim(TCZoneNumCounter) =
                    state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) * state.dataHeatBal->Zone(TCZoneNumCounter).OutDryBulbTemp;
            }

            state.dataHeatBalFanSys->MCPThermChim(ZoneNum) = TCVolumeAirFlowRate * AirDensity * CpAir;
            if (state.dataHeatBalFanSys->MCPThermChim(ZoneNum) <= 0.0) {
                state.dataHeatBalFanSys->MCPThermChim(ZoneNum) = 0.0;
            }
            state.dataHeatBalFanSys->ThermChimAMFL(ZoneNum) = state.dataHeatBalFanSys->MCPThermChim(ZoneNum) / CpAir;
            state.dataHeatBalFanSys->MCPTThermChim(ZoneNum) =
                state.dataHeatBalFanSys->MCPThermChim(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;

            state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlow = TCVolumeAirFlowRate;
            state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCMassFlow = TCMassAirFlowRate;
            state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlowStd = TCMassAirFlowRate / state.dataEnvrn->StdRhoAir;
            if (state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCMassFlow != (TCVolumeAirFlowRate * AirDensityThermalChim)) {
                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCMassFlow =
                    state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlow * AirDensityThermalChim;
            }
            state.dataThermalChimneys->ThermalChimneyReport(Loop).OutletAirTempThermalChim = ThermChimSubTemp(NTC) - DataGlobalConstants::KelvinConv;

            if (GetCurrentScheduleValue(state, state.dataThermalChimneys->ThermalChimneySys(Loop).SchedPtr) <= 0.0) {
                for (TCZoneNum = 1; TCZoneNum <= state.dataThermalChimneys->ThermalChimneySys(Loop).TotZoneToDistrib; ++TCZoneNum) {
                    TCZoneNumCounter = state.dataThermalChimneys->ThermalChimneySys(Loop).ZonePtr(TCZoneNum);
                    state.dataHeatBalFanSys->MCPThermChim(TCZoneNumCounter) = 0.0;
                    state.dataHeatBalFanSys->ThermChimAMFL(TCZoneNumCounter) = 0.0;
                    state.dataHeatBalFanSys->MCPTThermChim(TCZoneNumCounter) = 0.0;
                }
                state.dataHeatBalFanSys->MCPThermChim(ZoneNum) = 0.0;
                state.dataHeatBalFanSys->ThermChimAMFL(ZoneNum) = 0.0;
                state.dataHeatBalFanSys->MCPTThermChim(ZoneNum) = 0.0;
                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlow = 0.0;
                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCVolumeFlowStd = 0.0;
                state.dataThermalChimneys->ThermalChimneyReport(Loop).OverallTCMassFlow = 0.0;
                state.dataThermalChimneys->ThermalChimneyReport(Loop).OutletAirTempThermalChim = state.dataHeatBalFanSys->MAT(ZoneNum);
            }

        } // DO Loop=1, TotThermalChimney
    }

    void ReportThermalChimney(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   April 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine fills remaining report variables.

        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        int ZoneLoop; // Counter for the # of zones (nz)
        Real64 AirDensity;
        Real64 CpAir;
        Real64 TSMult;

        TSMult = TimeStepSys * DataGlobalConstants::SecInHour;

        for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...

            // Break the infiltration load into heat gain and loss components.
            AirDensity = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneLoop), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop));
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop));
            state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyVolume =
                (state.dataHeatBalFanSys->MCPThermChim(ZoneLoop) / CpAir / AirDensity) * TSMult;
            state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyMass =
                (state.dataHeatBalFanSys->MCPThermChim(ZoneLoop) / CpAir) * TSMult;

            state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatLoss = 0.0;
            state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatGain = 0.0;

            if (state.dataHeatBalFanSys->ZT(ZoneLoop) > state.dataHeatBal->Zone(ZoneLoop).OutDryBulbTemp) {

                state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatLoss =
                    state.dataHeatBalFanSys->MCPThermChim(ZoneLoop) *
                    (state.dataHeatBalFanSys->ZT(ZoneLoop) - state.dataHeatBal->Zone(ZoneLoop).OutDryBulbTemp) * TSMult;
                state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatGain = 0.0;

            } else if (state.dataHeatBalFanSys->ZT(ZoneLoop) <= state.dataHeatBal->Zone(ZoneLoop).OutDryBulbTemp) {

                state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatGain =
                    state.dataHeatBalFanSys->MCPThermChim(ZoneLoop) *
                    (state.dataHeatBal->Zone(ZoneLoop).OutDryBulbTemp - state.dataHeatBalFanSys->ZT(ZoneLoop)) * TSMult;
                state.dataThermalChimneys->ZnRptThermChim(ZoneLoop).ThermalChimneyHeatLoss = 0.0;
            }

        } // ... end of zone loads report variable update loop.
    }

    void GaussElimination(Array2A<Real64> EquaCoef, Array1D<Real64> &EquaConst, Array1D<Real64> &ThermChimSubTemp, int const NTC)
    {
        // SUBROUTINE INFORMATION:

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sovles linear algebraic equations using Gauss Elimination Method.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Argument array dimensioning
        EquaCoef.dim(NTC, NTC);
        EP_SIZE_CHECK(EquaConst, NTC);
        EP_SIZE_CHECK(ThermChimSubTemp, NTC);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Array1D<Real64> tempor(NTC);
        Real64 tempb;
        Real64 TCvalue;
        Real64 TCcoefficient;
        int pivot;
        Real64 ThermalChimSum;
        int ThermChimLoop1;
        int ThermChimLoop2;
        int ThermChimLoop3;

        for (ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1) {

            TCvalue = std::abs(EquaCoef(ThermChimLoop1, ThermChimLoop1));
            pivot = ThermChimLoop1;
            for (ThermChimLoop2 = ThermChimLoop1 + 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2) {
                if (std::abs(EquaCoef(ThermChimLoop1, ThermChimLoop2)) > TCvalue) {
                    TCvalue = std::abs(EquaCoef(ThermChimLoop1, ThermChimLoop2));
                    pivot = ThermChimLoop2;
                }
            }

            if (pivot != ThermChimLoop1) {
                tempor({ThermChimLoop1, NTC}) = EquaCoef({ThermChimLoop1, NTC}, ThermChimLoop1);
                tempb = EquaConst(ThermChimLoop1);
                EquaCoef({ThermChimLoop1, NTC}, ThermChimLoop1) = EquaCoef({ThermChimLoop1, NTC}, pivot);
                EquaConst(ThermChimLoop1) = EquaConst(pivot);
                EquaCoef({ThermChimLoop1, NTC}, pivot) = tempor({ThermChimLoop1, NTC});
                EquaConst(pivot) = tempb;
            }

            for (ThermChimLoop2 = ThermChimLoop1 + 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2) {
                TCcoefficient = -EquaCoef(ThermChimLoop1, ThermChimLoop2) / EquaCoef(ThermChimLoop1, ThermChimLoop1);
                EquaCoef({ThermChimLoop1, NTC}, ThermChimLoop2) += TCcoefficient * EquaCoef({ThermChimLoop1, NTC}, ThermChimLoop1);
                EquaConst(ThermChimLoop2) += TCcoefficient * EquaConst(ThermChimLoop1);
            }
        }

        ThermChimSubTemp(NTC) = EquaConst(NTC) / EquaCoef(NTC, NTC);
        for (ThermChimLoop2 = NTC - 1; ThermChimLoop2 >= 1; --ThermChimLoop2) {
            ThermalChimSum = 0.0;
            for (ThermChimLoop3 = ThermChimLoop2 + 1; ThermChimLoop3 <= NTC; ++ThermChimLoop3) {
                ThermalChimSum += EquaCoef(ThermChimLoop3, ThermChimLoop2) * ThermChimSubTemp(ThermChimLoop3);
            }
            ThermChimSubTemp(ThermChimLoop2) = (EquaConst(ThermChimLoop2) - ThermalChimSum) / EquaCoef(ThermChimLoop2, ThermChimLoop2);
        }
    }

    //        End of Module Subroutines for ThermalChimney

    //*****************************************************************************************

} // namespace ThermalChimney

} // namespace EnergyPlus
