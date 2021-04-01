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
#include <EnergyPlus/CoolTower.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus {

namespace CoolTower {
    // Module containing the data for cooltower system

    // MODULE INFORMATION:
    //       AUTHOR         Daeho Kang
    //       DATE WRITTEN   Aug 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to manage the cooltower component.

    // REFERENCES:
    // Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
    //     John Wiley & Sons, Inc.
    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataHeatBalance;

    void ManageCoolTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aug 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the simulation of Cooltower component.
        // This driver manages the calls to all of the other drivers and simulation algorithms.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Obtains and allocates heat balance related parameters from input
        if (state.dataCoolTower->GetInputFlag) {
            GetCoolTower(state);
            state.dataCoolTower->GetInputFlag = false;
        }

        if (state.dataCoolTower->NumCoolTowers == 0) return;

        CalcCoolTower(state);

        UpdateCoolTower(state);

        ReportCoolTower(state);
    }

    void GetCoolTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aug 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets input data for cooltower components
        // and stores it in the Cooltower data structure.

        // Using/Aliasing

        using ScheduleManager::GetScheduleIndex;
        using WaterManager::SetupTankDemandComponent;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const CurrentModuleObject("ZoneCoolTower:Shower");
        Real64 const MaximumWaterFlowRate(0.016667); // Maximum limit of water flow rate in m3/s (1000 l/min)
        Real64 const MinimumWaterFlowRate(0.0);      // Minimum limit of water flow rate
        Real64 const MaxHeight(30.0);                // Maximum effective tower height in m
        Real64 const MinHeight(1.0);                 // Minimum effective tower height in m
        Real64 const MaxValue(100.0);                // Maximum limit of outlet area, airflow, and temperature
        Real64 const MinValue(0.0);                  // Minimum limit of outlet area, airflow, and temperature
        Real64 const MaxFrac(1.0);                   // Maximum fraction
        Real64 const MinFrac(0.0);                   // Minimum fraction

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // If errors detected in input
        int CoolTowerNum;        // Cooltower number
        int NumAlphas;           // Number of Alphas for each GetobjectItem call
        int NumNumbers;          // Number of Numbers for each GetobjectItem call
        int NumArgs;
        int IOStat;
        Array1D_string cAlphaArgs;     // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> rNumericArgs;  // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.

        // Initializations and allocations
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumArgs, NumAlphas, NumNumbers);
        cAlphaArgs.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        rNumericArgs.dimension(NumNumbers, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        auto &Zone(state.dataHeatBal->Zone);

        state.dataCoolTower->NumCoolTowers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        state.dataCoolTower->CoolTowerSys.allocate(state.dataCoolTower->NumCoolTowers);

        // Obtain inputs
        for (CoolTowerNum = 1; CoolTowerNum <= state.dataCoolTower->NumCoolTowers; ++CoolTowerNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CoolTowerNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), CurrentModuleObject, ErrorsFound);
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).Name = state.dataIPShortCut->cAlphaArgs(1);     // Name of cooltower
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).Schedule = state.dataIPShortCut->cAlphaArgs(2); // Get schedule
            if (lAlphaBlanks(2)) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).SchedPtr == 0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state, "Invalid-Schedule not found " + cAlphaFields(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZoneName = state.dataIPShortCut->cAlphaArgs(3); // Name of zone where cooltower is serving
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), Zone);
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr == 0) {
                if (lAlphaBlanks(3)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(3) +
                                        " is required but input is blank.");
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(3) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(3) + "\" not found.");
                }
                ErrorsFound = true;
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyName = state.dataIPShortCut->cAlphaArgs(4); // Name of water storage tank
            if (lAlphaBlanks(4)) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode = WaterSupplyMode::FromMains;
            } else if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode == WaterSupplyMode::FromTank) {
                SetupTankDemandComponent(state,
                                         state.dataCoolTower->CoolTowerSys(CoolTowerNum).Name,
                                         CurrentModuleObject,
                                         state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyName,
                                         ErrorsFound,
                                         state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupTankID,
                                         state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterTankDemandARRID);
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(5)); // Type of flow control
                if (SELECT_CASE_var == "WATERFLOWSCHEDULE") {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).FlowCtrlType = FlowCtrlEnum::FlowSchedule;
                } else if ((SELECT_CASE_var == "WINDDRIVENFLOW") || (SELECT_CASE_var == "NONE") || (SELECT_CASE_var.empty())) {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).FlowCtrlType = FlowCtrlEnum::WindDriven;
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(5) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpSchedPtr == 0) {
                if (lAlphaBlanks(6)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(6) +
                                        " is required but input is blank.");
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(6) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(6) + "\" not found.");
                }
                ErrorsFound = true;
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate = state.dataIPShortCut->rNumericArgs(1); // Maximum limit of water supply
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate > MaximumWaterFlowRate) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate = MaximumWaterFlowRate;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(1),
                                        state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaximumWaterFlowRate));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate < MinimumWaterFlowRate) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate = MinimumWaterFlowRate;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(1),
                                        state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinimumWaterFlowRate));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight = state.dataIPShortCut->rNumericArgs(2); // Get effctive tower height
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight > MaxHeight) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight = MaxHeight;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(2),
                                        state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxHeight));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight < MinHeight) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight = MinHeight;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(2),
                                        state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinHeight));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea = state.dataIPShortCut->rNumericArgs(3); // Get outlet area
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea > MaxValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea = MaxValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(3),
                                        state.dataIPShortCut->rNumericArgs(3)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxValue));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea < MinValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea = MinValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(3),
                                        state.dataIPShortCut->rNumericArgs(3)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinValue));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate =
                state.dataIPShortCut->rNumericArgs(4); // Maximum limit of air flow to the space
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate > MaxValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate = MaxValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(4),
                                        state.dataIPShortCut->rNumericArgs(4)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxValue));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate < MinValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate = MinValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(4),
                                        state.dataIPShortCut->rNumericArgs(4)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinValue));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp =
                state.dataIPShortCut->rNumericArgs(5); // Get minimum temp limit which gets this cooltower off
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp > MaxValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp = MaxValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(5),
                                        state.dataIPShortCut->rNumericArgs(5)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxValue));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp < MinValue) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp = MinValue;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(5),
                                        state.dataIPShortCut->rNumericArgs(5)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinValue));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss = state.dataIPShortCut->rNumericArgs(6); // Fraction of water loss
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss > MaxFrac) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss = MaxFrac;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(6),
                                        state.dataIPShortCut->rNumericArgs(6)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxFrac));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss < MinFrac) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss = MinFrac;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(6),
                                        state.dataIPShortCut->rNumericArgs(6)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinFrac));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched = state.dataIPShortCut->rNumericArgs(7); // Fraction of loss of air flow
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched > MaxFrac) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched = MaxFrac;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.2R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(7),
                                        state.dataIPShortCut->rNumericArgs(7)));
                ShowContinueError(state, format("...Maximum Allowable=[{:.2R}].", MaxFrac));
            }
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched < MinFrac) {
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched = MinFrac;
                ShowWarningError(state,
                                 format("{}=\"{}\" invalid {}=[{:.5R}].",
                                        CurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1),
                                        cNumericFields(7),
                                        state.dataIPShortCut->rNumericArgs(7)));
                ShowContinueError(state, format("...Minimum Allowable=[{:.2R}].", MinFrac));
            }

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).RatedPumpPower = state.dataIPShortCut->rNumericArgs(8); // Get rated pump power
        }

        cAlphaArgs.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        rNumericArgs.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) ShowFatalError(state, CurrentModuleObject + " errors occurred in input.  Program terminates.");

        for (CoolTowerNum = 1; CoolTowerNum <= state.dataCoolTower->NumCoolTowers; ++CoolTowerNum) {
            SetupOutputVariable(state,
                                "Zone Cooltower Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatLoss,
                                "System",
                                "Sum",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatPower,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatLoss,
                                "System",
                                "Sum",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatPower,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Volume",
                                OutputProcessor::Unit::m3,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTAirVol,
                                "System",
                                "Sum",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Current Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRate,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Standard Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRateStd,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Mass",
                                OutputProcessor::Unit::kg,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTAirMass,
                                "System",
                                "Sum",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirMassFlowRate,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletDBTemp,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Inlet Humidity Ratio",
                                OutputProcessor::Unit::kgWater_kgDryAir,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletHumRat,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletTemp,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Air Outlet Humidity Ratio",
                                OutputProcessor::Unit::kgWater_kgDryAir,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletHumRat,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecPower,
                                "System",
                                "Average",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Cooltower Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecConsump,
                                "System",
                                "Sum",
                                Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name,
                                _,
                                "Electricity",
                                "Cooling",
                                _,
                                "System");
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode == WaterSupplyMode::FromMains) {
                SetupOutputVariable(state,
                                    "Zone Cooltower Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsump,
                                    "System",
                                    "Sum",
                                    Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Cooltower Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsump,
                                    "System",
                                    "Sum",
                                    Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name,
                                    _,
                                    "MainsWater",
                                    "Cooling",
                                    _,
                                    "System");
            } else if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode == WaterSupplyMode::FromTank) {
                SetupOutputVariable(state,
                                    "Zone Cooltower Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsump,
                                    "System",
                                    "Sum",
                                    Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Cooltower Storage Tank Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsump,
                                    "System",
                                    "Sum",
                                    Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Cooltower Starved Mains Water Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeup,
                                    "System",
                                    "Sum",
                                    Zone(state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr).Name,
                                    _,
                                    "MainsWater",
                                    "Cooling",
                                    _,
                                    "System");
            }
        }
    }

    void CalcCoolTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aug 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // REFERENCES:
        // Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
        //     John Wiley & Sons, Inc.

        // Using/Aliasing
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdbH;
        using Psychrometrics::PsyWFnTdbTwbPb;
        using Psychrometrics::RhoH2O;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MinWindSpeed(0.1);  // Minimum limit of outdoor air wind speed in m/s
        Real64 const MaxWindSpeed(30.0); // Maximum limit of outdoor air wind speed in m/s
        Real64 const UCFactor(60000.0);  // Unit conversion factor m3/s to l/min

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;            // Number of zone being served
        int CoolTowerNum;       // Number of coolter being served
        Real64 CVF_ZoneNum;     // Design flow rate in m3/s
        Real64 AirMassFlowRate; // Actual air mass flow rate in kg/s
        Real64 AirSpecHeat;     // Specific heat of air
        Real64 AirDensity;      // Density of air
        Real64 RhoWater;        // Density of water
        Real64 PumpPartLoadRat; // Pump part load ratio (based on user schedule, or 1.0 for no schedule)
        Real64 WaterFlowRate;   // Calculated water flow rate in m3/s
        Real64 AirVolFlowRate;  // Calculated air volume flow rate in m3/s
        Real64 InletHumRat;     // Humidity ratio of outdoor air
        Real64 OutletHumRat;    // Humidity ratio of air at the cooltower outlet
        Real64 OutletTemp;      // Dry bulb temperature of air at the cooltower outlet
        Real64 IntHumRat;       // Humidity ratio of initialized air

        state.dataHeatBalFanSys->MCPTC = 0.0;
        state.dataHeatBalFanSys->MCPC = 0.0;
        state.dataHeatBalFanSys->CTMFL = 0.0;

        auto &Zone(state.dataHeatBal->Zone);

        for (CoolTowerNum = 1; CoolTowerNum <= state.dataCoolTower->NumCoolTowers; ++CoolTowerNum) {
            ZoneNum = state.dataCoolTower->CoolTowerSys(CoolTowerNum).ZonePtr;

            if (GetCurrentScheduleValue(state, state.dataCoolTower->CoolTowerSys(CoolTowerNum).SchedPtr) > 0.0) {
                // check component operation
                if (state.dataEnvrn->WindSpeed < MinWindSpeed || state.dataEnvrn->WindSpeed > MaxWindSpeed) continue;
                if (state.dataHeatBalFanSys->MAT(ZoneNum) < state.dataCoolTower->CoolTowerSys(CoolTowerNum).MinZoneTemp) continue;

                // Unit is on and simulate this component
                // Determine the temperature and air flow rate at the cooltower outlet
                if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FlowCtrlType == FlowCtrlEnum::WindDriven) {
                    Real64 const height_sqrt(std::sqrt(state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight));
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletVelocity = 0.7 * height_sqrt + 0.47 * (state.dataEnvrn->WindSpeed - 1.0);
                    AirVolFlowRate =
                        state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletArea * state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletVelocity;
                    AirVolFlowRate = min(AirVolFlowRate, state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate);
                    WaterFlowRate = (AirVolFlowRate / (0.0125 * height_sqrt));
                    if (WaterFlowRate > state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate * UCFactor) {
                        WaterFlowRate = state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate * UCFactor;
                        AirVolFlowRate = 0.0125 * WaterFlowRate * height_sqrt;
                        AirVolFlowRate = min(AirVolFlowRate, state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate);
                    }
                    WaterFlowRate = min(WaterFlowRate, (state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate * UCFactor));
                    OutletTemp =
                        state.dataEnvrn->OutDryBulbTemp - (state.dataEnvrn->OutDryBulbTemp - state.dataEnvrn->OutWetBulbTemp) *
                                                              (1.0 - std::exp(-0.8 * state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight)) *
                                                              (1.0 - std::exp(-0.15 * WaterFlowRate));
                } else if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FlowCtrlType == FlowCtrlEnum::FlowSchedule) {
                    WaterFlowRate = state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxWaterFlowRate * UCFactor;
                    AirVolFlowRate = 0.0125 * WaterFlowRate * std::sqrt(state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight);
                    AirVolFlowRate = min(AirVolFlowRate, state.dataCoolTower->CoolTowerSys(CoolTowerNum).MaxAirVolFlowRate);
                    OutletTemp =
                        state.dataEnvrn->OutDryBulbTemp - (state.dataEnvrn->OutDryBulbTemp - state.dataEnvrn->OutWetBulbTemp) *
                                                              (1.0 - std::exp(-0.8 * state.dataCoolTower->CoolTowerSys(CoolTowerNum).TowerHeight)) *
                                                              (1.0 - std::exp(-0.15 * WaterFlowRate));
                }

                if (OutletTemp < state.dataEnvrn->OutWetBulbTemp) {
                    ShowSevereError(state, "Cooltower outlet temperature exceed the outdoor wet bulb temperature reset to input values");
                    ShowContinueError(state, "Occurs in Cooltower =" + state.dataCoolTower->CoolTowerSys(CoolTowerNum).Name);
                }

                WaterFlowRate /= UCFactor;
                // Determine actual water flow rate
                if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss > 0.0) {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualWaterFlowRate =
                        WaterFlowRate * (1.0 + state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracWaterLoss);
                } else {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualWaterFlowRate = WaterFlowRate;
                }

                // Determine actual air flow rate
                if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched > 0.0) {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualAirVolFlowRate =
                        AirVolFlowRate * (1.0 - state.dataCoolTower->CoolTowerSys(CoolTowerNum).FracFlowSched);
                } else {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualAirVolFlowRate = AirVolFlowRate;
                }

                // Determine pump power
                if (GetCurrentScheduleValue(state, state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpSchedPtr) > 0) {
                    PumpPartLoadRat = GetCurrentScheduleValue(state, state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpSchedPtr);
                } else {
                    PumpPartLoadRat = 1.0;
                }

                // Determine air mass flow rate and volume flow rate
                InletHumRat = PsyWFnTdbTwbPb(state, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutBaroPress);
                // Assume no pressure drops and no changes in enthalpy between inlet and outlet air
                IntHumRat = PsyWFnTdbH(state, OutletTemp, state.dataEnvrn->OutEnthalpy); // Initialized humidity ratio
                AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, OutletTemp, IntHumRat);
                AirMassFlowRate = AirDensity * state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualAirVolFlowRate;
                // From the mass balance W_in*(m_air + m_water) = W_out*m_air
                RhoWater = RhoH2O(OutletTemp); // Assume T_water = T_outlet
                OutletHumRat = (InletHumRat * (AirMassFlowRate + (state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualWaterFlowRate * RhoWater))) /
                               AirMassFlowRate;
                AirSpecHeat = PsyCpAirFnW(OutletHumRat);
                AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, OutletTemp, OutletHumRat); // Outlet air density
                CVF_ZoneNum = state.dataCoolTower->CoolTowerSys(CoolTowerNum).ActualAirVolFlowRate *
                              GetCurrentScheduleValue(state, state.dataCoolTower->CoolTowerSys(CoolTowerNum).SchedPtr);
                state.dataHeatBalFanSys->MCPC(ZoneNum) = CVF_ZoneNum * AirDensity * AirSpecHeat;
                state.dataHeatBalFanSys->MCPTC(ZoneNum) = state.dataHeatBalFanSys->MCPC(ZoneNum) * OutletTemp;
                state.dataHeatBalFanSys->CTMFL(ZoneNum) = state.dataHeatBalFanSys->MCPC(ZoneNum) / AirSpecHeat;

                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatPower =
                    state.dataHeatBalFanSys->MCPC(ZoneNum) * std::abs(state.dataHeatBalFanSys->ZT(ZoneNum) - OutletTemp);
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatPower =
                    CVF_ZoneNum * std::abs(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) - OutletHumRat);
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletTemp = OutletTemp;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletHumRat = OutletHumRat;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRate = CVF_ZoneNum;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirMassFlowRate = state.dataHeatBalFanSys->CTMFL(ZoneNum);
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRateStd =
                    state.dataHeatBalFanSys->CTMFL(ZoneNum) / state.dataEnvrn->StdRhoAir;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletDBTemp = Zone(ZoneNum).OutDryBulbTemp;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletWBTemp = Zone(ZoneNum).OutWetBulbTemp;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletHumRat = state.dataEnvrn->OutHumRat;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate =
                    (std::abs(InletHumRat - OutletHumRat) * state.dataHeatBalFanSys->CTMFL(ZoneNum)) / RhoWater;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeupRate = 0.0; // initialize -- calc in update
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecPower =
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).RatedPumpPower * PumpPartLoadRat;
            } else { // Unit is off
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatPower = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatPower = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletTemp = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).OutletHumRat = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRate = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirMassFlowRate = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRateStd = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletDBTemp = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).InletHumRat = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecPower = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate = 0.0;
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeupRate = 0.0;
            }
        }
    }

    void UpdateCoolTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   October 2000
        //       MODIFIED       Aug 2008 Daeho Kang
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using namespace DataWater;

        int CoolTowerNum;
        Real64 AvailWaterRate;

        for (CoolTowerNum = 1; CoolTowerNum <= state.dataCoolTower->NumCoolTowers; ++CoolTowerNum) {

            // Set the demand request for supply water from water storage tank (if needed)
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode == WaterSupplyMode::FromTank) {
                state.dataWaterData->WaterStorage(state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupTankID)
                    .VdotRequestDemand(state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterTankDemandARRID) =
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate;
            }

            // check if should be starved by restricted flow from tank
            if (state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupplyMode == WaterSupplyMode::FromTank) {
                AvailWaterRate = state.dataWaterData->WaterStorage(state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterSupTankID)
                                     .VdotAvailDemand(state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterTankDemandARRID);
                if (AvailWaterRate < state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate) {
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeupRate =
                        state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate - AvailWaterRate;
                    state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate = AvailWaterRate;
                }
            }
        }
    }

    void ReportCoolTower(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Aut 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoolTowerNum;
        Real64 TSMult;

        TSMult = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        for (CoolTowerNum = 1; CoolTowerNum <= state.dataCoolTower->NumCoolTowers; ++CoolTowerNum) {

            state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTAirVol = state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirVolFlowRate * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTAirMass = state.dataCoolTower->CoolTowerSys(CoolTowerNum).AirMassFlowRate * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatLoss = state.dataCoolTower->CoolTowerSys(CoolTowerNum).SenHeatPower * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatLoss = state.dataCoolTower->CoolTowerSys(CoolTowerNum).LatHeatPower * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecConsump = state.dataCoolTower->CoolTowerSys(CoolTowerNum).PumpElecPower * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsump =
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterConsumpRate * TSMult;
            state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeup =
                state.dataCoolTower->CoolTowerSys(CoolTowerNum).CoolTWaterStarvMakeupRate * TSMult;
        }
    }

} // namespace CoolTower

} // namespace EnergyPlus
