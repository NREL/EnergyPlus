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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace HighTempRadiantSystem {

    // Module containing the routines dealing with the high temperature radiant systems

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   February 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to simulate high temperature radiant systems.
    // It is the intention of this module to cover all types of high temperature
    // radiant systems (gas and electric)

    // METHODOLOGY EMPLOYED:
    // Based on work done in BLAST, the EnergyPlus low temperature radiant system
    // model, this model has similar inherent challenges that are similar to the
    // low temperature radiant system.  Because it is a system that directly
    // effects the surface heat balances, it must be a part of both the heat
    // balance routines and linked in with the HVAC system.
    // REFERENCES:
    // Building Systems Laboratory, BLAST User's Guide/Reference.
    // Maloney, Dan. 1987. "Development of a radiant heater model and the
    //   incorporation of thermal comfort considerations into the BLAST
    //   energy analysis program", M.S. thesis, University of Illinois at
    //   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using DataHVACGlobals::SmallLoad;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    constexpr const char *cGas("Gas");
    constexpr const char *cNaturalGas("NaturalGas");
    constexpr const char *cElectric("Electric");
    constexpr const char *cElectricity("Electricity");
    constexpr const char *cMATControl("MeanAirTemperature");                   // Control for using mean air temperature
    constexpr const char *cMRTControl("MeanRadiantTemperature");               // Control for using mean radiant temperature
    constexpr const char *cOperativeControl("OperativeTemperature");           // Control for using operative temperature
    constexpr const char *cMATSPControl("MeanAirTemperatureSetpoint");         // Control for to MAT setpoint
    constexpr const char *cMRTSPControl("MeanRadiantTemperatureSetpoint");     // Control for to MRT setpoint
    constexpr const char *cOperativeSPControl("OperativeTemperatureSetpoint"); // Control for operative temperature setpoint

    // MODULE VARIABLE DECLARATIONS:

    // SUBROUTINE SPECIFICATIONS FOR MODULE HighTempRadiantSystem

    // Functions

    void SimHighTempRadiantSystem(EnergyPlusData &state,
                                  std::string_view CompName,   // name of the low temperature radiant system
                                  bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                  Real64 &LoadMet,               // load met by the radiant system, in Watts
                                  int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the "manager" for the high temperature radiant
        // system model.  It is called from the outside and controls the
        // actions and subroutine calls to lower levels as appropriate.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus manager subroutine layout

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFoundInGet; // Set to true when there are severe errors during the Get routine
        int RadSysNum;         // Radiant system number/index in local derived types

        if (state.dataHighTempRadSys->GetInputFlag) {
            ErrorsFoundInGet = false;
            GetHighTempRadiantSystem(state, ErrorsFoundInGet);
            if (ErrorsFoundInGet)
                ShowFatalError(state, "GetHighTempRadiantSystem: Errors found in input.  Preceding condition(s) cause termination.");
            state.dataHighTempRadSys->GetInputFlag = false;
        }

        // Find the correct ZoneHVAC:HighTemperatureRadiant
        if (CompIndex == 0) {
            RadSysNum = UtilityRoutines::FindItemInList(CompName, state.dataHighTempRadSys->HighTempRadSys);
            if (RadSysNum == 0) {
                ShowFatalError(state, "SimHighTempRadiantSystem: Unit not found=" + std::string{CompName});
            }
            CompIndex = RadSysNum;
        } else {
            RadSysNum = CompIndex;
            if (RadSysNum > state.dataHighTempRadSys->NumOfHighTempRadSys || RadSysNum < 1) {
                ShowFatalError(state,
                               format("SimHighTempRadiantSystem:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      RadSysNum,
                                      state.dataHighTempRadSys->NumOfHighTempRadSys,
                                      CompName));
            }
            if (state.dataHighTempRadSys->CheckEquipName(RadSysNum)) {
                if (CompName != state.dataHighTempRadSys->HighTempRadSys(RadSysNum).Name) {
                    ShowFatalError(state,
                                   format("SimHighTempRadiantSystem: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          RadSysNum,
                                          CompName,
                                          state.dataHighTempRadSys->HighTempRadSys(RadSysNum).Name));
                }
                state.dataHighTempRadSys->CheckEquipName(RadSysNum) = false;
            }
        }

        InitHighTempRadiantSystem(state, FirstHVACIteration, RadSysNum);

        {
            auto const SELECT_CASE_var(state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ControlType);
            if ((SELECT_CASE_var == RadControlType::MATControl) || (SELECT_CASE_var == RadControlType::MRTControl) ||
                (SELECT_CASE_var == RadControlType::OperativeControl)) {
                CalcHighTempRadiantSystem(state, RadSysNum);
            } else if ((SELECT_CASE_var == RadControlType::MATSPControl) || (SELECT_CASE_var == RadControlType::MRTSPControl) ||
                       (SELECT_CASE_var == RadControlType::OperativeSPControl)) {
                CalcHighTempRadiantSystemSP(state, FirstHVACIteration, RadSysNum);
            }
        }

        UpdateHighTempRadiantSystem(state, RadSysNum, LoadMet);

        ReportHighTempRadiantSystem(state, RadSysNum);
    }

    void GetHighTempRadiantSystem(EnergyPlusData &state, bool &ErrorsFound // TRUE if errors are found on processing the input
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads the input for high temperature radiant systems
        // from the user input file.  This will contain all of the information
        // needed to simulate a high temperature radiant system.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataSizing::AutoSize;
        using DataSizing::CapacityPerFloorArea;
        using DataSizing::FractionOfAutosizedHeatingCapacity;
        using DataSizing::HeatingDesignCapacity;

        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MaxCombustionEffic(1.00); // Limit the combustion efficiency to perfection
        Real64 const MaxFraction(1.0);         // Limit the highest allowed fraction for heat transfer parts
        Real64 const MinCombustionEffic(0.01); // Limit the minimum combustion efficiency
        Real64 const MinFraction(0.0);         // Limit the lowest allowed fraction for heat transfer parts
        Real64 const MinThrottlingRange(0.5);  // Smallest throttling range allowed in degrees Celsius
        //  INTEGER,          PARAMETER :: MaxDistribSurfaces = 20    ! Maximum number of surfaces that a radiant heater can radiate to
        int const iHeatCAPMAlphaNum(4);                   // get input index to High Temperature Radiant system heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1);       // get input index to High Temperature Radiant system heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(2); // get input index to High Temperature Radiant system heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); //  get input index to High Temperature Radiant system heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllFracsSummed;           // Sum of the fractions radiant, latent, and lost (must be <= 1)
        Real64 FracOfRadPotentiallyLost; // Difference between unity and AllFracsSummed for error reporting
        int IOStatus;                    // Used in GetObjectItem
        int Item;                        // Item to be "gotten"
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        int SurfNum;                     // Surface number DO loop counter
        Real64 TotalFracToSurfs;         // Sum of fractions of radiation to surfaces

        // Initializations and allocations
        state.dataHighTempRadSys->NumOfHighTempRadSys =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneHVAC:HighTemperatureRadiant");

        state.dataHighTempRadSys->HighTempRadSys.allocate(state.dataHighTempRadSys->NumOfHighTempRadSys);
        state.dataHighTempRadSys->CheckEquipName.allocate(state.dataHighTempRadSys->NumOfHighTempRadSys);
        state.dataHighTempRadSys->HighTempRadSysNumericFields.allocate(state.dataHighTempRadSys->NumOfHighTempRadSys);
        state.dataHighTempRadSys->CheckEquipName = true;

        // extensible object, do not need max args because using IPShortCuts
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        cCurrentModuleObject = "ZoneHVAC:HighTemperatureRadiant";
        // Obtain all of the user data related to high temperature radiant systems...
        for (Item = 1; Item <= state.dataHighTempRadSys->NumOfHighTempRadSys; ++Item) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            state.dataHighTempRadSys->HighTempRadSysNumericFields(Item).FieldNames.allocate(NumNumbers);
            state.dataHighTempRadSys->HighTempRadSysNumericFields(Item).FieldNames = "";
            state.dataHighTempRadSys->HighTempRadSysNumericFields(Item).FieldNames = state.dataIPShortCut->cNumericFieldNames;
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            // General user input data
            state.dataHighTempRadSys->HighTempRadSys(Item).Name = state.dataIPShortCut->cAlphaArgs(1);

            state.dataHighTempRadSys->HighTempRadSys(Item).SchedName = state.dataIPShortCut->cAlphaArgs(2);
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHighTempRadSys->HighTempRadSys(Item).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (state.dataHighTempRadSys->HighTempRadSys(Item).SchedPtr == 0) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                        " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                        " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).ZoneName = state.dataIPShortCut->cAlphaArgs(3);
            state.dataHighTempRadSys->HighTempRadSys(Item).ZonePtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            // state.dataHighTempRadSys->HighTempRadSys( Item ).MaxPowerCapac = state.dataIPShortCut->rNumericArgs( 1 );

            // Determine High Temp Radiant heating design capacity sizing method
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeatingCapMethod = HeatingDesignCapacity;

                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum);
                    if (state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity < 0.0 &&
                        state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeatingCapMethod = CapacityPerFloorArea;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                    if (state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                    } else if (state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          "Illegal " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                    if (state.dataHighTempRadSys->HighTempRadSys(Item).ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(
                        state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                ShowContinueError(state,
                                  "Illegal " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), cNaturalGas)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType = RadHeaterType::Gas;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), cElectricity)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType = RadHeaterType::Electric;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), cGas)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType = RadHeaterType::Gas;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), cElectric)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType = RadHeaterType::Electric;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            if (state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType == RadHeaterType::Gas) {
                state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic = state.dataIPShortCut->rNumericArgs(4);
                // Limit the combustion efficiency to between zero and one...
                if (state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic < MinCombustionEffic) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic = MinCombustionEffic;
                    ShowWarningError(state,
                                     state.dataIPShortCut->cNumericFieldNames(4) + " was less than the allowable minimum, reset to minimum value.");
                    ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }
                if (state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic > MaxCombustionEffic) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic = MaxCombustionEffic;
                    ShowWarningError(
                        state, state.dataIPShortCut->cNumericFieldNames(4) + " was greater than the allowable maximum, reset to maximum value.");
                    ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }
            } else {
                state.dataHighTempRadSys->HighTempRadSys(Item).CombustionEffic = MaxCombustionEffic; // No inefficiency in the heater
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant = state.dataIPShortCut->rNumericArgs(5);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant < MinFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant = MinFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(5) + " was less than the allowable minimum, reset to minimum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant > MaxFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant = MaxFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(5) + " was greater than the allowable maximum, reset to maximum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent = state.dataIPShortCut->rNumericArgs(6);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent < MinFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent = MinFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(6) + " was less than the allowable minimum, reset to minimum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent > MaxFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent = MaxFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(6) + " was greater than the allowable maximum, reset to maximum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).FracLost = state.dataIPShortCut->rNumericArgs(7);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracLost < MinFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracLost = MinFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(7) + " was less than the allowable minimum, reset to minimum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracLost > MaxFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracLost = MaxFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(7) + " was greater than the allowable maximum, reset to maximum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }

            // Based on the input for fractions radiant, latent, and lost, determine the fraction convective (remaining fraction)
            AllFracsSummed = state.dataHighTempRadSys->HighTempRadSys(Item).FracRadiant + state.dataHighTempRadSys->HighTempRadSys(Item).FracLatent +
                             state.dataHighTempRadSys->HighTempRadSys(Item).FracLost;
            if (AllFracsSummed > MaxFraction) {
                ShowSevereError(state, "Fractions radiant, latent, and lost sum up to greater than 1 for" + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
                state.dataHighTempRadSys->HighTempRadSys(Item).FracConvect = 0.0;
            } else {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracConvect = 1.0 - AllFracsSummed;
            }

            // Process the temperature control type
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cMATControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::MATControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cMRTControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::MRTControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cOperativeControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::OperativeControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cMATSPControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::MATSPControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cMRTSPControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::MRTSPControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), cOperativeSPControl)) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::OperativeSPControl;
            } else {
                ShowWarningError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Control reset to OPERATIVE control for this " + cCurrentModuleObject);
                state.dataHighTempRadSys->HighTempRadSys(Item).ControlType = RadControlType::OperativeControl;
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).ThrottlRange = state.dataIPShortCut->rNumericArgs(8);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).ThrottlRange < MinThrottlingRange) {
                state.dataHighTempRadSys->HighTempRadSys(Item).ThrottlRange = 1.0;
                ShowWarningError(state, state.dataIPShortCut->cNumericFieldNames(8) + " is below the minimum allowed.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Thus, the throttling range value has been reset to 1.0");
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).SetptSched = state.dataIPShortCut->cAlphaArgs(7);
            state.dataHighTempRadSys->HighTempRadSys(Item).SetptSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7));
            if ((state.dataHighTempRadSys->HighTempRadSys(Item).SetptSchedPtr == 0) && (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                ShowSevereError(state, state.dataIPShortCut->cAlphaFieldNames(7) + " not found: " + state.dataIPShortCut->cAlphaArgs(7));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson = state.dataIPShortCut->rNumericArgs(9);
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson < MinFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson = MinFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(9) + " was less than the allowable minimum, reset to minimum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }
            if (state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson > MaxFraction) {
                state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson = MaxFraction;
                ShowWarningError(state,
                                 state.dataIPShortCut->cNumericFieldNames(9) + " was greater than the allowable maximum, reset to maximum value.");
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
            }

            state.dataHighTempRadSys->HighTempRadSys(Item).TotSurfToDistrib = NumNumbers - 9;
            //    IF (state.dataHighTempRadSys->HighTempRadSys(Item)%TotSurfToDistrib > MaxDistribSurfaces) THEN
            //      CALL ShowSevereError(state, 'Trying to distribute radiant energy to too many surfaces for heater
            //      '//TRIM(state.dataIPShortCut->cAlphaArgs(1))) CALL ShowContinueError(state, 'Occurs for '//TRIM(cCurrentModuleObject)//' =
            //      '//TRIM(state.dataIPShortCut->cAlphaArgs(1))) ErrorsFound=.TRUE.
            //    END IF
            state.dataHighTempRadSys->HighTempRadSys(Item).SurfaceName.allocate(state.dataHighTempRadSys->HighTempRadSys(Item).TotSurfToDistrib);
            state.dataHighTempRadSys->HighTempRadSys(Item).SurfacePtr.allocate(state.dataHighTempRadSys->HighTempRadSys(Item).TotSurfToDistrib);
            state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf.allocate(
                state.dataHighTempRadSys->HighTempRadSys(Item).TotSurfToDistrib);

            AllFracsSummed = state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson;
            for (SurfNum = 1; SurfNum <= state.dataHighTempRadSys->HighTempRadSys(Item).TotSurfToDistrib; ++SurfNum) {
                state.dataHighTempRadSys->HighTempRadSys(Item).SurfaceName(SurfNum) = state.dataIPShortCut->cAlphaArgs(SurfNum + 7);
                state.dataHighTempRadSys->HighTempRadSys(Item).SurfacePtr(SurfNum) =
                    HeatBalanceIntRadExchange::GetRadiantSystemSurface(state,
                                                                       cCurrentModuleObject,
                                                                       state.dataHighTempRadSys->HighTempRadSys(Item).Name,
                                                                       state.dataHighTempRadSys->HighTempRadSys(Item).ZonePtr,
                                                                       state.dataHighTempRadSys->HighTempRadSys(Item).SurfaceName(SurfNum),
                                                                       ErrorsFound);
                state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum) = state.dataIPShortCut->rNumericArgs(SurfNum + 9);
                // Error trap for fractions that are out of range
                if (state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum) < MinFraction) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum) = MinFraction;
                    ShowWarningError(state,
                                     state.dataIPShortCut->cNumericFieldNames(SurfNum + 9) +
                                         " was less than the allowable minimum, reset to minimum value.");
                    ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }
                if (state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum) > MaxFraction) {
                    state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum) = MaxFraction;
                    ShowWarningError(state,
                                     state.dataIPShortCut->cNumericFieldNames(SurfNum + 9) +
                                         " was greater than the allowable maximum, reset to maximum value.");
                    ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                }

                if (state.dataHighTempRadSys->HighTempRadSys(Item).SurfacePtr(SurfNum) != 0) {
                    state.dataSurface->SurfIntConvSurfGetsRadiantHeat(state.dataHighTempRadSys->HighTempRadSys(Item).SurfacePtr(SurfNum)) = true;
                }

                AllFracsSummed += state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribToSurf(SurfNum);

            } // ...end of DO loop through surfaces that the heater radiates to.

            // Error trap if the fractions add up to greater than 1.0
            if (AllFracsSummed > (MaxFraction + 0.01)) {
                ShowSevereError(state,
                                "Fraction of radiation distributed to surfaces sums up to greater than 1 for " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Occurs for " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            if (AllFracsSummed < (MaxFraction - 0.01)) { // User didn't distribute all of the radiation warn that some will be lost
                TotalFracToSurfs = AllFracsSummed - state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson;
                FracOfRadPotentiallyLost = 1.0 - AllFracsSummed;
                ShowSevereError(state,
                                "Fraction of radiation distributed to surfaces and people sums up to less than 1 for " +
                                    state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "This would result in some of the radiant energy delivered by the high temp radiant heater being lost.");
                ShowContinueError(state, format("The sum of all radiation fractions to surfaces = {:.5T}", TotalFracToSurfs));
                ShowContinueError(
                    state, format("The radiant fraction to people = {:.5T}", state.dataHighTempRadSys->HighTempRadSys(Item).FracDistribPerson));
                ShowContinueError(state, format("So, all radiant fractions including surfaces and people = {:.5T}", AllFracsSummed));
                ShowContinueError(state,
                                  format("This means that the fraction of radiant energy that would be lost from the high temperature radiant heater "
                                         "would be = {:.5T}",
                                         FracOfRadPotentiallyLost));
                ShowContinueError(state,
                                  "Please check and correct this so that all radiant energy is accounted for in " + cCurrentModuleObject + " = " +
                                      state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

        } // ...end of DO loop through all of the high temperature radiant heaters

        // Set up the output variables for high temperature radiant heaters
        // cCurrentModuleObject = "ZoneHVAC:HighTemperatureRadiant"
        for (Item = 1; Item <= state.dataHighTempRadSys->NumOfHighTempRadSys; ++Item) {
            SetupOutputVariable(state,
                                "Zone Radiant HVAC Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHighTempRadSys->HighTempRadSys(Item).HeatPower,
                                "System",
                                "Average",
                                state.dataHighTempRadSys->HighTempRadSys(Item).Name);
            SetupOutputVariable(state,
                                "Zone Radiant HVAC Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHighTempRadSys->HighTempRadSys(Item).HeatEnergy,
                                "System",
                                "Sum",
                                state.dataHighTempRadSys->HighTempRadSys(Item).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            if (state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType == RadHeaterType::Gas) {
                SetupOutputVariable(state,
                                    "Zone Radiant HVAC NaturalGas Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHighTempRadSys->HighTempRadSys(Item).GasPower,
                                    "System",
                                    "Average",
                                    state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                SetupOutputVariable(state,
                                    "Zone Radiant HVAC NaturalGas Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHighTempRadSys->HighTempRadSys(Item).GasEnergy,
                                    "System",
                                    "Sum",
                                    state.dataHighTempRadSys->HighTempRadSys(Item).Name,
                                    _,
                                    "NaturalGas",
                                    "Heating",
                                    _,
                                    "System");
            } else if (state.dataHighTempRadSys->HighTempRadSys(Item).HeaterType == RadHeaterType::Electric) {
                SetupOutputVariable(state,
                                    "Zone Radiant HVAC Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHighTempRadSys->HighTempRadSys(Item).ElecPower,
                                    "System",
                                    "Average",
                                    state.dataHighTempRadSys->HighTempRadSys(Item).Name);
                SetupOutputVariable(state,
                                    "Zone Radiant HVAC Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHighTempRadSys->HighTempRadSys(Item).ElecEnergy,
                                    "System",
                                    "Sum",
                                    state.dataHighTempRadSys->HighTempRadSys(Item).Name,
                                    _,
                                    "ELECTRICITY",
                                    "Heating",
                                    _,
                                    "System");
            }
        }
    }

    void InitHighTempRadiantSystem(EnergyPlusData &state,
                                   bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                   int const RadSysNum // Index for the low temperature radiant system under consideration within the derived types
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables relating to high temperature
        // radiant heating systems.

        // METHODOLOGY EMPLOYED:
        // Simply initializes whatever needs initializing.

        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum; // Intermediate variable for keeping track of the zone number
        int Loop;

        if (state.dataHighTempRadSys->firstTime) {
            state.dataHighTempRadSys->ZeroSourceSumHATsurf.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataHighTempRadSys->QHTRadSource.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, 0.0);
            state.dataHighTempRadSys->QHTRadSrcAvg.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, 0.0);
            state.dataHighTempRadSys->LastQHTRadSrc.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, 0.0);
            state.dataHighTempRadSys->LastSysTimeElapsed.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, 0.0);
            state.dataHighTempRadSys->LastTimeStepSys.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, 0.0);
            state.dataHighTempRadSys->MySizeFlag.dimension(state.dataHighTempRadSys->NumOfHighTempRadSys, true);
            state.dataHighTempRadSys->firstTime = false;
        }

        // need to check all units to see if they are on Zone Equipment List or issue warning
        if (!state.dataHighTempRadSys->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataHighTempRadSys->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataHighTempRadSys->NumOfHighTempRadSys; ++Loop) {
                if (CheckZoneEquipmentList(state, "ZoneHVAC:HighTemperatureRadiant", state.dataHighTempRadSys->HighTempRadSys(Loop).Name)) continue;
                ShowSevereError(state,
                                "InitHighTempRadiantSystem: Unit=[ZoneHVAC:HighTemperatureRadiant," +
                                    state.dataHighTempRadSys->HighTempRadSys(Loop).Name +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataHighTempRadSys->MySizeFlag(RadSysNum)) {
            // for each radiant systen do the sizing once.
            SizeHighTempRadiantSystem(state, RadSysNum);
            state.dataHighTempRadSys->MySizeFlag(RadSysNum) = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && state.dataHighTempRadSys->MyEnvrnFlag) {
            state.dataHighTempRadSys->ZeroSourceSumHATsurf = 0.0;
            state.dataHighTempRadSys->QHTRadSource = 0.0;
            state.dataHighTempRadSys->QHTRadSrcAvg = 0.0;
            state.dataHighTempRadSys->LastQHTRadSrc = 0.0;
            state.dataHighTempRadSys->LastSysTimeElapsed = 0.0;
            state.dataHighTempRadSys->LastTimeStepSys = 0.0;
            state.dataHighTempRadSys->MyEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataHighTempRadSys->MyEnvrnFlag = true;
        }

        if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) { // This is the first pass through in a particular time step
            ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
            state.dataHighTempRadSys->ZeroSourceSumHATsurf(ZoneNum) =
                SumHATsurf(state, ZoneNum);                          // Set this to figure out what part of the load the radiant system meets
            state.dataHighTempRadSys->QHTRadSrcAvg(RadSysNum) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
            state.dataHighTempRadSys->LastQHTRadSrc(RadSysNum) =
                0.0; // At the beginning of a time step, reset to zero so average calculation can start again
            state.dataHighTempRadSys->LastSysTimeElapsed(RadSysNum) =
                0.0; // At the beginning of a time step, reset to zero so average calculation can start again
            state.dataHighTempRadSys->LastTimeStepSys(RadSysNum) =
                0.0; // At the beginning of a time step, reset to zero so average calculation can start again
        }
    }

    void SizeHighTempRadiantSystem(EnergyPlusData &state, int const RadSysNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing high temperature radiant components for which max power input has not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains design heating load from the zone sizing arrays

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::HeatingCapacitySizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr const char *RoutineName("SizeHighTempRadiantSystem");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS
        Real64 MaxPowerCapacDes;  // Design maximum capacity for reproting
        Real64 MaxPowerCapacUser; // User hard-sized maximum capacity for reproting
        bool IsAutoSize;          // Indicator to autosizing nominal capacity

        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;          // autosized value of coil input field
        int FieldNum = 1;         // IDD numeric field number where input field description is found
        int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                          // HeatingCapacitySizing, etc.)
        bool PrintFlag;   // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                // FractionOfAutosizedHeatingCapacity )

        IsAutoSize = false;
        MaxPowerCapacDes = 0.0;
        MaxPowerCapacUser = 0.0;
        state.dataSize->DataScalableCapSizingON = false;

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);
        auto &FinalZoneSizing(state.dataSize->FinalZoneSizing);

        if (state.dataSize->CurZoneEqNum > 0) {

            CompType = "ZoneHVAC:HighTemperatureRadiant";
            CompName = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).Name;
            state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
            state.dataSize->DataZoneNumber = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
            SizingMethod = HeatingCapacitySizing;
            FieldNum = 1;
            PrintFlag = true;
            SizingString = state.dataHighTempRadSys->HighTempRadSysNumericFields(RadSysNum).FieldNames(FieldNum) + " [W]";
            CapSizingMethod = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeatingCapMethod;
            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedHeatingCapacity) {

                if (CapSizingMethod == HeatingDesignCapacity) {
                    if (state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ScaledHeatingCapacity == AutoSize) {
                        CheckZoneSizing(state, CompType, CompName);
                        ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                            FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad / (state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracRadiant +
                                                                                  state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracConvect);
                    } else {
                        ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ScaledHeatingCapacity;
                    }
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ScaledHeatingCapacity *
                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                    state.dataSize->DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    CheckZoneSizing(state, CompType, CompName);
                    ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ScaledHeatingCapacity;
                    ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                        FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad / (state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracRadiant +
                                                                              state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracConvect);
                    TempSize = AutoSize;
                    state.dataSize->DataScalableCapSizingON = true;
                } else {
                    TempSize = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ScaledHeatingCapacity;
                }
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataHighTempRadSys->HighTempRadSys(RadSysNum).MaxPowerCapac = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                state.dataSize->DataScalableCapSizingON = false;
            }
        }
    }

    void CalcHighTempRadiantSystem(EnergyPlusData &state, int const RadSysNum) // name of the low temperature radiant system
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a high temperature radiant heating system.

        // METHODOLOGY EMPLOYED:
        // Follows the methods used by many other pieces of zone equipment except
        // that we are controlling the input to the heater element.  Note that
        // cooling is not allowed for such a system.  Controls are very basic at
        // this point using just a linear interpolation between being off at
        // one end of the throttling range, fully on at the other end, and varying
        // linearly in between.

        // REFERENCES:
        // Other EnergyPlus modules
        // Building Systems Laboratory, BLAST User's Guide/Reference.
        // Fanger, P.O. "Analysis and Applications in Environmental Engineering",
        //   Danish Technical Press, 1970.
        // Maloney, Dan. 1987. "Development of a radiant heater model and the
        //   incorporation of thermal comfort considerations into the BLAST
        //   energy analysis program", M.S. thesis, University of Illinois at
        //   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HeatFrac; // fraction of maximum energy input to radiant system [dimensionless]
        Real64 OffTemp;  // Temperature above which the radiant system should be completely off [C]
        Real64 OpTemp;   // Operative temperature [C]
        //  REAL(r64)    :: QZnReq         ! heating or cooling needed by zone [Watts]
        Real64 SetPtTemp; // Setpoint temperature [C]
        int ZoneNum;      // number of zone being served

        // initialize local variables
        ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
        HeatFrac = 0.0;

        if (GetCurrentScheduleValue(state, state.dataHighTempRadSys->HighTempRadSys(RadSysNum).SchedPtr) <= 0) {

            // Unit is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions
            state.dataHighTempRadSys->QHTRadSource(RadSysNum) = 0.0;

        } else { // Unit might be on-->this section is intended to control the output of the
            // high temperature radiant heater (temperature controlled)

            // Determine the current setpoint temperature and the temperature at which the unit should be completely off
            SetPtTemp = GetCurrentScheduleValue(state, state.dataHighTempRadSys->HighTempRadSys(RadSysNum).SetptSchedPtr);
            OffTemp = SetPtTemp + 0.5 * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ThrottlRange;
            OpTemp = (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum)) / 2.0; // Approximate the "operative" temperature

            // Determine the fraction of maximum power to the unit (limiting the fraction range from zero to unity)
            {
                auto const SELECT_CASE_var(state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ControlType);
                if (SELECT_CASE_var == RadControlType::MATControl) {
                    HeatFrac = (OffTemp - state.dataHeatBalFanSys->MAT(ZoneNum)) / state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ThrottlRange;
                } else if (SELECT_CASE_var == RadControlType::MRTControl) {
                    HeatFrac = (OffTemp - state.dataHeatBal->ZoneMRT(ZoneNum)) / state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ThrottlRange;
                } else if (SELECT_CASE_var == RadControlType::OperativeControl) {
                    OpTemp = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
                    HeatFrac = (OffTemp - OpTemp) / state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ThrottlRange;
                }
            }
            if (HeatFrac < 0.0) HeatFrac = 0.0;
            if (HeatFrac > 1.0) HeatFrac = 1.0;

            // Set the heat source for the high temperature electric radiant system
            state.dataHighTempRadSys->QHTRadSource(RadSysNum) = HeatFrac * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).MaxPowerCapac;
        }
    }

    void CalcHighTempRadiantSystemSP(
        EnergyPlusData &state,
        [[maybe_unused]] bool const FirstHVACIteration, // true if this is the first HVAC iteration at this system time step !unused1208
        int const RadSysNum                             // name of the low temperature radiant system
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2008
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a high temperature radiant heating system using setpoint temperature control.

        // METHODOLOGY EMPLOYED:
        // Follows the methods used by many other pieces of zone equipment except
        // that we are controlling the input to the heater element.  Note that
        // cooling is not allowed for such a system.  Controls are very basic and
        // use an iterative approach to get close to what we need.

        // REFERENCES:
        // Other EnergyPlus modules
        // Building Systems Laboratory, BLAST User's Guide/Reference.
        // Fanger, P.O. "Analysis and Applications in Environmental Engineering",
        //   Danish Technical Press, 1970.
        // Maloney, Dan. 1987. "Development of a radiant heater model and the
        //   incorporation of thermal comfort considerations into the BLAST
        //   energy analysis program", M.S. thesis, University of Illinois at
        //   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        float const TempConvToler(0.1f); // Temperature controller tries to converge to within 0.1C
        int const MaxIterations(10);     // Maximum number of iterations to achieve temperature control
        // (10 interval halvings achieves control to 0.1% of capacity)
        // These two parameters are intended to achieve reasonable control
        // without excessive run times.

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ConvergFlag; // convergence flag for temperature control
        // unused  INTEGER, SAVE :: ErrIterCount=0   ! number of time max iterations has been exceeded
        float HeatFrac;       // fraction of maximum energy input to radiant system [dimensionless]
        float HeatFracMax;    // maximum range of heat fraction
        float HeatFracMin;    // minimum range of heat fraction
        int IterNum;          // iteration number
        Real64 SetPtTemp;     // Setpoint temperature [C]
        int ZoneNum;          // number of zone being served
        Real64 ZoneTemp(0.0); // zone temperature (MAT, MRT, or Operative Temperature, depending on control type) [C]

        // initialize local variables
        ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
        state.dataHighTempRadSys->QHTRadSource(RadSysNum) = 0.0;

        if (GetCurrentScheduleValue(state, state.dataHighTempRadSys->HighTempRadSys(RadSysNum).SchedPtr) > 0) {

            // Unit is scheduled on-->this section is intended to control the output of the
            // high temperature radiant heater (temperature controlled)

            // Determine the current setpoint temperature and the temperature at which the unit should be completely off
            SetPtTemp = GetCurrentScheduleValue(state, state.dataHighTempRadSys->HighTempRadSys(RadSysNum).SetptSchedPtr);

            // Now, distribute the radiant energy of all systems to the appropriate
            // surfaces, to people, and the air; determine the latent portion
            DistributeHTRadGains(state);

            // Now "simulate" the system by recalculating the heat balances
            HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
            HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

            // First determine whether or not the unit should be on
            // Determine the proper temperature on which to control
            {
                auto const SELECT_CASE_var(state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ControlType);
                if (SELECT_CASE_var == RadControlType::MATSPControl) {
                    ZoneTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                } else if (SELECT_CASE_var == RadControlType::MRTSPControl) {
                    ZoneTemp = state.dataHeatBal->ZoneMRT(ZoneNum);
                } else if (SELECT_CASE_var == RadControlType::OperativeSPControl) {
                    ZoneTemp = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
                } else {
                    assert(false);
                }
            }

            if (ZoneTemp < (SetPtTemp - TempConvToler)) {

                // Use simple interval halving to find the best operating fraction to achieve proper temperature control
                IterNum = 0;
                ConvergFlag = false;
                HeatFracMax = 1.0;
                HeatFracMin = 0.0;

                while ((IterNum <= MaxIterations) && (!ConvergFlag)) {

                    // In the first iteration (IterNum=0), try full capacity and see if that is the best solution
                    if (IterNum == 0) {
                        HeatFrac = 1.0;
                    } else {
                        HeatFrac = (HeatFracMin + HeatFracMax) / 2.0;
                    }

                    // Set the heat source for the high temperature radiant system
                    state.dataHighTempRadSys->QHTRadSource(RadSysNum) = HeatFrac * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).MaxPowerCapac;

                    // Now, distribute the radiant energy of all systems to the appropriate
                    // surfaces, to people, and the air; determine the latent portion
                    DistributeHTRadGains(state);

                    // Now "simulate" the system by recalculating the heat balances
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

                    // Redetermine the current value of the controlling temperature
                    {
                        auto const SELECT_CASE_var(state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ControlType);
                        if (SELECT_CASE_var == RadControlType::MATControl) {
                            ZoneTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                        } else if (SELECT_CASE_var == RadControlType::MRTControl) {
                            ZoneTemp = state.dataHeatBal->ZoneMRT(ZoneNum);
                        } else if (SELECT_CASE_var == RadControlType::OperativeControl) {
                            ZoneTemp = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
                        }
                    }

                    if ((std::abs(ZoneTemp - SetPtTemp)) <= TempConvToler) {
                        // The radiant heater has controlled the zone temperature to the appropriate level--stop iterating
                        ConvergFlag = true;
                    } else if (ZoneTemp < SetPtTemp) {
                        // The zone temperature is too low--try increasing the radiant heater output
                        if (IterNum == 0) {
                            // Heater already at capacity--this is the best that we can do
                            ConvergFlag = true;
                        } else {
                            HeatFracMin = HeatFrac;
                        }
                    } else { // (ZoneTemp > SetPtTemp)
                        // The zone temperature is too high--try decreasing the radiant heater output
                        if (IterNum > 0) HeatFracMax = HeatFrac;
                    }

                    ++IterNum;
                }
            }
        }
    }

    void UpdateHighTempRadiantSystem(EnergyPlusData &state,
                                     int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
                                     Real64 &LoadMet      // load met by the radiant system, in Watts
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does any updating that needs to be done for high
        // temperature radiant heating systems.  This routine has two functions.
        // First, it needs to keep track of the average high temperature
        // radiant source.  The method for doing this is similar to low
        // temperature systems except that heat input is kept locally on
        // a system basis rather than a surface basis.  This is because a high
        // temperature system affects many surfaces while a low temperature
        // system directly affects only one surface.  This leads to the second
        // function of this subroutine which is to account for the affect of
        // all high temperature radiant systems on each surface.  This
        // distribution must be "redone" every time to be sure that we have
        // properly accounted for all of the systems.

        // METHODOLOGY EMPLOYED:
        // For the source average update, if the system time step elapsed is
        // still what it used to be, then either we are still iterating or we
        // had to go back and shorten the time step.  As a result, we have to
        // subtract out the previous value that we added.  If the system time
        // step elapsed is different, then we just need to add the new values
        // to the running average.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum; // Zone index number for the current radiant system

        // First, update the running average if necessary...
        if (state.dataHighTempRadSys->LastSysTimeElapsed(RadSysNum) == SysTimeElapsed) {
            // Still iterating or reducing system time step, so subtract old values which were
            // not valid
            state.dataHighTempRadSys->QHTRadSrcAvg(RadSysNum) -= state.dataHighTempRadSys->LastQHTRadSrc(RadSysNum) *
                                                                 state.dataHighTempRadSys->LastTimeStepSys(RadSysNum) /
                                                                 state.dataGlobal->TimeStepZone;
        }

        // Update the running average and the "last" values with the current values of the appropriate variables
        state.dataHighTempRadSys->QHTRadSrcAvg(RadSysNum) +=
            state.dataHighTempRadSys->QHTRadSource(RadSysNum) * TimeStepSys / state.dataGlobal->TimeStepZone;

        state.dataHighTempRadSys->LastQHTRadSrc(RadSysNum) = state.dataHighTempRadSys->QHTRadSource(RadSysNum);
        state.dataHighTempRadSys->LastSysTimeElapsed(RadSysNum) = SysTimeElapsed;
        state.dataHighTempRadSys->LastTimeStepSys(RadSysNum) = TimeStepSys;

        {
            auto const SELECT_CASE_var(state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ControlType);
            if ((SELECT_CASE_var == RadControlType::MATControl) || (SELECT_CASE_var == RadControlType::MRTControl) ||
                (SELECT_CASE_var == RadControlType::OperativeControl)) {
                // Only need to do this for the non-SP controls (SP has already done this enough)
                // Now, distribute the radiant energy of all systems to the appropriate
                // surfaces, to people, and the air; determine the latent portion
                DistributeHTRadGains(state);

                // Now "simulate" the system by recalculating the heat balances
                ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
                HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);
            }
        }

        if (state.dataHighTempRadSys->QHTRadSource(RadSysNum) <= 0.0) {
            LoadMet = 0.0; // System wasn't running so it can't meet a load
        } else {
            ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;
            LoadMet = (SumHATsurf(state, ZoneNum) - state.dataHighTempRadSys->ZeroSourceSumHATsurf(ZoneNum)) +
                      state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum);
        }
    }

    void UpdateHTRadSourceValAvg(EnergyPlusData &state, bool &HighTempRadSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To transfer the average value of the heat source over the entire
        // zone time step back to the heat balance routines so that the heat
        // balance algorithms can simulate one last time with the average source
        // to maintain some reasonable amount of continuity and energy balance
        // in the temperature and flux histories.

        // METHODOLOGY EMPLOYED:
        // All of the record keeping for the average term is done in the Update
        // routine so the only other thing that this subroutine does is check to
        // see if the system was even on.  If any average term is non-zero, then
        // one or more of the radiant systems was running.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadSysNum; // DO loop counter for surface index

        HighTempRadSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(state.dataHighTempRadSys->QHTRadSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (RadSysNum = 1; RadSysNum <= state.dataHighTempRadSys->NumOfHighTempRadSys; ++RadSysNum) {
            if (state.dataHighTempRadSys->QHTRadSrcAvg(RadSysNum) != 0.0) {
                HighTempRadSysOn = true;
                break; // DO loop
            }
        }

        state.dataHighTempRadSys->QHTRadSource = state.dataHighTempRadSys->QHTRadSrcAvg;

        DistributeHTRadGains(state); // state.dataHighTempRadSys->QHTRadSource has been modified so we need to redistribute gains
    }

    void DistributeHTRadGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       April 2010 Brent Griffith, max limit to protect surface temperature calcs
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To distribute the gains from the high temperature radiant heater
        // as specified in the user input file.  This includes distribution
        // of long wavelength radiant gains to surfaces and "people" as well
        // as latent, lost, and convective portions of the total gain.

        // METHODOLOGY EMPLOYED:
        // We must cycle through all of the radiant systems because each
        // surface could feel the effect of more than one radiant system.
        // Note that the energy radiated to people is assumed to affect them
        // but them it is assumed to be convected to the air.  This is why
        // the convective portion shown below has two parts to it.

        // Using/Aliasing
        using DataHeatBalFanSys::MaxRadHeatFlux;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
        int RadSysNum;            // Counter for the radiant systems
        int SurfNum;              // Pointer to the Surface derived type
        int ZoneNum;              // Pointer to the Zone derived type
        Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

        // Initialize arrays
        state.dataHeatBalFanSys->SumConvHTRadSys = 0.0;
        state.dataHeatBalFanSys->SumLatentHTRadSys = 0.0;
        state.dataHeatBalFanSys->QHTRadSysSurf = 0.0;
        state.dataHeatBalFanSys->QHTRadSysToPerson = 0.0;

        for (RadSysNum = 1; RadSysNum <= state.dataHighTempRadSys->NumOfHighTempRadSys; ++RadSysNum) {

            ZoneNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ZonePtr;

            state.dataHeatBalFanSys->QHTRadSysToPerson(ZoneNum) = state.dataHighTempRadSys->QHTRadSource(RadSysNum) *
                                                                  state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracRadiant *
                                                                  state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracDistribPerson;

            state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) +=
                state.dataHighTempRadSys->QHTRadSource(RadSysNum) * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracConvect;

            state.dataHeatBalFanSys->SumLatentHTRadSys(ZoneNum) +=
                state.dataHighTempRadSys->QHTRadSource(RadSysNum) * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracLatent;

            for (RadSurfNum = 1; RadSurfNum <= state.dataHighTempRadSys->HighTempRadSys(RadSysNum).TotSurfToDistrib; ++RadSurfNum) {
                SurfNum = state.dataHighTempRadSys->HighTempRadSys(RadSysNum).SurfacePtr(RadSurfNum);
                if (state.dataSurface->Surface(SurfNum).Area > SmallestArea) {
                    ThisSurfIntensity =
                        (state.dataHighTempRadSys->QHTRadSource(RadSysNum) * state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracRadiant *
                         state.dataHighTempRadSys->HighTempRadSys(RadSysNum).FracDistribToSurf(RadSurfNum) /
                         state.dataSurface->Surface(SurfNum).Area);
                    state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) += ThisSurfIntensity;

                    if (ThisSurfIntensity > MaxRadHeatFlux) { // CR 8074, trap for excessive intensity (throws off surface balance )
                        ShowSevereError(state, "DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected");
                        ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                        ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                        ShowContinueError(state,
                                          "Occurs in ZoneHVAC:HighTemperatureRadiant = " + state.dataHighTempRadSys->HighTempRadSys(RadSysNum).Name);
                        ShowContinueError(state, format("Radiation intensity = {:.2R} [W/m2]", ThisSurfIntensity));
                        ShowContinueError(state, "Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant");
                        ShowFatalError(state, "DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected");
                    }
                } else { // small surface
                    ShowSevereError(state, "DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux");
                    ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                    ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                    ShowContinueError(state,
                                      "Occurs in ZoneHVAC:HighTemperatureRadiant = " + state.dataHighTempRadSys->HighTempRadSys(RadSysNum).Name);
                    ShowContinueError(state, "Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant");
                    ShowFatalError(state, "DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux");
                }
            }
        }

        // Here an assumption is made regarding radiant heat transfer to people.
        // While the QHTRadSysToPerson array will be used by the thermal comfort
        // routines, the energy transfer to people would get lost from the perspective
        // of the heat balance.  So, to avoid this net loss of energy which clearly
        // gets added to the zones, we must account for it somehow.  This assumption
        // that all energy radiated to people is converted to convective energy is
        // not very precise, but at least it conserves energy.
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) += state.dataHeatBalFanSys->QHTRadSysToPerson(ZoneNum);
        }
    }

    void ReportHighTempRadiantSystem(EnergyPlusData &state,
                                     int const RadSysNum) // Index for the low temperature radiant system under consideration within the derived types
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simply produces output for the high temperature radiant system.

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        if (state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeaterType == RadHeaterType::Gas) {
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).GasPower =
                state.dataHighTempRadSys->QHTRadSource(RadSysNum) / state.dataHighTempRadSys->HighTempRadSys(RadSysNum).CombustionEffic;
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).GasEnergy =
                state.dataHighTempRadSys->HighTempRadSys(RadSysNum).GasPower * TimeStepSys * DataGlobalConstants::SecInHour;
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ElecPower = 0.0;
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ElecEnergy = 0.0;
        } else if (state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeaterType == RadHeaterType::Electric) {
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).GasPower = 0.0;
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).GasEnergy = 0.0;
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ElecPower = state.dataHighTempRadSys->QHTRadSource(RadSysNum);
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ElecEnergy =
                state.dataHighTempRadSys->HighTempRadSys(RadSysNum).ElecPower * TimeStepSys * DataGlobalConstants::SecInHour;
        } else {
            ShowWarningError(state, "Someone forgot to add a high temperature radiant heater type to the reporting subroutine");
        }
        state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeatPower = state.dataHighTempRadSys->QHTRadSource(RadSysNum);
        state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeatEnergy =
            state.dataHighTempRadSys->HighTempRadSys(RadSysNum).HeatPower * TimeStepSys * DataGlobalConstants::SecInHour;
    }

    Real64 SumHATsurf(EnergyPlusData &state, int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;

        // Return value
        Real64 SumHATsurf;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // Surface number
        Real64 Area; // Effective surface area

        SumHATsurf = 0.0;

        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

            Area = state.dataSurface->Surface(SurfNum).Area;

            if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
                    Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                }

                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                  (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) * state.dataSurface->SurfWinFrameTempSurfIn(SurfNum);
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 &&
                    !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                  (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                  state.dataSurface->SurfWinDividerTempSurfIn(SurfNum);
                }
            }

            SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * Area * state.dataHeatBalSurf->TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

} // namespace HighTempRadiantSystem

} // namespace EnergyPlus
