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
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ElectricBaseboardRadiator.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ElectricBaseboardRadiator {

    // Module ElectricBaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Electric)

    // Module containing the routines dealing with the electric baseboard heater

    // MODULE INFORMATION:
    //       AUTHOR         Daeho Kang
    //       DATE WRITTEN   Feb 2010

    // PURPOSE OF THIS MODULE:
    // This module is to calculate the actual convective heat addition that an electrical baseboard heater
    // deliveres to a space.

    // METHODOLOGY EMPLOYED:
    // Based on the convective-only electric baseboard module (Object: ZoneHVAC:Baseboard:Convective:Electric)
    // written by Richard Liesen in Nov 2001, this new electric baseboard module is to add the existing calculation
    // algorithm of radiant heat transfer in the high temperature radiant system module.

    // REFERENCES:
    // HighTempRadiantSystem module (ZoneHVAC:HighTemperatureRadiant)
    // Convective electric baseboard module (ZoneHVAC:Baseboard:Convective:Electric)

    void SimElecBaseboard(EnergyPlusData &state,
                          std::string const &EquipName,
                          [[maybe_unused]] int const ActualZoneNum,
                          int const ControlledZoneNum,
                          bool const FirstHVACIteration,
                          Real64 &PowerMet,
                          int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       Feb 2010 Daeho Kang for radiant component

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Electric Baseboard units.

        // REFERENCES:
        // Water baseboard module

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum; // Index of unit in baseboard array
        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;
        auto &GetInputFlag = state.dataElectBaseboardRad->GetInputFlag;
        auto &NumElecBaseboards = state.dataElectBaseboardRad->NumElecBaseboards;
        auto &CheckEquipName = state.dataElectBaseboardRad->CheckEquipName;

        if (GetInputFlag) {
            GetElectricBaseboardInput(state);
            GetInputFlag = false;
        }

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(EquipName, ElecBaseboard, &ElecBaseboardParams::EquipName);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "SimElectricBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > NumElecBaseboards || BaseboardNum < 1) {
                ShowFatalError(state,
                               format("SimElectricBaseboard:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      BaseboardNum,
                                      NumElecBaseboards,
                                      EquipName));
            }
            if (CheckEquipName(BaseboardNum)) {
                if (EquipName != ElecBaseboard(BaseboardNum).EquipName) {
                    ShowFatalError(state,
                                   format("SimElectricBaseboard: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          BaseboardNum,
                                          EquipName,
                                          ElecBaseboard(BaseboardNum).EquipName));
                }
                CheckEquipName(BaseboardNum) = false;
            }
        }

        InitElectricBaseboard(state, BaseboardNum, ControlledZoneNum, FirstHVACIteration);
        CalcElectricBaseboard(state, BaseboardNum, ControlledZoneNum);

        PowerMet = ElecBaseboard(BaseboardNum).TotPower;

        UpdateElectricBaseboard(state, BaseboardNum);
        ReportElectricBaseboard(state, BaseboardNum);
    }

    void GetElectricBaseboardInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       Feb 2010 Daeho Kang for radiant component

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the Baseboard units.

        // Using/Aliasing
        using DataSizing::AutoSize;
        using DataSizing::CapacityPerFloorArea;
        using DataSizing::FractionOfAutosizedHeatingCapacity;
        using DataSizing::HeatingDesignCapacity;
        using GlobalNames::VerifyUniqueBaseboardName;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBaseboardInput: "); // include trailing blank space
        Real64 const MaxFraction(1.0);                               // Maximum limit of fractional values
        Real64 const MinFraction(0.0);                               // Minimum limit of fractional values
        //    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20      ! Maximum number of surfaces that a baseboard heater can radiate to
        int const MinDistribSurfaces(1);                  // Minimum number of surfaces that a baseboard heater can radiate to
        int const iHeatCAPMAlphaNum(3);                   // get input index to HW baseboard heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1);       // get input index to HW baseboard heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(2); // get input index to HW baseboard heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); // get input index to HW baseboard heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllFracsSummed; // Sum of the fractions radiant
        int BaseboardNum;
        int NumAlphas;
        int NumNumbers;
        int SurfNum; // surface number that radiant heat delivered
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        cCurrentModuleObject = state.dataElectBaseboardRad->cCMO_BBRadiator_Electric;

        state.dataElectBaseboardRad->NumElecBaseboards = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // object is extensible, no max args needed as IPShortCuts being used
        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;
        auto &CheckEquipName = state.dataElectBaseboardRad->CheckEquipName;
        auto &ElecBaseboardNumericFields = state.dataElectBaseboardRad->ElecBaseboardNumericFields;
        auto &NumElecBaseboards = state.dataElectBaseboardRad->NumElecBaseboards;

        ElecBaseboard.allocate(NumElecBaseboards);
        CheckEquipName.allocate(NumElecBaseboards);
        ElecBaseboardNumericFields.allocate(NumElecBaseboards);
        CheckEquipName = true;

        for (BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     BaseboardNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            ElecBaseboardNumericFields(BaseboardNum).FieldNames.allocate(NumNumbers);
            ElecBaseboardNumericFields(BaseboardNum).FieldNames = "";
            ElecBaseboardNumericFields(BaseboardNum).FieldNames = state.dataIPShortCut->cNumericFieldNames;
            if (UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound)) {
                continue;
            }

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state, cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

            ElecBaseboard(BaseboardNum).EquipName = state.dataIPShortCut->cAlphaArgs(1); // name of this baseboard
            ElecBaseboard(BaseboardNum).Schedule = state.dataIPShortCut->cAlphaArgs(2);
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                ElecBaseboard(BaseboardNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                ElecBaseboard(BaseboardNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (ElecBaseboard(BaseboardNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                        " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) +
                                        '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            // Determine HW radiant baseboard heating design capacity sizing method
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                ElecBaseboard(BaseboardNum).HeatingCapMethod = HeatingDesignCapacity;

                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                    ElecBaseboard(BaseboardNum).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum);
                    if (ElecBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0 && ElecBaseboard(BaseboardNum).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                ElecBaseboard(BaseboardNum).HeatingCapMethod = CapacityPerFloorArea;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    ElecBaseboard(BaseboardNum).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                    if (ElecBaseboard(BaseboardNum).ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                    } else if (ElecBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          "Illegal " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                ElecBaseboard(BaseboardNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    ElecBaseboard(BaseboardNum).ScaledHeatingCapacity = state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                    if (ElecBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(
                        state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + " = " + ElecBaseboard(BaseboardNum).EquipName);
                ShowContinueError(state,
                                  "Illegal " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            ElecBaseboard(BaseboardNum).BaseboardEfficiency = state.dataIPShortCut->rNumericArgs(4);
            ElecBaseboard(BaseboardNum).FracRadiant = state.dataIPShortCut->rNumericArgs(5);
            if (ElecBaseboard(BaseboardNum).FracRadiant < MinFraction) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(5) + " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinFraction));
                ElecBaseboard(BaseboardNum).FracRadiant = MinFraction;
            }
            if (ElecBaseboard(BaseboardNum).FracRadiant > MaxFraction) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(5) + " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                ElecBaseboard(BaseboardNum).FracRadiant = MaxFraction;
            }

            // Remaining fraction is added to the zone as convective heat transfer
            AllFracsSummed = ElecBaseboard(BaseboardNum).FracRadiant;
            if (AllFracsSummed > MaxFraction) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", Fraction Radiant was higher than the allowable maximum.");
                ElecBaseboard(BaseboardNum).FracRadiant = MaxFraction;
                ElecBaseboard(BaseboardNum).FracConvect = 0.0;
            } else {
                ElecBaseboard(BaseboardNum).FracConvect = 1.0 - AllFracsSummed;
            }

            ElecBaseboard(BaseboardNum).FracDistribPerson = state.dataIPShortCut->rNumericArgs(6);
            if (ElecBaseboard(BaseboardNum).FracDistribPerson < MinFraction) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(6) + " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinFraction));
                ElecBaseboard(BaseboardNum).FracDistribPerson = MinFraction;
            }
            if (ElecBaseboard(BaseboardNum).FracDistribPerson > MaxFraction) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                     state.dataIPShortCut->cNumericFieldNames(6) + " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                ElecBaseboard(BaseboardNum).FracDistribPerson = MaxFraction;
            }

            ElecBaseboard(BaseboardNum).TotSurfToDistrib = NumNumbers - 6;
            //      IF (ElecBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
            //        CALL ShowWarningError(state, RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(state.dataIPShortCut->cAlphaArgs(1))// &
            //          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
            //        CALL ShowContinueError(state, '...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))// &
            //           '] will be processed.')
            //        ElecBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
            //      END IF
            if ((ElecBaseboard(BaseboardNum).TotSurfToDistrib < MinDistribSurfaces) && (ElecBaseboard(BaseboardNum).FracRadiant > MinFraction)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", the number of surface/radiant fraction groups entered was less than the allowable minimum.");
                ShowContinueError(state, format("...the minimum that must be entered=[{}].", MinDistribSurfaces));
                ErrorsFound = true;
                ElecBaseboard(BaseboardNum).TotSurfToDistrib = 0; // error
            }

            ElecBaseboard(BaseboardNum).SurfaceName.allocate(ElecBaseboard(BaseboardNum).TotSurfToDistrib);
            ElecBaseboard(BaseboardNum).SurfaceName = "";
            ElecBaseboard(BaseboardNum).SurfacePtr.allocate(ElecBaseboard(BaseboardNum).TotSurfToDistrib);
            ElecBaseboard(BaseboardNum).SurfacePtr = 0;
            ElecBaseboard(BaseboardNum).FracDistribToSurf.allocate(ElecBaseboard(BaseboardNum).TotSurfToDistrib);
            ElecBaseboard(BaseboardNum).FracDistribToSurf = 0.0;

            // search zone equipment list structure for zone index
            for (int ctrlZone = 1; ctrlZone <= state.dataGlobal->NumOfZones; ++ctrlZone) {
                for (int zoneEquipTypeNum = 1; zoneEquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ctrlZone).NumOfEquipTypes; ++zoneEquipTypeNum) {
                    if (state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipType_Num(zoneEquipTypeNum) == DataZoneEquipment::BBElectric_Num &&
                        state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipName(zoneEquipTypeNum) == ElecBaseboard(BaseboardNum).EquipName) {
                        ElecBaseboard(BaseboardNum).ZonePtr = ctrlZone;
                    }
                }
            }
            if (ElecBaseboard(BaseboardNum).ZonePtr <= 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + ElecBaseboard(BaseboardNum).EquipName +
                                    "\" is not on any ZoneHVAC:EquipmentList.");
                ErrorsFound = true;
                break;
            }

            AllFracsSummed = ElecBaseboard(BaseboardNum).FracDistribPerson;
            for (SurfNum = 1; SurfNum <= ElecBaseboard(BaseboardNum).TotSurfToDistrib; ++SurfNum) {
                ElecBaseboard(BaseboardNum).SurfaceName(SurfNum) = state.dataIPShortCut->cAlphaArgs(SurfNum + 3);
                ElecBaseboard(BaseboardNum).SurfacePtr(SurfNum) =
                    HeatBalanceIntRadExchange::GetRadiantSystemSurface(state,
                                                                       cCurrentModuleObject,
                                                                       ElecBaseboard(BaseboardNum).EquipName,
                                                                       ElecBaseboard(BaseboardNum).ZonePtr,
                                                                       ElecBaseboard(BaseboardNum).SurfaceName(SurfNum),
                                                                       ErrorsFound);
                ElecBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) = state.dataIPShortCut->rNumericArgs(SurfNum + 6);
                if (ElecBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) > MaxFraction) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(SurfNum + 6) + "was greater than the allowable maximum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxFraction));
                    ElecBaseboard(BaseboardNum).TotSurfToDistrib = MaxFraction;
                }
                if (ElecBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) < MinFraction) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(SurfNum + 6) + "was less than the allowable minimum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MinFraction));
                    ElecBaseboard(BaseboardNum).TotSurfToDistrib = MinFraction;
                }
                if (ElecBaseboard(BaseboardNum).SurfacePtr(SurfNum) != 0) {
                    state.dataSurface->SurfIntConvSurfGetsRadiantHeat(ElecBaseboard(BaseboardNum).SurfacePtr(SurfNum)) = true;
                }

                AllFracsSummed += ElecBaseboard(BaseboardNum).FracDistribToSurf(SurfNum);
            } // Surfaces

            if (AllFracsSummed > (MaxFraction + 0.01)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", Summed radiant fractions for people + surface groups > 1.0");
                ErrorsFound = true;
            }
            if ((AllFracsSummed < (MaxFraction - 0.01)) &&
                (ElecBaseboard(BaseboardNum).FracRadiant >
                 MinFraction)) { // User didn't distribute all of the | radiation warn that some will be lost
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", Summed radiant fractions for people + surface groups < 1.0");
                ShowContinueError(state, "The rest of the radiant energy delivered by the baseboard heater will be lost");
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + cCurrentModuleObject + "Errors found getting input. Program terminates.");
        }

        for (BaseboardNum = 1; BaseboardNum <= NumElecBaseboards; ++BaseboardNum) {

            // Setup Report variables for the Electric Baseboards
            // CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Electric'
            SetupOutputVariable(state,
                                "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                ElecBaseboard(BaseboardNum).TotPower,
                                "System",
                                "Average",
                                ElecBaseboard(BaseboardNum).EquipName);

            SetupOutputVariable(state,
                                "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                ElecBaseboard(BaseboardNum).ConvPower,
                                "System",
                                "Average",
                                ElecBaseboard(BaseboardNum).EquipName);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                ElecBaseboard(BaseboardNum).RadPower,
                                "System",
                                "Average",
                                ElecBaseboard(BaseboardNum).EquipName);

            SetupOutputVariable(state,
                                "Baseboard Electricity Energy",
                                OutputProcessor::Unit::J,
                                ElecBaseboard(BaseboardNum).ElecUseLoad,
                                "System",
                                "Sum",
                                ElecBaseboard(BaseboardNum).EquipName,
                                _,
                                "Electricity",
                                "HEATING",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Baseboard Electricity Rate",
                                OutputProcessor::Unit::W,
                                ElecBaseboard(BaseboardNum).ElecUseRate,
                                "System",
                                "Average",
                                ElecBaseboard(BaseboardNum).EquipName);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                ElecBaseboard(BaseboardNum).TotEnergy,
                                "System",
                                "Sum",
                                ElecBaseboard(BaseboardNum).EquipName,
                                _,
                                "ENERGYTRANSFER",
                                "BASEBOARD",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                ElecBaseboard(BaseboardNum).ConvEnergy,
                                "System",
                                "Sum",
                                ElecBaseboard(BaseboardNum).EquipName);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                ElecBaseboard(BaseboardNum).RadEnergy,
                                "System",
                                "Sum",
                                ElecBaseboard(BaseboardNum).EquipName);
        }
    }

    void InitElectricBaseboard(EnergyPlusData &state, int const BaseboardNum, int const ControlledZoneNumSub, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       Feb 2010 Daeho Kang for radiant component

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the Baseboard units during simulation.

        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNode;
        int ZoneNum;
        int Loop;

        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;
        auto &NumElecBaseboards = state.dataElectBaseboardRad->NumElecBaseboards;
        auto &MyOneTimeFlag = state.dataElectBaseboardRad->MyOneTimeFlag;
        auto &MySizeFlag = state.dataElectBaseboardRad->MySizeFlag;
        auto &ZeroSourceSumHATsurf = state.dataElectBaseboardRad->ZeroSourceSumHATsurf;
        auto &QBBElecRadSource = state.dataElectBaseboardRad->QBBElecRadSource;
        auto &QBBElecRadSrcAvg = state.dataElectBaseboardRad->QBBElecRadSrcAvg;
        auto &LastQBBElecRadSrc = state.dataElectBaseboardRad->LastQBBElecRadSrc;
        auto &LastSysTimeElapsed = state.dataElectBaseboardRad->LastSysTimeElapsed;
        auto &LastTimeStepSys = state.dataElectBaseboardRad->LastTimeStepSys;
        auto &ZoneEquipmentListChecked = state.dataElectBaseboardRad->ZoneEquipmentListChecked;

        // Do the one time initializations
        if (MyOneTimeFlag) {
            // initialize the environment and sizing flags
            state.dataElectBaseboardRad->MyEnvrnFlag.allocate(NumElecBaseboards);
            MySizeFlag.allocate(NumElecBaseboards);
            ZeroSourceSumHATsurf.dimension(state.dataGlobal->NumOfZones, 0.0);
            QBBElecRadSource.dimension(NumElecBaseboards, 0.0);
            QBBElecRadSrcAvg.dimension(NumElecBaseboards, 0.0);
            LastQBBElecRadSrc.dimension(NumElecBaseboards, 0.0);
            LastSysTimeElapsed.dimension(NumElecBaseboards, 0.0);
            LastTimeStepSys.dimension(NumElecBaseboards, 0.0);
            state.dataElectBaseboardRad->MyEnvrnFlag = true;
            MySizeFlag = true;

            MyOneTimeFlag = false;
        }

        if (ElecBaseboard(BaseboardNum).ZonePtr <= 0)
            ElecBaseboard(BaseboardNum).ZonePtr = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

        if (!state.dataGlobal->SysSizingCalc && MySizeFlag(BaseboardNum)) {
            // for each coil, do the sizing once.
            SizeElectricBaseboard(state, BaseboardNum);
            MySizeFlag(BaseboardNum) = false;
        }

        // need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= NumElecBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(state, state.dataElectBaseboardRad->cCMO_BBRadiator_Electric, ElecBaseboard(Loop).EquipName)) continue;
                ShowSevereError(state,
                                "InitBaseboard: Unit=[" + state.dataElectBaseboardRad->cCMO_BBRadiator_Electric + ',' +
                                    ElecBaseboard(Loop).EquipName + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataElectBaseboardRad->MyEnvrnFlag(BaseboardNum)) {
            // Initialize
            ZeroSourceSumHATsurf = 0.0;
            QBBElecRadSource = 0.0;
            QBBElecRadSrcAvg = 0.0;
            LastQBBElecRadSrc = 0.0;
            LastSysTimeElapsed = 0.0;
            LastTimeStepSys = 0.0;

            state.dataElectBaseboardRad->MyEnvrnFlag(BaseboardNum) = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataElectBaseboardRad->MyEnvrnFlag(BaseboardNum) = true;
        }

        if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) {
            ZoneNum = ElecBaseboard(BaseboardNum).ZonePtr;
            ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(state, ZoneNum);
            QBBElecRadSrcAvg(BaseboardNum) = 0.0;
            LastQBBElecRadSrc(BaseboardNum) = 0.0;
            LastSysTimeElapsed(BaseboardNum) = 0.0;
            LastTimeStepSys(BaseboardNum) = 0.0;
        }

        // Do the every time step initializations
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
        ElecBaseboard(BaseboardNum).AirInletTemp = state.dataLoopNodes->Node(ZoneNode).Temp;
        ElecBaseboard(BaseboardNum).AirInletHumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;

        // Set the reporting variables to zero at each timestep.
        ElecBaseboard(BaseboardNum).TotPower = 0.0;
        ElecBaseboard(BaseboardNum).Power = 0.0;
        ElecBaseboard(BaseboardNum).ConvPower = 0.0;
        ElecBaseboard(BaseboardNum).RadPower = 0.0;
        ElecBaseboard(BaseboardNum).TotEnergy = 0.0;
        ElecBaseboard(BaseboardNum).Energy = 0.0;
        ElecBaseboard(BaseboardNum).ConvEnergy = 0.0;
        ElecBaseboard(BaseboardNum).RadEnergy = 0.0;
        ElecBaseboard(BaseboardNum).ElecUseLoad = 0.0;
        ElecBaseboard(BaseboardNum).ElecUseRate = 0.0;
    }

    void SizeElectricBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing electric baseboard components for which nominal capacities have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
        // calculated by numerically inverting the baseboard calculation routine.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::HeatingCapacitySizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeElectricBaseboard");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;          // autosized value of coil input field
        Real64 FracOfAutoSzCap;   // fraction of autosized capacity
        int FieldNum = 1;         // IDD numeric field number where input field description is found
        int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                          // HeatingCapacitySizing, etc.)
        bool PrintFlag;   // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                // FractionOfAutosizedHeatingCapacity )

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;

        state.dataSize->DataScalableCapSizingON = false;

        if (state.dataSize->CurZoneEqNum > 0) {

            CompType = state.dataElectBaseboardRad->cCMO_BBRadiator_Electric;
            CompName = ElecBaseboard(BaseboardNum).EquipName;
            state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
            state.dataSize->DataZoneNumber = ElecBaseboard(BaseboardNum).ZonePtr;
            SizingMethod = HeatingCapacitySizing;
            FieldNum = 1;
            PrintFlag = true;
            SizingString = state.dataElectBaseboardRad->ElecBaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
            CapSizingMethod = ElecBaseboard(BaseboardNum).HeatingCapMethod;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
            if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                if (CapSizingMethod == HeatingDesignCapacity) {
                    if (ElecBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                        CheckZoneSizing(state, CompType, CompName);
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesHeatLoad;
                    } else {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad = ElecBaseboard(BaseboardNum).ScaledHeatingCapacity;
                    }
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                    TempSize = ElecBaseboard(BaseboardNum).ScaledHeatingCapacity;
                } else if (CapSizingMethod == CapacityPerFloorArea) {
                    if (state.dataSize->ZoneSizingRunDone) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                            state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesHeatLoad;
                    }
                    TempSize = ElecBaseboard(BaseboardNum).ScaledHeatingCapacity * state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    state.dataSize->DataScalableCapSizingON = true;
                } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    CheckZoneSizing(state, CompType, CompName);
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = ElecBaseboard(BaseboardNum).ScaledHeatingCapacity;
                    ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                        state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).NonAirSysDesHeatLoad;
                    FracOfAutoSzCap = AutoSize;
                    bool ErrorsFound = false;
                    HeatingCapacitySizer sizerHeatingCapacity;
                    sizerHeatingCapacity.overrideSizingString(SizingString);
                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    FracOfAutoSzCap = sizerHeatingCapacity.size(state, FracOfAutoSzCap, ErrorsFound);
                    TempSize = FracOfAutoSzCap;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
                    state.dataSize->DataScalableCapSizingON = true;
                } else {
                    TempSize = ElecBaseboard(BaseboardNum).ScaledHeatingCapacity;
                }
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                ElecBaseboard(BaseboardNum).NominalCapacity = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                state.dataSize->DataScalableCapSizingON = false;
            }
        }
    }

    void CalcElectricBaseboard(EnergyPlusData &state, int const BaseboardNum, [[maybe_unused]] int const ControlledZoneNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       Feb 2010 Daeho Kang for radiant component
        //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the heat exchange rate in a Electric baseboard heater.
        // It includes radiant heat transfer to people and surfaces in a space, and the actual convective
        // system impact of a electric baseboard heater is determined after the radiant heat distribution.

        // METHODOLOGY EMPLOYED:
        // This is primarily modified from Convective Electric Baseboard. An existing algorithm of radiant
        // heat transfer calculation in the High Temperature Radiant System module is implemented.

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using Psychrometrics::PsyCpAirFnW;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SimpConvAirFlowSpeed(0.5); // m/s

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        Real64 AirInletTemp;
        Real64 CpAir;
        Real64 AirMassFlowRate;
        Real64 CapacitanceAir;
        Real64 Effic;
        Real64 AirOutletTemp;
        Real64 QBBCap;
        Real64 RadHeat;
        Real64 QZnReq;
        Real64 LoadMet;
        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;

        ZoneNum = ElecBaseboard(BaseboardNum).ZonePtr;
        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        AirInletTemp = ElecBaseboard(BaseboardNum).AirInletTemp;
        AirOutletTemp = AirInletTemp;
        CpAir = PsyCpAirFnW(ElecBaseboard(BaseboardNum).AirInletHumRat);
        AirMassFlowRate = SimpConvAirFlowSpeed;
        CapacitanceAir = CpAir * AirMassFlowRate;

        // Currently only the efficiency is used to calculate the electric consumption.  There could be some
        // thermal loss that could be accounted for with this efficiency input.
        Effic = ElecBaseboard(BaseboardNum).BaseboardEfficiency;

        if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) &&
            GetCurrentScheduleValue(state, ElecBaseboard(BaseboardNum).SchedPtr) > 0.0) {

            // If the load exceeds the capacity than the capacity is set to the BB limit.
            if (QZnReq > ElecBaseboard(BaseboardNum).NominalCapacity) {
                QBBCap = ElecBaseboard(BaseboardNum).NominalCapacity;
            } else {
                QBBCap = QZnReq;
            }
            RadHeat = QBBCap * ElecBaseboard(BaseboardNum).FracRadiant;
            state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) = RadHeat;

            if (ElecBaseboard(BaseboardNum).FracRadiant > 0.0) { // User defines radiant heat addition
                // Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
                DistributeBBElecRadGains(state);
                // Now "simulate" the system by recalculating the heat balances
                HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);
                // Here an assumption is made regarding radiant heat transfer to people.
                // While the radiant heat transfer to people array will be used by the thermal comfort
                // routines, the energy transfer to people would get lost from the perspective
                // of the heat balance.  So, to avoid this net loss of energy which clearly
                // gets added to the zones, we must account for it somehow.  This assumption
                // that all energy radiated to people is converted to convective energy is
                // not very precise, but at least it conserves energy. The system impact to heat balance
                // should include this.
                LoadMet = (SumHATsurf(state, ZoneNum) - state.dataElectBaseboardRad->ZeroSourceSumHATsurf(ZoneNum)) +
                          (QBBCap * ElecBaseboard(BaseboardNum).FracConvect) + (RadHeat * ElecBaseboard(BaseboardNum).FracDistribPerson);

                if (LoadMet < 0.0) {
                    // This basically means that SumHATsurf is LESS than ZeroSourceSumHATsurf which
                    // should not happen unless something unusual is happening like a fast change
                    // in temperature or some sort of change in internal load.  This is not a problem
                    // normally, but when LoadMet goes negative the choice is to either zero out
                    // the baseboard or give it another shot at getting an accurate reading on
                    // what is happening in the zone.  If it is still predicting a negative heating
                    // load, then zero everything out.
                    // First, turn off the baseboard:
                    Real64 TempZeroSourceSumHATsurf;
                    state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) = 0.0;
                    DistributeBBElecRadGains(state);
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);
                    TempZeroSourceSumHATsurf = SumHATsurf(state, ZoneNum);
                    // Now, turn it back on:
                    state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) = RadHeat;
                    DistributeBBElecRadGains(state);
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);
                    // Recalculate LoadMet with new ZeroSource... term and see if it is positive now.  If not, shut it down.
                    LoadMet = (SumHATsurf(state, ZoneNum) - TempZeroSourceSumHATsurf) + (QBBCap * ElecBaseboard(BaseboardNum).FracConvect) +
                              (RadHeat * ElecBaseboard(BaseboardNum).FracDistribPerson);
                    if (LoadMet < 0.0) {
                        // LoadMet is still less than zero so shut everything down
                        UpdateElectricBaseboardOff(LoadMet,
                                                   QBBCap,
                                                   RadHeat,
                                                   state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum),
                                                   ElecBaseboard(BaseboardNum).ElecUseRate,
                                                   AirOutletTemp,
                                                   AirInletTemp);
                    } else {
                        // Corrected LoadMet is now positive so use this and move forward with system operating
                        UpdateElectricBaseboardOn(
                            AirOutletTemp, ElecBaseboard(BaseboardNum).ElecUseRate, AirInletTemp, QBBCap, CapacitanceAir, Effic);
                    }
                } else {

                    UpdateElectricBaseboardOn(AirOutletTemp, ElecBaseboard(BaseboardNum).ElecUseRate, AirInletTemp, QBBCap, CapacitanceAir, Effic);
                }

            } else { // zero radiant fraction, no need of recalculation of heat balances

                LoadMet = QBBCap;
                UpdateElectricBaseboardOn(AirOutletTemp, ElecBaseboard(BaseboardNum).ElecUseRate, AirInletTemp, QBBCap, CapacitanceAir, Effic);
            }

        } else { // If there is an off condition the BB does nothing.

            UpdateElectricBaseboardOff(LoadMet,
                                       QBBCap,
                                       RadHeat,
                                       state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum),
                                       ElecBaseboard(BaseboardNum).ElecUseRate,
                                       AirOutletTemp,
                                       AirInletTemp);
        }

        // Assign calculated ones
        ElecBaseboard(BaseboardNum).AirOutletTemp = AirOutletTemp;
        ElecBaseboard(BaseboardNum).Power = QBBCap;
        ElecBaseboard(BaseboardNum).TotPower = LoadMet;
        ElecBaseboard(BaseboardNum).RadPower = RadHeat;
        ElecBaseboard(BaseboardNum).ConvPower = QBBCap - RadHeat;
    }

    void UpdateElectricBaseboardOff(Real64 &LoadMet,
                                    Real64 &QBBCap,
                                    Real64 &RadHeat,
                                    Real64 &QBBElecRadSrc,
                                    Real64 &ElecUseRate,
                                    Real64 &AirOutletTemp,
                                    Real64 const AirInletTemp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   August 2017

        // PURPOSE OF THIS SUBROUTINE: Zero out appropriate system variables when it is off

        QBBCap = 0.0;
        LoadMet = 0.0;
        RadHeat = 0.0;
        AirOutletTemp = AirInletTemp;
        QBBElecRadSrc = 0.0;
        ElecUseRate = 0.0;
    }

    void UpdateElectricBaseboardOn(
        Real64 &AirOutletTemp, Real64 &ElecUseRate, Real64 const AirInletTemp, Real64 const QBBCap, Real64 const CapacitanceAir, Real64 const Effic)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   August 2017

        // PURPOSE OF THIS SUBROUTINE: System is on, so calculate some of the result variables

        AirOutletTemp = AirInletTemp + QBBCap / CapacitanceAir;
        // This could be utilized somehow or even reported so the data structures are left in place
        // The Baseboard electric Load is calculated using the efficiency
        ElecUseRate = QBBCap / Effic;
    }

    void UpdateElectricBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //                      Rick Strand
        //       DATE WRITTEN   Nov 1997
        //                      February 2001
        //       MODIFIED       Feb 2010 Daeho Kang for radiant component

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // First, update the running average if necessary...
        if (state.dataElectBaseboardRad->LastSysTimeElapsed(BaseboardNum) == SysTimeElapsed) {
            state.dataElectBaseboardRad->QBBElecRadSrcAvg(BaseboardNum) -= state.dataElectBaseboardRad->LastQBBElecRadSrc(BaseboardNum) *
                                                                           state.dataElectBaseboardRad->LastTimeStepSys(BaseboardNum) /
                                                                           state.dataGlobal->TimeStepZone;
        }
        // Update the running average and the "last" values with the current values of the appropriate variables
        state.dataElectBaseboardRad->QBBElecRadSrcAvg(BaseboardNum) +=
            state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) * TimeStepSys / state.dataGlobal->TimeStepZone;

        state.dataElectBaseboardRad->LastQBBElecRadSrc(BaseboardNum) = state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum);
        state.dataElectBaseboardRad->LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed;
        state.dataElectBaseboardRad->LastTimeStepSys(BaseboardNum) = TimeStepSys;
    }

    void UpdateBBElecRadSourceValAvg(EnergyPlusData &state, bool &ElecBaseboardSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Feb 2010 Daeho Kang for baseboard

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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum; // DO loop counter for surface index

        ElecBaseboardSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(state.dataElectBaseboardRad->QBBElecRadSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (BaseboardNum = 1; BaseboardNum <= state.dataElectBaseboardRad->NumElecBaseboards; ++BaseboardNum) {
            if (state.dataElectBaseboardRad->QBBElecRadSrcAvg(BaseboardNum) != 0.0) {
                ElecBaseboardSysOn = true;
                break; // DO loop
            }
        }

        state.dataElectBaseboardRad->QBBElecRadSource = state.dataElectBaseboardRad->QBBElecRadSrcAvg;

        // QBBElecRadSource has been modified so we need to redistribute gains

        DistributeBBElecRadGains(state);
    }

    void DistributeBBElecRadGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Feb 2010 Daeho Kang for baseboard
        //                      April 2010 Brent Griffith, max limit to protect surface temperature calcs

        // PURPOSE OF THIS SUBROUTINE:
        // To distribute the gains from the electric basebaord heater
        // as specified in the user input file.  This includes distribution
        // of long wavelength radiant gains to surfaces and "people."

        // METHODOLOGY EMPLOYED:
        // We must cycle through all of the radiant systems because each
        // surface could feel the effect of more than one radiant system.
        // Note that the energy radiated to people is assumed to affect them
        // but them it is assumed to be convected to the air.

        // Using/Aliasing
        using DataHeatBalFanSys::MaxRadHeatFlux;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
        int BaseboardNum;         // Counter for the baseboard
        int SurfNum;              // Pointer to the Surface derived type
        int ZoneNum;              // Pointer to the Zone derived type
        Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

        // Initialize arrays
        state.dataHeatBalFanSys->QElecBaseboardSurf = 0.0;
        state.dataHeatBalFanSys->QElecBaseboardToPerson = 0.0;

        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;
        for (BaseboardNum = 1; BaseboardNum <= state.dataElectBaseboardRad->NumElecBaseboards; ++BaseboardNum) {

            if (ElecBaseboard(BaseboardNum).ZonePtr >
                0) { // issue 5806 can be zero during first calls to baseboards, will be set after all are modeled
                ZoneNum = ElecBaseboard(BaseboardNum).ZonePtr;
                state.dataHeatBalFanSys->QElecBaseboardToPerson(ZoneNum) +=
                    state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) * ElecBaseboard(BaseboardNum).FracDistribPerson;

                for (RadSurfNum = 1; RadSurfNum <= ElecBaseboard(BaseboardNum).TotSurfToDistrib; ++RadSurfNum) {
                    SurfNum = ElecBaseboard(BaseboardNum).SurfacePtr(RadSurfNum);
                    if (state.dataSurface->Surface(SurfNum).Area > SmallestArea) {
                        ThisSurfIntensity = (state.dataElectBaseboardRad->QBBElecRadSource(BaseboardNum) *
                                             ElecBaseboard(BaseboardNum).FracDistribToSurf(RadSurfNum) / state.dataSurface->Surface(SurfNum).Area);
                        state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum) += ThisSurfIntensity;
                        if (ThisSurfIntensity > MaxRadHeatFlux) {
                            ShowSevereError(state, "DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected");
                            ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                            ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                            ShowContinueError(state,
                                              "Occurs in " + state.dataElectBaseboardRad->cCMO_BBRadiator_Electric + " = " +
                                                  ElecBaseboard(BaseboardNum).EquipName);
                            ShowContinueError(state, format("Radiation intensity = {:.2R} [W/m2]", ThisSurfIntensity));
                            ShowContinueError(
                                state, "Assign a larger surface area or more surfaces in " + state.dataElectBaseboardRad->cCMO_BBRadiator_Electric);
                            ShowFatalError(state, "DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected");
                        }
                    } else {
                        ShowSevereError(state, "DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux");
                        ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                        ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataElectBaseboardRad->cCMO_BBRadiator_Electric + " = " +
                                              ElecBaseboard(BaseboardNum).EquipName);
                        ShowContinueError(
                            state, "Assign a larger surface area or more surfaces in " + state.dataElectBaseboardRad->cCMO_BBRadiator_Electric);
                        ShowFatalError(state, "DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux");
                    }
                }
            }
        }
    }

    void ReportElectricBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   Feb 2010

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        auto &ElecBaseboard = state.dataElectBaseboardRad->ElecBaseboard;
        ElecBaseboard(BaseboardNum).ElecUseLoad = ElecBaseboard(BaseboardNum).ElecUseRate * TimeStepSys * DataGlobalConstants::SecInHour;
        ElecBaseboard(BaseboardNum).TotEnergy = ElecBaseboard(BaseboardNum).TotPower * TimeStepSys * DataGlobalConstants::SecInHour;
        ElecBaseboard(BaseboardNum).Energy = ElecBaseboard(BaseboardNum).Power * TimeStepSys * DataGlobalConstants::SecInHour;
        ElecBaseboard(BaseboardNum).ConvEnergy = ElecBaseboard(BaseboardNum).ConvPower * TimeStepSys * DataGlobalConstants::SecInHour;
        ElecBaseboard(BaseboardNum).RadEnergy = ElecBaseboard(BaseboardNum).RadPower * TimeStepSys * DataGlobalConstants::SecInHour;
    }

    Real64 SumHATsurf(EnergyPlusData &state, int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;

        // Return value
        Real64 SumHATsurf;

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

} // namespace ElectricBaseboardRadiator

} // namespace EnergyPlus
