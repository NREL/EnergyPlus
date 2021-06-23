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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace VentilatedSlab {

    // Module containing the routines dealing with the Ventilated Slab

    // MODULE INFORMATION:
    //       AUTHOR         Young Tae Chae, Rick Strand
    //       DATE WRITTEN   June 2008
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Simulate Ventilated Slab Systems.

    // METHODOLOGY EMPLOYED:
    // Systems are modeled as a collection of components: radiant panel, outside air mixer,
    // fan, heating coil and/or cooling coil plus an integrated control
    // algorithm that adjusts the hot or cold water flow to meet the setpoint
    // condition.  Outside air mixing is handled locally as either fixed percent
    // or as attempting to meet a prescribed mixed air temperature.

    // REFERENCES:
    // ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
    // Fred Buhl's fan coil module (FanCoilUnits.cc)

    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::SmallAirVolFlow;
    using namespace ScheduleManager;
    using namespace Psychrometrics;
    using namespace FluidProperties;

    static std::string const fluidNameSteam("STEAM");
    static std::string const fluidNameWater("WATER");

    void SimVentilatedSlab(EnergyPlusData &state,
                           std::string const &CompName,   // name of the fan coil unit
                           int const ZoneNum,             // number of zone being served
                           bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           Real64 &PowerMet,              // Sensible power supplied (W)
                           Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                           int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //       RE-ENGINEERED
        // This is re-engineered by Rick Strand and Young T. Chae for Ventilated Slab (June, 2008)

        // PURPOSE OF THIS SUBROUTINE:
        // This is the main driver subroutine for the Ventilated Slab simulation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item; // index of ventilated slab being simulated

        if (state.dataVentilatedSlab->GetInputFlag) {
            GetVentilatedSlabInput(state);
            state.dataVentilatedSlab->GetInputFlag = false;
        }

        // Find the correct VentilatedSlabInput
        if (CompIndex == 0) {
            Item = UtilityRoutines::FindItemInList(CompName, state.dataVentilatedSlab->VentSlab);
            if (Item == 0) {
                ShowFatalError(state, "SimVentilatedSlab: system not found=" + CompName);
            }
            CompIndex = Item;
        } else {
            Item = CompIndex;
            if (Item > state.dataVentilatedSlab->NumOfVentSlabs || Item < 1) {
                ShowFatalError(state,
                               format("SimVentilatedSlab:  Invalid CompIndex passed={}, Number of Systems={}, Entered System name={}",
                                      Item,
                                      state.dataVentilatedSlab->NumOfVentSlabs,
                                      CompName));
            }
            if (state.dataVentilatedSlab->CheckEquipName(Item)) {
                if (CompName != state.dataVentilatedSlab->VentSlab(Item).Name) {
                    ShowFatalError(state,
                                   format("SimVentilatedSlab: Invalid CompIndex passed={}, System name={}, stored System Name for that index={}",
                                          Item,
                                          CompName,
                                          state.dataVentilatedSlab->VentSlab(Item).Name));
                }
                state.dataVentilatedSlab->CheckEquipName(Item) = false;
            }
        }

        state.dataSize->ZoneEqVentedSlab = true;

        InitVentilatedSlab(state, Item, ZoneNum, FirstHVACIteration);

        CalcVentilatedSlab(state, Item, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided);

        UpdateVentilatedSlab(state, Item, FirstHVACIteration);

        ReportVentilatedSlab(state, Item);

        state.dataSize->ZoneEqVentedSlab = false;
    }

    void GetVentilatedSlabInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine obtains the input for ventilated slab and sets
        // up the appropriate derived type.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // REFERENCES:
        // Fred Buhl's fan coil module (FanCoilUnits.cc)
        // Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.cc)
        // Rick Strand's Low temperature Radiant system (RadiantSystemLowTemp.cc)

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using NodeInputManager::GetOnlySingleNode;
        auto &GetWaterCoilMaxFlowRate(WaterCoils::GetCoilMaxWaterFlowRate);
        auto &GetSteamCoilMaxFlowRate(SteamCoils::GetCoilMaxWaterFlowRate);
        auto &GetHXAssistedCoilFlowRate(HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate);
        using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataLoopNode;
        using namespace DataSurfaceLists;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using FluidProperties::FindRefrigerant;
        using OutAirNodeManager::CheckAndAddAirNodeNumber;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const MeanAirTemperature("MeanAirTemperature");
        static std::string const MeanRadiantTemperature("MeanRadiantTemperature");
        static std::string const OperativeTemperature("OperativeTemperature");
        static std::string const OutsideAirDryBulbTemperature("OutdoorDryBulbTemperature");
        static std::string const OutsideAirWetBulbTemperature("OutdoorWetBulbTemperature");
        static std::string const SlabSurfaceTemperature("SurfaceTemperature");
        static std::string const SlabSurfaceDewPointTemperature("ZoneAirDewPointTemperature");
        static std::string const CurrentModuleObject("ZoneHVAC:VentilatedSlab");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;            // Used in GetObjectItem
        bool IsNotOK;            // TRUE if there was a problem with a list name
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumArgs;             // Unused variable that is part of a subroutine call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int Item;                // Item to be "gotten"
        int BaseNum;             // Temporary number for creating RadiantSystemTypes structure
        bool errFlag;            // interim error flag
        int SurfListNum;         // Index within the SurfList derived type for a surface list name
        // unused0309  INTEGER                         :: NumOfSurfListVB  ! Number of surface lists in the user input file
        int SurfNum;                   // DO loop counter for surfaces
        bool IsValid;                  // Set for outside air node check
        Array1D_string cAlphaArgs;     // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> rNumericArgs;  // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        bool SteamMessageNeeded;

        // Figure out how many Ventilated Slab Systems there are in the input file

        SteamMessageNeeded = true;
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumArgs, NumAlphas, NumNumbers);
        cAlphaArgs.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        rNumericArgs.dimension(NumNumbers, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        // make sure data is gotten for surface lists
        BaseNum = GetNumberOfSurfListVentSlab(state);

        state.dataVentilatedSlab->NumOfVentSlabs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        // Allocate the local derived type and do one-time initializations for all parts of it

        state.dataVentilatedSlab->VentSlab.allocate(state.dataVentilatedSlab->NumOfVentSlabs);
        state.dataVentilatedSlab->CheckEquipName.dimension(state.dataVentilatedSlab->NumOfVentSlabs, true);
        state.dataVentilatedSlab->VentSlabNumericFields.allocate(state.dataVentilatedSlab->NumOfVentSlabs);

        for (Item = 1; Item <= state.dataVentilatedSlab->NumOfVentSlabs;
             ++Item) { // Begin looping over the entire ventilated slab systems found in the input file...

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            state.dataVentilatedSlab->VentSlabNumericFields(Item).FieldNames.allocate(NumNumbers);
            state.dataVentilatedSlab->VentSlabNumericFields(Item).FieldNames = cNumericFields;
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), CurrentModuleObject, ErrorsFound);

            state.dataVentilatedSlab->VentSlab(Item).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataVentilatedSlab->VentSlab(Item).SchedName = state.dataIPShortCut->cAlphaArgs(2);
            if (lAlphaBlanks(2)) {
                state.dataVentilatedSlab->VentSlab(Item).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataVentilatedSlab->VentSlab(Item).SchedPtr =
                    GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
                if (state.dataVentilatedSlab->VentSlab(Item).SchedPtr == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(2) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(2) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            state.dataVentilatedSlab->VentSlab(Item).ZoneName = state.dataIPShortCut->cAlphaArgs(3);
            state.dataVentilatedSlab->VentSlab(Item).ZonePtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
            if (state.dataVentilatedSlab->VentSlab(Item).ZonePtr == 0) {
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

            state.dataVentilatedSlab->VentSlab(Item).SurfListName = state.dataIPShortCut->cAlphaArgs(4);
            SurfListNum = 0;
            //    IF (NumOfSlabLists > 0) SurfListNum = UtilityRoutines::FindItemInList(VentSlab(Item)%SurfListName, SlabList%Name, NumOfSlabLists)
            if (state.dataSurfLists->NumOfSurfListVentSlab > 0)
                SurfListNum = UtilityRoutines::FindItemInList(state.dataVentilatedSlab->VentSlab(Item).SurfListName, state.dataSurfLists->SlabList);
            if (SurfListNum > 0) { // Found a valid surface list
                state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces = state.dataSurfLists->SlabList(SurfListNum).NumOfSurfaces;
                state.dataVentilatedSlab->VentSlab(Item).ZName.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).ZPtr.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SurfaceName.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SurfacePtr.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).CDiameter.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).CLength.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).CNumbers.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SlabIn.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SlabOut.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);

                state.dataVentilatedSlab->MaxCloNumOfSurfaces =
                    max(state.dataVentilatedSlab->MaxCloNumOfSurfaces, state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                for (SurfNum = 1; SurfNum <= state.dataSurfLists->SlabList(SurfListNum).NumOfSurfaces; ++SurfNum) {
                    state.dataVentilatedSlab->VentSlab(Item).ZName(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).ZoneName(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).ZPtr(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).ZonePtr(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).SurfaceName(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).SurfName(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).SurfPtr(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).CDiameter(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).CoreDiameter(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).CLength(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).CoreLength(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).CNumbers(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).CoreNumbers(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).SlabIn(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).SlabInNodeName(SurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).SlabOut(SurfNum) = state.dataSurfLists->SlabList(SurfListNum).SlabOutNodeName(SurfNum);
                    if (state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum) != 0) {
                        state.dataSurface->SurfIntConvSurfHasActiveInIt(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)) = true;
                    }
                }

            } else { // User entered a single surface name rather than a surface list
                state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces = 1;
                state.dataVentilatedSlab->VentSlab(Item).SurfacePtr.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SurfaceName.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SurfaceFlowFrac.allocate(state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->MaxCloNumOfSurfaces =
                    max(state.dataVentilatedSlab->MaxCloNumOfSurfaces, state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces);
                state.dataVentilatedSlab->VentSlab(Item).SurfaceName(1) = state.dataVentilatedSlab->VentSlab(Item).SurfListName;
                state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1) =
                    UtilityRoutines::FindItemInList(state.dataVentilatedSlab->VentSlab(Item).SurfaceName(1), state.dataSurface->Surface);
                state.dataVentilatedSlab->VentSlab(Item).SurfaceFlowFrac(1) = 1.0;
                // Error checking for single surfaces
                if (state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1) == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(4) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(4) + "\" not found.");
                    ErrorsFound = true;
                } else if (state.dataSurface->SurfIsRadSurfOrVentSlabOrPool(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1))) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid Surface");
                    ShowContinueError(state,
                                      cAlphaFields(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                          "\" has been used in another radiant system or ventilated slab.");
                    ErrorsFound = true;
                }
                if (state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1) != 0) {
                    state.dataSurface->SurfIntConvSurfHasActiveInIt(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1)) = true;
                    state.dataSurface->SurfIsRadSurfOrVentSlabOrPool(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1)) = true;
                }
            }

            // Error checking for zones and construction information

            if (SurfListNum > 0) {

                for (SurfNum = 1; SurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++SurfNum) {

                    if (state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum) == 0) continue; // invalid surface -- detected earlier
                    if (state.dataVentilatedSlab->VentSlab(Item).ZPtr(SurfNum) == 0) continue;       // invalid zone -- detected earlier
                    if (state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction == 0)
                        continue; // invalid construction, detected earlier
                    if (!state.dataConstruction
                             ->Construct(state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction)
                             .SourceSinkPresent) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid surface=\"" +
                                            state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Name + "\".");
                        ShowContinueError(
                            state,
                            "Surface Construction does not have a source/sink, Construction name= \"" +
                                state.dataConstruction
                                    ->Construct(state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction)
                                    .Name +
                                "\".");
                        ErrorsFound = true;
                    }
                }
            } else {
                for (SurfNum = 1; SurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++SurfNum) {
                    if (state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum) == 0) continue; // invalid surface -- detected earlier
                    if (state.dataVentilatedSlab->VentSlab(Item).ZonePtr == 0) continue;             // invalid zone -- detected earlier
                    if (state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Zone !=
                        state.dataVentilatedSlab->VentSlab(Item).ZonePtr) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid surface=\"" +
                                            state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Name + "\".");
                        ShowContinueError(
                            state,
                            "Surface in Zone=" +
                                state.dataHeatBal->Zone(state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Zone)
                                    .Name +
                                ' ' + CurrentModuleObject + " in Zone=" + state.dataIPShortCut->cAlphaArgs(3));
                        ErrorsFound = true;
                    }
                    if (state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction == 0)
                        continue; // invalid construction, detected earlier
                    if (!state.dataConstruction
                             ->Construct(state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction)
                             .SourceSinkPresent) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid surface=\"" +
                                            state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Name + "\".");
                        ShowContinueError(
                            state,
                            "Surface Construction does not have a source/sink, Construction name= \"" +
                                state.dataConstruction
                                    ->Construct(state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(SurfNum)).Construction)
                                    .Name +
                                "\".");
                        ErrorsFound = true;
                    }
                }
            }

            state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow = state.dataIPShortCut->rNumericArgs(1);

            // Outside air information:
            state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
            state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow = state.dataIPShortCut->rNumericArgs(3);

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(5));
                if (SELECT_CASE_var == "VARIABLEPERCENT") {
                    state.dataVentilatedSlab->VentSlab(Item).OAControlType = state.dataVentilatedSlab->VariablePercent;
                    state.dataVentilatedSlab->VentSlab(Item).MaxOASchedName = state.dataIPShortCut->cAlphaArgs(6);
                    state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr =
                        GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7)); // convert schedule name to pointer
                    if (state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(7) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                        ErrorsFound = true;
                    } else if (!CheckScheduleValueMinMax(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr, ">=0", 0.0, "<=", 1.0)) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(7) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(7) + "\" values out of range [0,1].");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "FIXEDAMOUNT") {
                    state.dataVentilatedSlab->VentSlab(Item).OAControlType = state.dataVentilatedSlab->FixedOAControl;
                    state.dataVentilatedSlab->VentSlab(Item).MaxOASchedName = state.dataIPShortCut->cAlphaArgs(7);
                    state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr =
                        GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7)); // convert schedule name to pointer
                    if (state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(7) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                        ErrorsFound = true;
                    } else if (!CheckScheduleValueMinMax(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr, ">=0", 0.0)) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(7) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(7) + "\" values out of range (must be >=0).");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "FIXEDTEMPERATURE") {
                    state.dataVentilatedSlab->VentSlab(Item).OAControlType = state.dataVentilatedSlab->FixedTemperature;
                    state.dataVentilatedSlab->VentSlab(Item).TempSchedName = state.dataIPShortCut->cAlphaArgs(7);
                    state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr =
                        GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7)); // convert schedule name to pointer
                    if (state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(7) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(7) + "\" not found.");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(5) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(5) + "\".");
                }
            }

            state.dataVentilatedSlab->VentSlab(Item).MinOASchedName = state.dataIPShortCut->cAlphaArgs(6);
            state.dataVentilatedSlab->VentSlab(Item).MinOASchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6)); // convert schedule name to pointer
            if (state.dataVentilatedSlab->VentSlab(Item).MinOASchedPtr == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(6) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(6) + "\" not found.");
                ErrorsFound = true;
            }

            // System Configuration:
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SlabOnly")) {
                state.dataVentilatedSlab->VentSlab(Item).SysConfg = state.dataVentilatedSlab->SlabOnly;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SlabAndZone")) {
                state.dataVentilatedSlab->VentSlab(Item).SysConfg = state.dataVentilatedSlab->SlabAndZone;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SeriesSlabs")) {
                state.dataVentilatedSlab->VentSlab(Item).SysConfg = state.dataVentilatedSlab->SeriesSlabs;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(8) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(8) + "\".");
                ShowContinueError(state, "Control reset to SLAB ONLY Configuration.");
                state.dataVentilatedSlab->VentSlab(Item).SysConfg = state.dataVentilatedSlab->SlabOnly;
            }

            // Hollow Core information :
            state.dataVentilatedSlab->VentSlab(Item).CoreDiameter = state.dataIPShortCut->rNumericArgs(4);
            state.dataVentilatedSlab->VentSlab(Item).CoreLength = state.dataIPShortCut->rNumericArgs(5);
            state.dataVentilatedSlab->VentSlab(Item).CoreNumbers = state.dataIPShortCut->rNumericArgs(6);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SurfaceListNames")) {
                if (!lNumericBlanks(4)) {
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                         "\"  Core Diameter is not needed for the series slabs configuration- ignored.");
                    ShowContinueError(state, "...It has been asigned on SlabGroup.");
                }
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SurfaceListNames")) {
                if (!lNumericBlanks(5)) {
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                         "\"  Core Length is not needed for the series slabs configuration- ignored.");
                    ShowContinueError(state, "...It has been asigned on SlabGroup.");
                }
            }

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "SurfaceListNames")) {
                if (!lNumericBlanks(6)) {
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                         "\"  Core Numbers is not needed for the series slabs configuration- ignored.");
                    ShowContinueError(state, "...It has been asigned on SlabGroup.");
                }
            }

            // Process the temperature control type
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), OutsideAirDryBulbTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->ODBControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), OutsideAirWetBulbTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->OWBControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), OperativeTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->OPTControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), MeanAirTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->MATControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), MeanRadiantTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->MRTControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), SlabSurfaceTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->SURControl;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), SlabSurfaceDewPointTemperature)) {
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->DPTZControl;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(9) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(9) + "\".");
                ShowContinueError(state, "Control reset to ODB control.");
                state.dataVentilatedSlab->VentSlab(Item).ControlType = state.dataVentilatedSlab->ODBControl;
            }

            // Heating User Input Data For Ventilated Slab Control :

            // High Air Temp :
            state.dataVentilatedSlab->VentSlab(Item).HotAirHiTempSched = state.dataIPShortCut->cAlphaArgs(10);
            state.dataVentilatedSlab->VentSlab(Item).HotAirHiTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
            if ((state.dataVentilatedSlab->VentSlab(Item).HotAirHiTempSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(10) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(10) + "\" not found.");
                ErrorsFound = true;
            }

            // Low Air Temp :

            state.dataVentilatedSlab->VentSlab(Item).HotAirLoTempSched = state.dataIPShortCut->cAlphaArgs(11);
            state.dataVentilatedSlab->VentSlab(Item).HotAirLoTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
            if ((state.dataVentilatedSlab->VentSlab(Item).HotAirLoTempSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(11) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(11) + "\" not found.");
                ErrorsFound = true;
            }

            state.dataVentilatedSlab->VentSlab(Item).HotCtrlHiTempSched = state.dataIPShortCut->cAlphaArgs(12);
            state.dataVentilatedSlab->VentSlab(Item).HotCtrlHiTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(12));
            if ((state.dataVentilatedSlab->VentSlab(Item).HotCtrlHiTempSchedPtr == 0) && (!lAlphaBlanks(12))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(12) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(12) + "\" not found.");
                ErrorsFound = true;
            }

            state.dataVentilatedSlab->VentSlab(Item).HotCtrlLoTempSched = state.dataIPShortCut->cAlphaArgs(13);
            state.dataVentilatedSlab->VentSlab(Item).HotCtrlLoTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(13));
            if ((state.dataVentilatedSlab->VentSlab(Item).HotCtrlLoTempSchedPtr == 0) && (!lAlphaBlanks(13))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(13) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(13) + "\" not found.");
                ErrorsFound = true;
            }

            // Cooling User Input Data For Ventilated Slab Control :
            // Cooling High Temp Sch.
            state.dataVentilatedSlab->VentSlab(Item).ColdAirHiTempSched = state.dataIPShortCut->cAlphaArgs(13);
            state.dataVentilatedSlab->VentSlab(Item).ColdAirHiTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(14));
            if ((state.dataVentilatedSlab->VentSlab(Item).ColdAirHiTempSchedPtr == 0) && (!lAlphaBlanks(14))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(14) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(14) + "\" not found.");
                ErrorsFound = true;
            }

            // Cooling Low Temp Sch.

            state.dataVentilatedSlab->VentSlab(Item).ColdAirLoTempSched = state.dataIPShortCut->cAlphaArgs(15);
            state.dataVentilatedSlab->VentSlab(Item).ColdAirLoTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(15));
            if ((state.dataVentilatedSlab->VentSlab(Item).ColdAirLoTempSchedPtr == 0) && (!lAlphaBlanks(15))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(15) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(15) + "\" not found.");
                ErrorsFound = true;
            }

            // Cooling Control High Sch.

            state.dataVentilatedSlab->VentSlab(Item).ColdCtrlHiTempSched = state.dataIPShortCut->cAlphaArgs(16);
            state.dataVentilatedSlab->VentSlab(Item).ColdCtrlHiTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(16));
            if ((state.dataVentilatedSlab->VentSlab(Item).ColdCtrlHiTempSchedPtr == 0) && (!lAlphaBlanks(16))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(16) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(16) + "\" not found.");
                ErrorsFound = true;
            }

            // Cooling Control Low Sch.

            state.dataVentilatedSlab->VentSlab(Item).ColdCtrlLoTempSched = state.dataIPShortCut->cAlphaArgs(17);
            state.dataVentilatedSlab->VentSlab(Item).ColdCtrlLoTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(17));
            if ((state.dataVentilatedSlab->VentSlab(Item).ColdCtrlLoTempSchedPtr == 0) && (!lAlphaBlanks(17))) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(17) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(17) + "\" not found.");
                ErrorsFound = true;
            }

            // Main air nodes (except outside air node):
            // Refer the Unit Ventilator Air Node note

            // MJW CR7903 - Ventilated slab was not drawing properly in HVAC Diagram svg output
            //  This object is structured differently from other zone equipment in that it functions
            //  as both a parent and non-parent, and it has an implicit OA mixer.  This makes it difficult
            //  to register the nodes in a way that HVAC Diagram can understand and in a way that satisfies
            //  node connection tests.  Here's an explanation of the changes made for this CR:
            //      In general, nodes associated with the ventilated slab system (the overall parent object)
            //         are registered with "-SYSTEM" appended to the object type and object name
            //         This same suffix is also added later when SetUpCompSets is called, for the same reason
            //      In general, nodes associated with the implicit OA mixer object
            //         are registered with "-OA MIXER" appended to the object type and object name
            //      %ReturnAirNode is one inlet to the implicit oa mixer
            //         For SlabOnly and SeriesSlab this node does nothing,
            //             so DataLoopNode::NodeConnectionType::Internal,ObjectIsNotParent, -OA MIXER
            //         For SlabandZone, this node extracts air from the zone,
            //             so DataLoopNode::NodeConnectionType::Inlet,ObjectIsNotParent, -OA MIXER
            //         For SlabandZone, this node is also used to associate the whole system with a pair of zone inlet/exhaust nodes,
            //             so it is registered again as DataLoopNode::NodeConnectionType::Inlet,1,ObjectIsParent, -SYSTEM
            //      %RadInNode is the ultimate air inlet to the slab or series of slabs
            //         For all types of ventilated slab, this is DataLoopNode::NodeConnectionType::Inlet,ObjectIsNotParent
            //      %OAMixerOutNode is the outlet from the implicit OA mixer
            //         For all types of ventilated slab, this is DataLoopNode::NodeConnectionType::Outlet,ObjectIsNotParent
            //      %FanOutletNode is the outlet from the explicit fan child object (redundant input, should mine from child)
            //         For all types of ventilated slab, this is DataLoopNode::NodeConnectionType::Internal,ObjectIsParent
            //      %ZoneAirInNode is applicable only to SlabandZone configuration. It is the node that flows into the zone,
            //         and it is also the outlet from the ventilated slab section, so it must be registered twice
            //         First for the overall system, DataLoopNode::NodeConnectionType::Outlet,ObjectIsParent, -SYSTEM
            //         Second as the slab outlet, DataLoopNode::NodeConnectionType::Outlet,ObjectIsNotParent
            //      %OutsideAirNode is the outdoor air inlet to the OA mixer
            //         For all types of ventilated slab, this is DataLoopNode::NodeConnectionType::Inlet,ObjectIsNotParent, -OA MIXER

            if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) {

                state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(18),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject + "-OA MIXER",
                                                                                           state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).RadInNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(19),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);

                state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode = GetOnlySingleNode(state,
                                                                                            state.dataIPShortCut->cAlphaArgs(23),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject + "-OA MIXER",
                                                                                            state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).FanOutletNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(24),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsParent);

            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs) {

                state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(18),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject + "-OA MIXER",
                                                                                           state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).RadInNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(19),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);

                state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode = GetOnlySingleNode(state,
                                                                                            state.dataIPShortCut->cAlphaArgs(23),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject + "-OA MIXER",
                                                                                            state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).FanOutletNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(24),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsParent);

            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {

                state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(18),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject + "-SYSTEM",
                                                                                           state.dataIPShortCut->cAlphaArgs(1) + "-SYSTEM",
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsParent);
                state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(18),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject + "-OA MIXER",
                                                                                           state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).RadInNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(19),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode = GetOnlySingleNode(state,
                                                                                            state.dataIPShortCut->cAlphaArgs(23),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject + "-OA MIXER",
                                                                                            state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Primary,
                                                                                            ObjectIsNotParent);
                state.dataVentilatedSlab->VentSlab(Item).FanOutletNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(24),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsParent);
            }

            if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) {
                if (!lAlphaBlanks(20)) {
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" " + cAlphaFields(20) + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(20) + "\" not needed - ignored.");
                    ShowContinueError(state, "It is used for \"SlabAndZone\" only");
                }

            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
                if (lAlphaBlanks(20)) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(20) +
                                        " is blank and must be entered.");
                    ErrorsFound = true;
                }

                state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(20),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject + "-SYSTEM",
                                                                                           state.dataIPShortCut->cAlphaArgs(1) + "-SYSTEM",
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsParent);

                state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode = GetOnlySingleNode(state,
                                                                                           state.dataIPShortCut->cAlphaArgs(20),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                           ObjectIsNotParent);
            }

            //  Set connection type to 'Inlet', because it now uses an OA node
            state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode = GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(21),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject + "-OA MIXER",
                                                                                        state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                        DataLoopNode::NodeFluidType::Air,
                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                        NodeInputManager::compFluidStream::Primary,
                                                                                        ObjectIsNotParent);

            if (!lAlphaBlanks(21)) {
                CheckAndAddAirNodeNumber(state, state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode, IsValid);
                if (!IsValid) {
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                         "\", Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(21));
                }
            }

            state.dataVentilatedSlab->VentSlab(Item).AirReliefNode = GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(22),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject + "-OA MIXER",
                                                                                       state.dataIPShortCut->cAlphaArgs(1) + "-OA MIXER",
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::NodeConnectionType::ReliefAir,
                                                                                       NodeInputManager::compFluidStream::Primary,
                                                                                       ObjectIsNotParent);

            // Fan information:
            state.dataVentilatedSlab->VentSlab(Item).FanName = state.dataIPShortCut->cAlphaArgs(25);

            if (HVACFan::checkIfFanNameIsAFanSystem(state, state.dataVentilatedSlab->VentSlab(Item).FanName)) {
                state.dataVentilatedSlab->VentSlab(Item).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, state.dataVentilatedSlab->VentSlab(Item).FanName));
                state.dataVentilatedSlab->VentSlab(Item).Fan_Index =
                    HVACFan::getFanObjectVectorIndex(state, state.dataVentilatedSlab->VentSlab(Item).FanName);
            } else {
                bool isNotOkay(false);
                ValidateComponent(state, "FAN:CONSTANTVOLUME", state.dataVentilatedSlab->VentSlab(Item).FanName, isNotOkay, "GetPIUs");
                if (isNotOkay) {
                    ShowContinueError(state, "In " + CurrentModuleObject + " = " + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ErrorsFound = true;
                }
                state.dataVentilatedSlab->VentSlab(Item).FanType_Num = DataHVACGlobals::FanType_SimpleConstVolume;
            }

            if (state.dataVentilatedSlab->VentSlab(Item).OAControlType == state.dataVentilatedSlab->FixedOAControl) {
                state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow = state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow;
                state.dataVentilatedSlab->VentSlab(Item).MaxOASchedName = state.dataVentilatedSlab->VentSlab(Item).MinOASchedName;
                state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr =
                    GetScheduleIndex(state, state.dataVentilatedSlab->VentSlab(Item).MinOASchedName);
            }

            // Add fan to component sets array
            SetUpCompSets(state,
                          CurrentModuleObject + "-SYSTEM",
                          state.dataVentilatedSlab->VentSlab(Item).Name + "-SYSTEM",
                          "UNDEFINED",
                          state.dataIPShortCut->cAlphaArgs(25),
                          state.dataIPShortCut->cAlphaArgs(23),
                          state.dataIPShortCut->cAlphaArgs(24));

            // Coil options assign

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(26));
                if (SELECT_CASE_var == "HEATINGANDCOOLING") {
                    state.dataVentilatedSlab->VentSlab(Item).CoilOption = state.dataVentilatedSlab->BothOption;
                } else if (SELECT_CASE_var == "HEATING") {
                    state.dataVentilatedSlab->VentSlab(Item).CoilOption = state.dataVentilatedSlab->HeatingOption;
                } else if (SELECT_CASE_var == "COOLING") {
                    state.dataVentilatedSlab->VentSlab(Item).CoilOption = state.dataVentilatedSlab->CoolingOption;
                } else if (SELECT_CASE_var == "NONE") {
                    state.dataVentilatedSlab->VentSlab(Item).CoilOption = state.dataVentilatedSlab->NoneOption;
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(26) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(26) + "\".");
                    ErrorsFound = true;
                }
            }

            if (state.dataVentilatedSlab->VentSlab(Item).CoilOption == state.dataVentilatedSlab->BothOption ||
                state.dataVentilatedSlab->VentSlab(Item).CoilOption == state.dataVentilatedSlab->HeatingOption) {
                // Heating coil information:
                //        A27, \field Heating Coil Object Type
                //             \type choice
                //             \key Coil:Heating:Water
                //             \key Coil:Heating:Electric
                //             \key Coil:Heating:Fuel
                //             \key Coil:Heating:Steam
                //        A28, \field Heating Coil Name
                //             \type object-list
                //             \object-list HeatingCoilName

                // Heating coil information:
                if (!lAlphaBlanks(28)) {
                    state.dataVentilatedSlab->VentSlab(Item).HCoilPresent = true;
                    state.dataVentilatedSlab->VentSlab(Item).HCoilTypeCh = state.dataIPShortCut->cAlphaArgs(27);
                    errFlag = false;

                    {
                        auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(27));
                        if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                            state.dataVentilatedSlab->VentSlab(Item).HCoilType = state.dataVentilatedSlab->Heating_WaterCoilType;
                            state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
                        } else if (SELECT_CASE_var == "COIL:HEATING:STEAM") {
                            state.dataVentilatedSlab->VentSlab(Item).HCoilType = state.dataVentilatedSlab->Heating_SteamCoilType;
                            state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
                            state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex = FindRefrigerant(state, "Steam");
                            if (state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex == 0) {
                                ShowSevereError(state,
                                                CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "Steam Properties not found.");
                                if (SteamMessageNeeded)
                                    ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                                ErrorsFound = true;
                                SteamMessageNeeded = false;
                            }
                        } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                            state.dataVentilatedSlab->VentSlab(Item).HCoilType = state.dataVentilatedSlab->Heating_ElectricCoilType;
                        } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                            state.dataVentilatedSlab->VentSlab(Item).HCoilType = state.dataVentilatedSlab->Heating_GasCoilType;
                        } else {
                            ShowSevereError(state,
                                            CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(27) +
                                                "=\"" + state.dataIPShortCut->cAlphaArgs(27) + "\".");
                            ErrorsFound = true;
                            errFlag = true;
                        }
                    }
                    if (!errFlag) {
                        state.dataVentilatedSlab->VentSlab(Item).HCoilName = state.dataIPShortCut->cAlphaArgs(28);
                        ValidateComponent(state,
                                          state.dataIPShortCut->cAlphaArgs(27),
                                          state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                          IsNotOK,
                                          CurrentModuleObject);
                        if (IsNotOK) {
                            ShowContinueError(state,
                                              CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(28) +
                                                  "=\"" + state.dataIPShortCut->cAlphaArgs(28) + "\".");
                            ShowContinueError(state, "... not valid for " + cAlphaFields(27) + "=\"" + state.dataIPShortCut->cAlphaArgs(27) + "\".");
                            ErrorsFound = true;
                        }
                    }

                    state.dataVentilatedSlab->VentSlab(Item).MinVolHotWaterFlow = 0.0;
                    state.dataVentilatedSlab->VentSlab(Item).MinVolHotSteamFlow = 0.0;

                    // The heating coil control node is necessary for a hot water coil, but not necessary for an
                    // electric or gas coil.
                    if (state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_GasCoilType ||
                        state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_ElectricCoilType) {
                        if (!lAlphaBlanks(29)) {
                            ShowWarningError(state,
                                             CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" " + cAlphaFields(29) + "=\"" +
                                                 state.dataIPShortCut->cAlphaArgs(29) + "\" not needed - ignored.");
                            ShowContinueError(state, "..It is used for hot water coils only.");
                        }
                    } else {
                        if (lAlphaBlanks(29)) {
                            ShowSevereError(state,
                                            CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(29) +
                                                " is blank and must be entered.");
                            ErrorsFound = true;
                        }
                        state.dataVentilatedSlab->VentSlab(Item).HotControlNode = GetOnlySingleNode(state,
                                                                                                    state.dataIPShortCut->cAlphaArgs(29),
                                                                                                    ErrorsFound,
                                                                                                    CurrentModuleObject,
                                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                                    DataLoopNode::NodeConnectionType::Actuator,
                                                                                                    NodeInputManager::compFluidStream::Primary,
                                                                                                    ObjectIsParent);
                    }
                    state.dataVentilatedSlab->VentSlab(Item).HotControlOffset = 0.001;

                    if (state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_WaterCoilType) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow =
                            GetWaterCoilMaxFlowRate(state, "Coil:Heating:Water", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow =
                            GetWaterCoilMaxFlowRate(state, "Coil:Heating:Water", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    } else if (state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_SteamCoilType) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow =
                            GetSteamCoilMaxFlowRate(state, "Coil:Heating:Steam", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow =
                            GetSteamCoilMaxFlowRate(state, "Coil:Heating:Steam", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    }

                } else { // no heating coil
                    ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" missing heating coil.");
                    ShowContinueError(state,
                                      "a heating coil is required for " + cAlphaFields(26) + "=\"" + state.dataIPShortCut->cAlphaArgs(26) + "\".");
                    ErrorsFound = true;
                }
            }

            if (state.dataVentilatedSlab->VentSlab(Item).CoilOption == state.dataVentilatedSlab->BothOption ||
                state.dataVentilatedSlab->VentSlab(Item).CoilOption == state.dataVentilatedSlab->CoolingOption) {
                // Cooling coil information (if one is present):
                //        A30, \field Cooling Coil Object Type
                //             \type choice
                //             \key Coil:Cooling:Water
                //             \key Coil:Cooling:Water:DetailedGeometry
                //             \key CoilSystem:Cooling:Water:HeatExchangerAssisted
                //        A31, \field Cooling Coil Name
                //             \type object-list
                //             \object-list CoolingCoilsWater
                // Cooling coil information (if one is present):
                if (!lAlphaBlanks(31)) {
                    state.dataVentilatedSlab->VentSlab(Item).CCoilPresent = true;
                    state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh = state.dataIPShortCut->cAlphaArgs(30);
                    errFlag = false;

                    {
                        auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(30));
                        if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                            state.dataVentilatedSlab->VentSlab(Item).CCoilType = state.dataVentilatedSlab->Cooling_CoilWaterCooling;
                            state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
                            state.dataVentilatedSlab->VentSlab(Item).CCoilPlantName = state.dataIPShortCut->cAlphaArgs(31);
                        } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                            state.dataVentilatedSlab->VentSlab(Item).CCoilType = state.dataVentilatedSlab->Cooling_CoilDetailedCooling;
                            state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
                            state.dataVentilatedSlab->VentSlab(Item).CCoilPlantName = state.dataIPShortCut->cAlphaArgs(31);
                        } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                            state.dataVentilatedSlab->VentSlab(Item).CCoilType = state.dataVentilatedSlab->Cooling_CoilHXAssisted;
                            GetHXCoilTypeAndName(state,
                                                 state.dataIPShortCut->cAlphaArgs(30),
                                                 state.dataIPShortCut->cAlphaArgs(31),
                                                 ErrorsFound,
                                                 state.dataVentilatedSlab->VentSlab(Item).CCoilPlantType,
                                                 state.dataVentilatedSlab->VentSlab(Item).CCoilPlantName);
                            if (UtilityRoutines::SameString(state.dataVentilatedSlab->VentSlab(Item).CCoilPlantType, "Coil:Cooling:Water")) {
                                state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
                            } else if (UtilityRoutines::SameString(state.dataVentilatedSlab->VentSlab(Item).CCoilPlantType,
                                                                   "Coil:Cooling:Water:DetailedGeometry")) {
                                state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
                            } else {
                                ShowSevereError(state,
                                                "GetVentilatedSlabInput: " + CurrentModuleObject + "=\"" +
                                                    state.dataVentilatedSlab->VentSlab(Item).Name + "\", invalid");
                                ShowContinueError(state, "For: " + cAlphaFields(30) + "=\"" + state.dataIPShortCut->cAlphaArgs(30) + "\".");
                                ShowContinueError(state,
                                                  "Invalid Coil Type=" + state.dataVentilatedSlab->VentSlab(Item).CCoilPlantType +
                                                      ", Name=" + state.dataVentilatedSlab->VentSlab(Item).CCoilPlantName);
                                ShowContinueError(state, "must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(state,
                                            CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(29) +
                                                "=\"" + state.dataIPShortCut->cAlphaArgs(29) + "\".");
                            ErrorsFound = true;
                            errFlag = true;
                        }
                    }

                    if (!errFlag) {
                        state.dataVentilatedSlab->VentSlab(Item).CCoilName = state.dataIPShortCut->cAlphaArgs(31);
                        ValidateComponent(state,
                                          state.dataIPShortCut->cAlphaArgs(30),
                                          state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                          IsNotOK,
                                          "ZoneHVAC:VentilatedSlab ");
                        if (IsNotOK) {
                            ShowContinueError(state,
                                              CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(31) +
                                                  "=\"" + state.dataIPShortCut->cAlphaArgs(31) + "\".");
                            ShowContinueError(state, "... not valid for " + cAlphaFields(30) + "=\"" + state.dataIPShortCut->cAlphaArgs(30) + "\".");
                            ErrorsFound = true;
                        }
                    }

                    state.dataVentilatedSlab->VentSlab(Item).MinVolColdWaterFlow = 0.0;

                    state.dataVentilatedSlab->VentSlab(Item).ColdControlNode = GetOnlySingleNode(state,
                                                                                                 state.dataIPShortCut->cAlphaArgs(32),
                                                                                                 ErrorsFound,
                                                                                                 CurrentModuleObject,
                                                                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                                                                 DataLoopNode::NodeFluidType::Water,
                                                                                                 DataLoopNode::NodeConnectionType::Actuator,
                                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                                 ObjectIsParent);

                    if (lAlphaBlanks(32)) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(32) +
                                            " is blank and must be entered.");
                        ErrorsFound = true;
                    }

                    state.dataVentilatedSlab->VentSlab(Item).ColdControlOffset = 0.001;

                    if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilWaterCooling) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow =
                            GetWaterCoilMaxFlowRate(state, "Coil:Cooling:Water", state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
                    } else if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilDetailedCooling) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate(
                            state, "Coil:Cooling:Water:DetailedGeometry", state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
                    } else if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow = GetHXAssistedCoilFlowRate(
                            state, "CoilSystem:Cooling:Water:HeatExchangerAssisted", state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
                    }

                } else { // No Cooling Coil
                    ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" missing cooling coil.");
                    ShowContinueError(state,
                                      "a cooling coil is required for " + cAlphaFields(26) + "=\"" + state.dataIPShortCut->cAlphaArgs(26) + "\".");
                    ErrorsFound = true;
                }
            }
            if (!lAlphaBlanks(33)) {
                state.dataVentilatedSlab->VentSlab(Item).AvailManagerListName = state.dataIPShortCut->cAlphaArgs(33);
            }

            state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex = 0;
            if (!lAlphaBlanks(34)) {
                state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(34), state.dataSize->ZoneHVACSizing);
                if (state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex == 0) {
                    ShowSevereError(state, cAlphaFields(34) + " = " + state.dataIPShortCut->cAlphaArgs(34) + " not found.");
                    ShowContinueError(
                        state, "Occurs in " + state.dataVentilatedSlab->cMO_VentilatedSlab + " = " + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).CoilOption);
                if (SELECT_CASE_var == state.dataVentilatedSlab->BothOption) { // 'HeatingAndCooling'
                    // Add cooling coil to component sets array when present
                    SetUpCompSets(state,
                                  CurrentModuleObject + "-SYSTEM",
                                  state.dataVentilatedSlab->VentSlab(Item).Name + "-SYSTEM",
                                  state.dataIPShortCut->cAlphaArgs(30),
                                  state.dataIPShortCut->cAlphaArgs(31),
                                  state.dataIPShortCut->cAlphaArgs(24),
                                  "UNDEFINED");

                    // Add heating coil to component sets array when cooling coil present
                    SetUpCompSets(state,
                                  CurrentModuleObject + "-SYSTEM",
                                  state.dataVentilatedSlab->VentSlab(Item).Name + "-SYSTEM",
                                  state.dataIPShortCut->cAlphaArgs(27),
                                  state.dataIPShortCut->cAlphaArgs(28),
                                  "UNDEFINED",
                                  state.dataIPShortCut->cAlphaArgs(19));

                } else if (SELECT_CASE_var == state.dataVentilatedSlab->HeatingOption) { // 'Heating'
                    // Add heating coil to component sets array when no cooling coil present
                    SetUpCompSets(state,
                                  CurrentModuleObject + "-SYSTEM",
                                  state.dataVentilatedSlab->VentSlab(Item).Name + "-SYSTEM",
                                  state.dataIPShortCut->cAlphaArgs(27),
                                  state.dataIPShortCut->cAlphaArgs(28),
                                  state.dataIPShortCut->cAlphaArgs(24),
                                  state.dataIPShortCut->cAlphaArgs(19));

                } else if (SELECT_CASE_var == state.dataVentilatedSlab->CoolingOption) { // 'Cooling'
                    // Add cooling coil to component sets array when no heating coil present
                    SetUpCompSets(state,
                                  CurrentModuleObject + "-SYSTEM",
                                  state.dataVentilatedSlab->VentSlab(Item).Name + "-SYSTEM",
                                  state.dataIPShortCut->cAlphaArgs(30),
                                  state.dataIPShortCut->cAlphaArgs(31),
                                  state.dataIPShortCut->cAlphaArgs(24),
                                  state.dataIPShortCut->cAlphaArgs(19));

                } else if (SELECT_CASE_var == state.dataVentilatedSlab->NoneOption) {

                } else {
                }
            }

        } // ...loop over all of the ventilated slab found in the input file

        cAlphaArgs.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        rNumericArgs.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) ShowFatalError(state, CurrentModuleObject + " errors occurred in input.  Program terminates.");

        // Setup Report variables for the VENTILATED SLAB
        for (Item = 1; Item <= state.dataVentilatedSlab->NumOfVentSlabs; ++Item) {
            //   CALL SetupOutputVariable(state, 'Ventilated Slab Direct Heat Loss Rate [W]', &
            //                             VentSlab(Item)%DirectHeatLossRate,'System', &
            //                             'Average', VentSlab(Item)%Name)
            //   CALL SetupOutputVariable(state, 'Ventilated Slab Direct Heat Loss [W]',        &
            //                             VentSlab(Item)%DirectHeatLoss,'System', &
            //                             'Sum', VentSlab(Item)%Name)
            //   CALL SetupOutputVariable(state, 'Ventilated Slab Direct Heat Gain Rate [W]',        &
            //                             VentSlab(Item)%DirectHeatGainRate,'System', &
            //                            'Average', VentSlab(Item)%Name)
            //   CALL SetupOutputVariable(state, 'Ventilated Slab Direct Heat Gain [J]',        &
            //                           VentSlab(Item)%DirectHeatGain,'System', &
            //                             'Sum', VentSlab(Item)%Name)
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).RadHeatingPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).RadHeatingEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Radiant Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).RadCoolingPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Radiant Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).RadCoolingEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).HeatCoilPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).HeatCoilEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).LateCoolCoilPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).LateCoolCoilEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataVentilatedSlab->VentSlab(Item).AirMassFlowRate,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Fan Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataVentilatedSlab->VentSlab(Item).ElecFanPower,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            //! Note that the ventilated slab fan electric is NOT metered because this value is already metered through the fan component
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataVentilatedSlab->VentSlab(Item).ElecFanEnergy,
                                "System",
                                "Sum",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Inlet Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataVentilatedSlab->VentSlab(Item).SlabInTemp,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Outlet Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataVentilatedSlab->VentSlab(Item).SlabOutTemp,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Zone Inlet Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataVentilatedSlab->VentSlab(Item).ZoneInletTemp,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Return Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataVentilatedSlab->VentSlab(Item).ReturnAirTemp,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Fan Outlet Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataVentilatedSlab->VentSlab(Item).FanOutletTemp,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
            SetupOutputVariable(state,
                                "Zone Ventilated Slab Fan Availability Status",
                                OutputProcessor::Unit::None,
                                state.dataVentilatedSlab->VentSlab(Item).AvailStatus,
                                "System",
                                "Average",
                                state.dataVentilatedSlab->VentSlab(Item).Name);
        }
    }

    void InitVentilatedSlab(EnergyPlusData &state,
                            int const Item,               // index for the current ventilated slab
                            int const VentSlabZoneNum,    // number of zone being served
                            bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes all of the data elements which are necessary
        // to simulate a Ventilated Slab.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::VentilatedSlab_Num;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitVentilatedSlab");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadNum;         // Number of the radiant system (DO loop counter)
        int RadSurfNum;     // Number of the radiant system surface (DO loop counter)
        int SurfNum;        // Intermediate variable for keeping track of the surface number
        int ZoneNum;        // Intermediate variable for keeping track of the zone number
        int AirRelNode;     // relief air node number in Ventilated Slab loop
        int ColdConNode;    // cold water control node number in Ventilated Slab loop
        int HotConNode;     // hot water control node number in Ventilated Slab loop
        int InNode;         // inlet node number in Ventilated Slab loop
        int OutNode;        // outlet node number in Ventilated Slab loop
        int OutsideAirNode; // outside air node number in Ventilated Slab loop
        Real64 RhoAir;      // air density at InNode
        Real64 TempSteamIn;
        Real64 SteamDensity;
        int ZoneAirInNode;
        int MixOut;
        Real64 rho;
        bool errFlag;

        // Do the one time initializations

        if (state.dataVentilatedSlab->MyOneTimeFlag) {
            state.dataVentilatedSlab->MyEnvrnFlag.allocate(state.dataVentilatedSlab->NumOfVentSlabs);
            state.dataVentilatedSlab->MySizeFlag.allocate(state.dataVentilatedSlab->NumOfVentSlabs);
            state.dataVentilatedSlab->MyPlantScanFlag.allocate(state.dataVentilatedSlab->NumOfVentSlabs);
            state.dataVentilatedSlab->MyZoneEqFlag.allocate(state.dataVentilatedSlab->NumOfVentSlabs);
            state.dataVentilatedSlab->ZeroSourceSumHATsurf.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataVentilatedSlab->QRadSysSrcAvg.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataVentilatedSlab->LastQRadSysSrc.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataVentilatedSlab->LastSysTimeElapsed.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataVentilatedSlab->LastTimeStepSys.dimension(state.dataSurface->TotSurfaces, 0.0);

            // Initialize total areas for all radiant systems
            for (RadNum = 1; RadNum <= state.dataVentilatedSlab->NumOfVentSlabs; ++RadNum) {
                state.dataVentilatedSlab->VentSlab(RadNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= state.dataVentilatedSlab->VentSlab(RadNum).NumOfSurfaces; ++SurfNum) {
                    state.dataVentilatedSlab->VentSlab(RadNum).TotalSurfaceArea +=
                        state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(RadNum).SurfacePtr(SurfNum)).Area;
                }
            }
            state.dataVentilatedSlab->MyEnvrnFlag = true;
            state.dataVentilatedSlab->MySizeFlag = true;
            state.dataVentilatedSlab->MyPlantScanFlag = true;
            state.dataVentilatedSlab->MyZoneEqFlag = true;
            state.dataVentilatedSlab->MyOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (state.dataVentilatedSlab->MyZoneEqFlag(Item)) { // initialize the name of each availability manager list and zone number
                ZoneComp(VentilatedSlab_Num).ZoneCompAvailMgrs(Item).AvailManagerListName =
                    state.dataVentilatedSlab->VentSlab(Item).AvailManagerListName;
                ZoneComp(VentilatedSlab_Num).ZoneCompAvailMgrs(Item).ZoneNum = VentSlabZoneNum;
                state.dataVentilatedSlab->MyZoneEqFlag(Item) = false;
            }
            state.dataVentilatedSlab->VentSlab(Item).AvailStatus = ZoneComp(VentilatedSlab_Num).ZoneCompAvailMgrs(Item).AvailStatus;
        }

        if (state.dataVentilatedSlab->MyPlantScanFlag(Item) && allocated(state.dataPlnt->PlantLoop)) {
            if ((state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) ||
                (state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating)) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                        state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum,
                                        state.dataVentilatedSlab->VentSlab(Item).HWLoopNum,
                                        state.dataVentilatedSlab->VentSlab(Item).HWLoopSide,
                                        state.dataVentilatedSlab->VentSlab(Item).HWBranchNum,
                                        state.dataVentilatedSlab->VentSlab(Item).HWCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError(state,
                                      "Reference Unit=\"" + state.dataVentilatedSlab->VentSlab(Item).Name + "\", type=ZoneHVAC:VentilatedSlab");
                    ShowFatalError(state, "InitVentilatedSlab: Program terminated due to previous condition(s).");
                }

                state.dataVentilatedSlab->VentSlab(Item).HotCoilOutNodeNum =
                    state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum)
                        .LoopSide(state.dataVentilatedSlab->VentSlab(Item).HWLoopSide)
                        .Branch(state.dataVentilatedSlab->VentSlab(Item).HWBranchNum)
                        .Comp(state.dataVentilatedSlab->VentSlab(Item).HWCompNum)
                        .NodeNumOut;
            }
            if ((state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) ||
                (state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling)) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataVentilatedSlab->VentSlab(Item).CCoilPlantName,
                                        state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum,
                                        state.dataVentilatedSlab->VentSlab(Item).CWLoopNum,
                                        state.dataVentilatedSlab->VentSlab(Item).CWLoopSide,
                                        state.dataVentilatedSlab->VentSlab(Item).CWBranchNum,
                                        state.dataVentilatedSlab->VentSlab(Item).CWCompNum,
                                        errFlag);
                if (errFlag) {
                    ShowContinueError(state,
                                      "Reference Unit=\"" + state.dataVentilatedSlab->VentSlab(Item).Name + "\", type=ZoneHVAC:VentilatedSlab");
                    ShowFatalError(state, "InitVentilatedSlab: Program terminated due to previous condition(s).");
                }
                state.dataVentilatedSlab->VentSlab(Item).ColdCoilOutNodeNum =
                    state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum)
                        .LoopSide(state.dataVentilatedSlab->VentSlab(Item).CWLoopSide)
                        .Branch(state.dataVentilatedSlab->VentSlab(Item).CWBranchNum)
                        .Comp(state.dataVentilatedSlab->VentSlab(Item).CWCompNum)
                        .NodeNumOut;
            } else {
                if (state.dataVentilatedSlab->VentSlab(Item).CCoilPresent)
                    ShowFatalError(state,
                                   "InitVentilatedSlab: Unit=" + state.dataVentilatedSlab->VentSlab(Item).Name +
                                       ", invalid cooling coil type. Program terminated.");
            }
            state.dataVentilatedSlab->MyPlantScanFlag(Item) = false;
        } else if (state.dataVentilatedSlab->MyPlantScanFlag(Item) && !state.dataGlobal->AnyPlantInModel) {
            state.dataVentilatedSlab->MyPlantScanFlag(Item) = false;
        }

        // need to check all Ventilated Slab units to see if they are on Zone Equipment List or issue warning
        if (!state.dataVentilatedSlab->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataVentilatedSlab->ZoneEquipmentListChecked = true;
            for (RadNum = 1; RadNum <= state.dataVentilatedSlab->NumOfVentSlabs; ++RadNum) {
                if (CheckZoneEquipmentList(state, state.dataVentilatedSlab->cMO_VentilatedSlab, state.dataVentilatedSlab->VentSlab(RadNum).Name))
                    continue;
                ShowSevereError(state,
                                "InitVentilatedSlab: Ventilated Slab Unit=[" + state.dataVentilatedSlab->cMO_VentilatedSlab + ',' +
                                    state.dataVentilatedSlab->VentSlab(RadNum).Name +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataVentilatedSlab->MySizeFlag(Item) && !state.dataVentilatedSlab->MyPlantScanFlag(Item)) {

            SizeVentilatedSlab(state, Item);

            state.dataVentilatedSlab->MySizeFlag(Item) = false;
        }

        // Do the one time initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataVentilatedSlab->MyEnvrnFlag(Item) && !state.dataVentilatedSlab->MyPlantScanFlag(Item)) {

            // Coil Part
            InNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
            OutNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
            HotConNode = state.dataVentilatedSlab->VentSlab(Item).HotControlNode;
            ColdConNode = state.dataVentilatedSlab->VentSlab(Item).ColdControlNode;
            OutsideAirNode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;
            RhoAir = state.dataEnvrn->StdRhoAir;

            // Radiation Panel Part
            state.dataVentilatedSlab->ZeroSourceSumHATsurf = 0.0;
            state.dataVentilatedSlab->QRadSysSrcAvg = 0.0;
            state.dataVentilatedSlab->LastQRadSysSrc = 0.0;
            state.dataVentilatedSlab->LastSysTimeElapsed = 0.0;
            state.dataVentilatedSlab->LastTimeStepSys = 0.0;
            if (state.dataVentilatedSlab->NumOfVentSlabs > 0) {
                for (auto &e : state.dataVentilatedSlab->VentSlab) {
                    e.RadHeatingPower = 0.0;
                    e.RadHeatingEnergy = 0.0;
                    e.RadCoolingPower = 0.0;
                    e.RadCoolingEnergy = 0.0;
                }
            }

            // set the initial Temperature of Return Air

            // set the mass flow rates from the input volume flow rates
            state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow = RhoAir * state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow;
            state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow = RhoAir * state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow;
            state.dataVentilatedSlab->VentSlab(Item).MinOutAirMassFlow = RhoAir * state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow;
            if (state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow > state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow) {
                state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
                state.dataVentilatedSlab->VentSlab(Item).MinOutAirMassFlow =
                    state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow *
                    (state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow / state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow);
                ShowWarningError(state,
                                 "Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for " +
                                     state.dataVentilatedSlab->VentSlab(Item).Name);
            }

            // set the node max and min mass flow rates
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMin = 0.0;

            state.dataLoopNodes->Node(OutNode).MassFlowRateMax = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;

            state.dataLoopNodes->Node(InNode).MassFlowRateMax = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
            state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;

            if (state.dataVentilatedSlab->VentSlab(Item).HCoilPresent) { // Only initialize these if a heating coil is actually present

                if (state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating &&
                    !state.dataVentilatedSlab->MyPlantScanFlag(Item)) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidIndex,
                                           RoutineName);

                    state.dataVentilatedSlab->VentSlab(Item).MaxHotWaterFlow = rho * state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow;
                    state.dataVentilatedSlab->VentSlab(Item).MinHotWaterFlow = rho * state.dataVentilatedSlab->VentSlab(Item).MinVolHotWaterFlow;

                    InitComponentNodes(state,
                                       state.dataVentilatedSlab->VentSlab(Item).MinHotWaterFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).MaxHotWaterFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).HotControlNode,
                                       state.dataVentilatedSlab->VentSlab(Item).HotCoilOutNodeNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWLoopNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWLoopSide,
                                       state.dataVentilatedSlab->VentSlab(Item).HWBranchNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWCompNum);
                }
                if (state.dataVentilatedSlab->VentSlab(Item).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating &&
                    !state.dataVentilatedSlab->MyPlantScanFlag(Item)) {
                    TempSteamIn = 100.00;
                    SteamDensity = GetSatDensityRefrig(
                        state, fluidNameSteam, TempSteamIn, 1.0, state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex, RoutineName);
                    state.dataVentilatedSlab->VentSlab(Item).MaxHotSteamFlow =
                        SteamDensity * state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow;
                    state.dataVentilatedSlab->VentSlab(Item).MinHotSteamFlow =
                        SteamDensity * state.dataVentilatedSlab->VentSlab(Item).MinVolHotSteamFlow;

                    InitComponentNodes(state,
                                       state.dataVentilatedSlab->VentSlab(Item).MinHotSteamFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).MaxHotSteamFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).HotControlNode,
                                       state.dataVentilatedSlab->VentSlab(Item).HotCoilOutNodeNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWLoopNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWLoopSide,
                                       state.dataVentilatedSlab->VentSlab(Item).HWBranchNum,
                                       state.dataVentilatedSlab->VentSlab(Item).HWCompNum);
                }
            } //(VentSlab(Item)%HCoilPresent)

            if (state.dataVentilatedSlab->VentSlab(Item).CCoilPresent && !state.dataVentilatedSlab->MyPlantScanFlag(Item)) {
                // Only initialize these if a cooling coil is actually present
                if ((state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) ||
                    (state.dataVentilatedSlab->VentSlab(Item).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling)) {
                    rho = GetDensityGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidName,
                                           DataGlobalConstants::CWInitConvTemp,
                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidIndex,
                                           RoutineName);
                    state.dataVentilatedSlab->VentSlab(Item).MaxColdWaterFlow = rho * state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow;
                    state.dataVentilatedSlab->VentSlab(Item).MinColdWaterFlow = rho * state.dataVentilatedSlab->VentSlab(Item).MinVolColdWaterFlow;
                    InitComponentNodes(state,
                                       state.dataVentilatedSlab->VentSlab(Item).MinColdWaterFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).MaxColdWaterFlow,
                                       state.dataVentilatedSlab->VentSlab(Item).ColdControlNode,
                                       state.dataVentilatedSlab->VentSlab(Item).ColdCoilOutNodeNum,
                                       state.dataVentilatedSlab->VentSlab(Item).CWLoopNum,
                                       state.dataVentilatedSlab->VentSlab(Item).CWLoopSide,
                                       state.dataVentilatedSlab->VentSlab(Item).CWBranchNum,
                                       state.dataVentilatedSlab->VentSlab(Item).CWCompNum);
                }
            }

            state.dataVentilatedSlab->MyEnvrnFlag(Item) = false;

        } // ...end start of environment inits

        if (!state.dataGlobal->BeginEnvrnFlag) {

            state.dataVentilatedSlab->MyEnvrnFlag(Item) = true;
        }

        // These initializations are done every iteration...
        InNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
        OutNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        OutsideAirNode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;
        AirRelNode = state.dataVentilatedSlab->VentSlab(Item).AirReliefNode;
        ZoneAirInNode = state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode;
        MixOut = state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode;

        // First, set the flow conditions up so that there is flow through the ventilated
        // slab system(this will be shut down if the system is not available or there
        // is no load
        state.dataLoopNodes->Node(InNode).MassFlowRate = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(OutNode).MassFlowRate = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMinAvail = state.dataVentilatedSlab->VentSlab(Item).MaxAirMassFlow;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
        state.dataLoopNodes->Node(AirRelNode).MassFlowRate = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
        state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;
        state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = state.dataVentilatedSlab->VentSlab(Item).OutAirMassFlow;

        // Initialize the relief air (same as inlet conditions to the Ventilated Slab ..
        // Note that mass flow rates will be taken care of later.
        state.dataLoopNodes->Node(AirRelNode) = state.dataLoopNodes->Node(InNode);
        state.dataVentilatedSlab->OAMassFlowRate = 0.0;

        // Just in case the system is off and conditions do not get sent through
        // the system for some reason, set the outlet conditions equal to the inlet
        // conditions of the ventilated slab mixer
        state.dataLoopNodes->Node(OutNode).Temp = state.dataLoopNodes->Node(InNode).Temp;
        state.dataLoopNodes->Node(OutNode).Press = state.dataLoopNodes->Node(InNode).Press;
        state.dataLoopNodes->Node(OutNode).HumRat = state.dataLoopNodes->Node(InNode).HumRat;
        state.dataLoopNodes->Node(OutNode).Enthalpy = state.dataLoopNodes->Node(InNode).Enthalpy;

        // These initializations only need to be done once at the start of the iterations...
        if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) {
            // Initialize the outside air conditions...
            state.dataLoopNodes->Node(OutsideAirNode).Temp = state.dataLoopNodes->Node(OutsideAirNode).OutAirDryBulb;
            state.dataLoopNodes->Node(OutsideAirNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(OutsideAirNode).Press = state.dataEnvrn->OutBaroPress;

            // The first pass through in a particular time step
            ZoneNum = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
            state.dataVentilatedSlab->ZeroSourceSumHATsurf(ZoneNum) =
                SumHATsurf(state, ZoneNum); // Set this to figure what part of the load the radiant system meets
            for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
                state.dataVentilatedSlab->QRadSysSrcAvg(SurfNum) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
                state.dataVentilatedSlab->LastQRadSysSrc(SurfNum) =
                    0.0; // At the start of a time step, reset to zero so average calculation can begin again
                state.dataVentilatedSlab->LastSysTimeElapsed(SurfNum) =
                    0.0; // At the start of a time step, reset to zero so average calculation can begin again
                state.dataVentilatedSlab->LastTimeStepSys(SurfNum) =
                    0.0; // At the start of a time step, reset to zero so average calculation can begin again
            }
        }
    }

    void SizeVentilatedSlab(EnergyPlusData &state, int const Item)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Ventilated Slab components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::CoolingCapacitySizing;
        using DataHVACGlobals::HeatingAirflowSizing;
        using DataHVACGlobals::HeatingCapacitySizing;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using HVACHXAssistedCoolingCoil::GetHXCoilType;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
        using PlantUtilities::MyPlantSizingIndex;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetCoilSteamOutletNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeVentilatedSlab");

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum; // index of plant sizing object for 1st heating loop
        int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
        bool ErrorsFound;
        Real64 DesCoilLoad;
        Real64 TempSteamIn;
        Real64 EnthSteamInDry;
        Real64 EnthSteamOutWet;
        Real64 LatentHeatSteam;
        Real64 SteamDensity;
        int CoilWaterInletNode(0);
        int CoilWaterOutletNode(0);
        int CoilSteamInletNode(0);
        int CoilSteamOutletNode(0);
        std::string CoolingCoilName;
        std::string CoolingCoilType;
        Real64 rho;
        Real64 Cp;
        int DummyWaterIndex(1);
        bool IsAutoSize;                // Indicator to autosize
        Real64 MaxAirVolFlowDes;        // Autosized maximum air flow for reporting
        Real64 MaxAirVolFlowUser;       // Hardsized maximum air flow for reporting
        Real64 OutAirVolFlowDes;        // Autosized outdoor air flow for reporting
        Real64 OutAirVolFlowUser;       // Hardsized outdoor air flow for reporting
        Real64 MinOutAirVolFlowDes;     // Autosized minimum outdoor air flow for reporting
        Real64 MinOutAirVolFlowUser;    // Hardsized minimum outdoor air flow for reporting
        Real64 MaxVolHotWaterFlowDes;   // Autosized maximum hot water flow for reporting
        Real64 MaxVolHotWaterFlowUser;  // Hardsized maximum hot water flow for reporting
        Real64 MaxVolHotSteamFlowDes;   // Autosized maximum hot steam flow for reporting
        Real64 MaxVolHotSteamFlowUser;  // Hardsized maximum hot steam flow for reporting
        Real64 MaxVolColdWaterFlowDes;  // Autosized maximum cold water flow for reporting
        Real64 MaxVolColdWaterFlowUser; // Hardsized maximum cold water flow for reporting
        std::string CompName;           // component name
        std::string CompType;           // component type
        std::string SizingString;       // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;                // autosized value of coil input field
        int FieldNum = 2;               // IDD numeric field number where input field description is found
        int SizingMethod;  // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                           // HeatingCapacitySizing, etc.)
        bool PrintFlag;    // TRUE when sizing information is reported in the eio file
        int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
        int SAFMethod(0);  // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                           // FractionOfAutosizedHeatingAirflow ...)
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                // FractionOfAutosizedHeatingCapacity )
        Real64 CoolingAirVolFlowScalable; // cooling airvolume for rate determined using scalable sizing method
        Real64 HeatingAirVolFlowScalable; // heating airvolume for rate determined using scalable sizing method
        bool DoWaterCoilSizing = false;   // if TRUE do water coil sizing calculation
        Real64 WaterCoilSizDeltaT;        // water coil deltaT for design water flow rate autosizing
        int CoilNum;                      // index of water coil object

        DoWaterCoilSizing = false;
        WaterCoilSizDeltaT = 0.0;
        CoilNum = 0;
        PltSizCoolNum = 0;
        PltSizHeatNum = 0;
        ErrorsFound = false;
        IsAutoSize = false;
        MaxAirVolFlowDes = 0.0;
        MaxAirVolFlowUser = 0.0;
        OutAirVolFlowDes = 0.0;
        OutAirVolFlowUser = 0.0;
        MinOutAirVolFlowDes = 0.0;
        MinOutAirVolFlowUser = 0.0;
        MaxVolHotWaterFlowDes = 0.0;
        MaxVolHotWaterFlowUser = 0.0;
        MaxVolHotSteamFlowDes = 0.0;
        MaxVolHotSteamFlowUser = 0.0;
        MaxVolColdWaterFlowDes = 0.0;
        MaxVolColdWaterFlowUser = 0.0;
        CoolingAirVolFlowScalable = 0.0;
        HeatingAirVolFlowScalable = 0.0;
        state.dataSize->DataScalableSizingON = false;
        state.dataSize->DataScalableCapSizingON = false;
        CompType = state.dataVentilatedSlab->cMO_VentilatedSlab;
        CompName = state.dataVentilatedSlab->VentSlab(Item).Name;
        state.dataSize->DataZoneNumber = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
        if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataSize->DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
        } else {
            state.dataSize->DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
        }
        state.dataSize->DataFanIndex = state.dataVentilatedSlab->VentSlab(Item).Fan_Index;
        // ventilated slab unit is always blow thru
        state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;

        if (state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex > 0) {
            zoneHVACIndex = state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex;
            // N1 , \field Maximum Supply Air Flow Rate
            FieldNum = 1;
            PrintFlag = true;
            SizingString = state.dataVentilatedSlab->VentSlabNumericFields(Item).FieldNames(FieldNum) + " [m3/s]";
            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod > 0) {
                SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
                SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                    SAFMethod == FractionOfAutosizedCoolingAirflow) {
                    if (SAFMethod == SupplyAirFlowRate) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                            ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                        }
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    } else if (SAFMethod == FlowPerFloorArea) {
                        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                        ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                        state.dataSize->DataScalableSizingON = true;
                    } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                        state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                    } else {
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    }
                    CoolingAirFlowSizer sizingCoolingAirFlow;
                    std::string stringOverride = "Maximum Air Flow Rate [m3/s]";
                    if (state.dataGlobal->isEpJSON) stringOverride = "maximum_air_flow_rate [m3/s]";
                    sizingCoolingAirFlow.overrideSizingString(stringOverride);
                    // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);

                } else if (SAFMethod == FlowPerCoolingCapacity) {
                    SizingMethod = CoolingCapacitySizing;
                    TempSize = AutoSize;
                    PrintFlag = false;
                    state.dataSize->DataScalableSizingON = true;
                    state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                    CoolingCapacitySizer sizerCoolingCapacity;
                    sizerCoolingCapacity.overrideSizingString(SizingString);
                    sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    state.dataSize->DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                    state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    PrintFlag = true;
                    TempSize = AutoSize;
                    CoolingAirFlowSizer sizingCoolingAirFlow;
                    std::string stringOverride = "Maximum Air Flow Rate [m3/s]";
                    if (state.dataGlobal->isEpJSON) stringOverride = "maximum_air_flow_rate [m3/s]";
                    sizingCoolingAirFlow.overrideSizingString(stringOverride);
                    // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                }
            }
            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod > 0) {
                SizingMethod = HeatingAirflowSizing;
                SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                    SAFMethod == FractionOfAutosizedHeatingAirflow) {
                    if (SAFMethod == SupplyAirFlowRate) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                            ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                            ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                        }
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                    } else if (SAFMethod == FlowPerFloorArea) {
                        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                        ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow *
                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                        state.dataSize->DataScalableSizingON = true;
                    } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                        state.dataSize->DataFracOfAutosizedHeatingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                    } else {
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                    }
                    HeatingAirFlowSizer sizingHeatingAirFlow;
                    sizingHeatingAirFlow.overrideSizingString(SizingString);
                    // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                } else if (SAFMethod == FlowPerHeatingCapacity) {
                    SizingMethod = HeatingCapacitySizing;
                    TempSize = AutoSize;
                    PrintFlag = false;
                    state.dataSize->DataScalableSizingON = true;
                    state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                    HeatingCapacitySizer sizerHeatingCapacity;
                    sizerHeatingCapacity.overrideSizingString(SizingString);
                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    state.dataSize->DataAutosizedHeatingCapacity = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                    state.dataSize->DataFlowPerHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                    SizingMethod = HeatingAirflowSizing;
                    PrintFlag = true;
                    TempSize = AutoSize;
                    HeatingAirFlowSizer sizingHeatingAirFlow;
                    sizingHeatingAirFlow.overrideSizingString(SizingString);
                    // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                }
            }
            // DataScalableSizingON = false;
            state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow = max(CoolingAirVolFlowScalable, HeatingAirVolFlowScalable);
        } else {
            // no scalble sizing method has been specified. Sizing proceeds using the method
            // specified in the zoneHVAC object
            // N1 , \field Maximum Supply Air Flow Rate
            FieldNum = 1;
            PrintFlag = true;
            SizingString = state.dataVentilatedSlab->VentSlabNumericFields(Item).FieldNames(FieldNum) + " [m3/s]";
            TempSize = state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow;
            SystemAirFlowSizer sizerSystemAirFlow;
            sizerSystemAirFlow.overrideSizingString(SizingString);
            // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow = sizerSystemAirFlow.size(state, TempSize, ErrorsFound);
        }

        IsAutoSize = false;
        if (state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                if (state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                 state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow);
                }
            } else { // Autosize or hard-size with sizing run
                CheckZoneSizing(state, state.dataVentilatedSlab->cMO_VentilatedSlab, state.dataVentilatedSlab->VentSlab(Item).Name);
                OutAirVolFlowDes = state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow;
                if (IsAutoSize) {
                    state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow = OutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "Design Size Maximum Outdoor Air Flow Rate [m3/s]",
                                                 OutAirVolFlowDes);
                } else {
                    if (state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0) {
                        OutAirVolFlowUser = state.dataVentilatedSlab->VentSlab(Item).OutAirVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "Design Size Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowDes,
                                                     "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(OutAirVolFlowDes - OutAirVolFlowUser) / OutAirVolFlowUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" +
                                                state.dataVentilatedSlab->VentSlab(Item).Name + "\".");
                                ShowContinueError(state, format("User-Specified Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                if (state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "User-Specified Minimum Outdoor Air Flow Rate [m3/s]",
                                                 state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow);
                }
            } else {
                CheckZoneSizing(state, state.dataVentilatedSlab->cMO_VentilatedSlab, state.dataVentilatedSlab->VentSlab(Item).Name);
                MinOutAirVolFlowDes =
                    min(state.dataSize->FinalZoneSizing(CurZoneEqNum).MinOA, state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow);
                if (MinOutAirVolFlowDes < SmallAirVolFlow) {
                    MinOutAirVolFlowDes = 0.0;
                }
                if (IsAutoSize) {
                    state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow = MinOutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "Design Size Minimum Outdoor Air Flow Rate [m3/s]",
                                                 MinOutAirVolFlowDes);
                } else { // Hard-size with sizing data
                    if (state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow > 0.0 && MinOutAirVolFlowDes > 0.0) {
                        MinOutAirVolFlowUser = state.dataVentilatedSlab->VentSlab(Item).MinOutAirVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "Design Size Minimum Outdoor Air Flow Rate [m3/s]",
                                                     MinOutAirVolFlowDes,
                                                     "User-Specified Minimum Outdoor Air Flow Rate [m3/s]",
                                                     MinOutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MinOutAirVolFlowDes - MinOutAirVolFlowUser) / MinOutAirVolFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" +
                                                state.dataVentilatedSlab->VentSlab(Item).Name + "\".");
                                ShowContinueError(state,
                                                  format("User-Specified Minimum Outdoor Air Flow Rate of {:.5R} [m3/s]", MinOutAirVolFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Minimum Outdoor Air Flow Rate of {:.5R} [m3/s]", MinOutAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_WaterCoilType) {

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                    if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "User-Specified Maximum Hot Water Flow [m3/s]",
                                                     state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow);
                    }
                } else { // Autosize or hard-size with sizing run
                    CheckZoneSizing(state, state.dataVentilatedSlab->cMO_VentilatedSlab, state.dataVentilatedSlab->VentSlab(Item).Name);

                    CoilWaterInletNode = WaterCoils::GetCoilWaterInletNode(
                        state, "Coil:Heating:Water", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    CoilWaterOutletNode = WaterCoils::GetCoilWaterOutletNode(
                        state, "Coil:Heating:Water", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizHeatNum = MyPlantSizingIndex(state,
                                                           "Coil:Heating:Water",
                                                           state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                           CoilWaterInletNode,
                                                           CoilWaterOutletNode,
                                                           ErrorsFound);
                        CoilNum = WaterCoils::GetWaterCoilIndex(
                            state, "COIL:HEATING:WATER", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                            WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                            DoWaterCoilSizing = true;
                        } else {
                            if (PltSizHeatNum > 0) {
                                WaterCoilSizDeltaT = state.dataSize->PlantSizData(PltSizHeatNum).DeltaT;
                                DoWaterCoilSizing = true;
                            } else {
                                DoWaterCoilSizing = false;
                                // If there is no heating Plant Sizing object and autosizing was requested, issue fatal error message
                                ShowSevereError(state, "Autosizing of water flow requires a heating loop Sizing:Plant object");
                                ShowContinueError(state,
                                                  "Occurs in " + state.dataVentilatedSlab->cMO_VentilatedSlab +
                                                      " Object=" + state.dataVentilatedSlab->VentSlab(Item).Name);
                                ErrorsFound = true;
                            }
                        }
                        if (DoWaterCoilSizing) {
                            if (state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow >= SmallAirVolFlow) {
                                SizingMethod = HeatingCapacitySizing;
                                if (state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex > 0) {
                                    zoneHVACIndex = state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex;
                                    CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                        if (CapSizingMethod == HeatingDesignCapacity) {
                                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                    state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            } else {
                                                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            }
                                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity *
                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                                            state.dataSize->DataScalableCapSizingON = true;
                                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                            state.dataSize->DataFracOfAutosizedHeatingCapacity =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            TempSize = AutoSize;
                                            state.dataSize->DataScalableCapSizingON = true;
                                        }
                                    }
                                    SizingString = "";
                                    PrintFlag = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                    DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                                    state.dataSize->DataScalableCapSizingON = false;
                                } else {
                                    SizingString = "";
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                    DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                                }
                                rho = GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidName,
                                                       DataGlobalConstants::HWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidIndex,
                                                       RoutineName);
                                Cp = GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidName,
                                                           DataGlobalConstants::HWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).HWLoopNum).FluidIndex,
                                                           RoutineName);
                                MaxVolHotWaterFlowDes = DesCoilLoad / (WaterCoilSizDeltaT * Cp * rho);
                            } else {
                                MaxVolHotWaterFlowDes = 0.0;
                            }
                        }
                    }

                    if (IsAutoSize) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "Design Size Maximum Hot Water Flow [m3/s]",
                                                     MaxVolHotWaterFlowDes);
                    } else { // Hard-size with sizing data
                        if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0) {
                            MaxVolHotWaterFlowUser = state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow;
                            BaseSizer::reportSizerOutput(state,
                                                         state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                         state.dataVentilatedSlab->VentSlab(Item).Name,
                                                         "Design Size Maximum Hot Water Flow [m3/s]",
                                                         MaxVolHotWaterFlowDes,
                                                         "User-Specified Maximum Hot Water Flow [m3/s]",
                                                         MaxVolHotWaterFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser) / MaxVolHotWaterFlowUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" +
                                                    state.dataVentilatedSlab->VentSlab(Item).Name + "\".");
                                    ShowContinueError(state,
                                                      format("User-Specified Maximum Hot Water Flow of {:.5R} [m3/s]", MaxVolHotWaterFlowUser));
                                    ShowContinueError(
                                        state, format("differs from Design Size Maximum Hot Water Flow of {:.5R} [m3/s]", MaxVolHotWaterFlowDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow = 0.0;
        }

        IsAutoSize = false;
        if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_SteamCoilType) {

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                    if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "User-Specified Maximum Steam Flow [m3/s]",
                                                     state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow);
                    }
                } else { // Autosize or hard-size with sizing run
                    CheckZoneSizing(state, "ZoneHVAC:VentilatedSlab", state.dataVentilatedSlab->VentSlab(Item).Name);

                    CoilSteamInletNode =
                        GetCoilSteamInletNode(state, "Coil:Heating:Steam", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    CoilSteamOutletNode =
                        GetCoilSteamOutletNode(state, "Coil:Heating:Steam", state.dataVentilatedSlab->VentSlab(Item).HCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizHeatNum = MyPlantSizingIndex(state,
                                                           "Coil:Heating:Steam",
                                                           state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                           CoilSteamInletNode,
                                                           CoilSteamOutletNode,
                                                           ErrorsFound);
                        if (PltSizHeatNum > 0) {
                            if (state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow >= SmallAirVolFlow) {
                                SizingMethod = HeatingCapacitySizing;
                                if (state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex > 0) {
                                    zoneHVACIndex = state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex;
                                    CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                        if (CapSizingMethod == HeatingDesignCapacity) {
                                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                    state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            } else {
                                                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            }
                                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity *
                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                                            state.dataSize->DataScalableCapSizingON = true;
                                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                            state.dataSize->DataFracOfAutosizedHeatingCapacity =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            TempSize = AutoSize;
                                            state.dataSize->DataScalableCapSizingON = true;
                                        }
                                    }
                                    SizingString = "";
                                    PrintFlag = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                    DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                                    state.dataSize->DataScalableCapSizingON = false;
                                } else {
                                    SizingString = "";
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                    DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                                }
                                TempSteamIn = 100.00;
                                EnthSteamInDry = GetSatEnthalpyRefrig(
                                    state, fluidNameSteam, TempSteamIn, 1.0, state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex, RoutineName);
                                EnthSteamOutWet = GetSatEnthalpyRefrig(
                                    state, fluidNameSteam, TempSteamIn, 0.0, state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex, RoutineName);
                                LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                                SteamDensity = GetSatDensityRefrig(
                                    state, fluidNameSteam, TempSteamIn, 1.0, state.dataVentilatedSlab->VentSlab(Item).HCoil_FluidIndex, RoutineName);
                                Cp = GetSpecificHeatGlycol(state, fluidNameWater, DataGlobalConstants::HWInitConvTemp, DummyWaterIndex, RoutineName);
                                rho = GetDensityGlycol(state, fluidNameWater, DataGlobalConstants::HWInitConvTemp, DummyWaterIndex, RoutineName);
                                MaxVolHotSteamFlowDes =
                                    DesCoilLoad / ((state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp * rho) + SteamDensity * LatentHeatSteam);
                            } else {
                                MaxVolHotSteamFlowDes = 0.0;
                            }
                        } else {
                            ShowSevereError(state, "Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                            ShowContinueError(state, "Occurs in ZoneHVAC:VentilatedSlab Object=" + state.dataVentilatedSlab->VentSlab(Item).Name);
                            ErrorsFound = true;
                        }
                    }
                    if (IsAutoSize) {
                        state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "Design Size Maximum Steam Flow [m3/s]",
                                                     MaxVolHotSteamFlowDes);
                    } else {
                        if (state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0) {
                            MaxVolHotSteamFlowUser = state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow;
                            BaseSizer::reportSizerOutput(state,
                                                         state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                         state.dataVentilatedSlab->VentSlab(Item).Name,
                                                         "Design Size Maximum Steam Flow [m3/s]",
                                                         MaxVolHotSteamFlowDes,
                                                         "User-Specified Maximum Steam Flow [m3/s]",
                                                         MaxVolHotSteamFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser) / MaxVolHotSteamFlowUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" +
                                                    state.dataVentilatedSlab->VentSlab(Item).Name + "\".");
                                    ShowContinueError(state, format("User-Specified Maximum Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowUser));
                                    ShowContinueError(state,
                                                      format("differs from Design Size Maximum Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            state.dataVentilatedSlab->VentSlab(Item).MaxVolHotSteamFlow = 0.0;
        }

        IsAutoSize = false;
        if (state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                if (state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "User-Specified Maximum Cold Water Flow [m3/s]",
                                                 state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow);
                }
            } else {
                CheckZoneSizing(state, state.dataVentilatedSlab->cMO_VentilatedSlab, state.dataVentilatedSlab->VentSlab(Item).Name);
                if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
                    CoolingCoilName = GetHXDXCoilName(
                        state, state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh, state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
                    CoolingCoilType = GetHXCoilType(
                        state, state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh, state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
                } else {
                    CoolingCoilName = state.dataVentilatedSlab->VentSlab(Item).CCoilName;
                    CoolingCoilType = state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh;
                }
                CoilWaterInletNode = WaterCoils::GetCoilWaterInletNode(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                CoilWaterOutletNode = WaterCoils::GetCoilWaterOutletNode(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                if (IsAutoSize) {
                    PltSizCoolNum = MyPlantSizingIndex(state, CoolingCoilType, CoolingCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound);
                    CoilNum = WaterCoils::GetWaterCoilIndex(state, CoolingCoilType, CoolingCoilName, ErrorsFound);
                    if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                        WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                        DoWaterCoilSizing = true;
                    } else {
                        if (PltSizCoolNum > 0) {
                            WaterCoilSizDeltaT = state.dataSize->PlantSizData(PltSizCoolNum).DeltaT;
                            DoWaterCoilSizing = true;
                        } else {
                            DoWaterCoilSizing = false;
                            // If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
                            ShowSevereError(state, "Autosizing of water flow requires a cooling loop Sizing:Plant object");
                            ShowContinueError(state,
                                              "Occurs in " + state.dataVentilatedSlab->cMO_VentilatedSlab +
                                                  " Object=" + state.dataVentilatedSlab->VentSlab(Item).Name);
                            ErrorsFound = true;
                        }
                    }
                    if (DoWaterCoilSizing) {
                        if (state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow >= SmallAirVolFlow) {
                            SizingMethod = CoolingCapacitySizing;
                            if (state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex > 0) {
                                zoneHVACIndex = state.dataVentilatedSlab->VentSlab(Item).HVACSizingIndex;
                                CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
                                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                    CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                    if (CapSizingMethod == CoolingDesignCapacity) {
                                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                                            ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesCoolingLoad =
                                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                        } else {
                                            state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                        }
                                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                                        ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                                        ZoneEqSizing(CurZoneEqNum).DesCoolingLoad =
                                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity *
                                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                                        state.dataSize->DataScalableCapSizingON = true;
                                    } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                        state.dataSize->DataFracOfAutosizedHeatingCapacity =
                                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                        state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                        TempSize = AutoSize;
                                        state.dataSize->DataScalableCapSizingON = true;
                                    }
                                }
                                SizingString = "";
                                PrintFlag = false;
                                CoolingCapacitySizer sizerCoolingCapacity;
                                sizerCoolingCapacity.overrideSizingString(SizingString);
                                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                                state.dataSize->DataScalableCapSizingON = false;
                            } else {
                                SizingString = "";
                                PrintFlag = false;
                                TempSize = AutoSize;
                                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                CoolingCapacitySizer sizerCoolingCapacity;
                                sizerCoolingCapacity.overrideSizingString(SizingString);
                                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                DesCoilLoad = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                            }
                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidName,
                                                   5.,
                                                   state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidName,
                                                       5.,
                                                       state.dataPlnt->PlantLoop(state.dataVentilatedSlab->VentSlab(Item).CWLoopNum).FluidIndex,
                                                       RoutineName);
                            MaxVolColdWaterFlowDes = DesCoilLoad / (WaterCoilSizDeltaT * Cp * rho);
                        } else {
                            MaxVolColdWaterFlowDes = 0.0;
                        }
                    }
                }
                if (IsAutoSize) {
                    state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow = MaxVolColdWaterFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                 state.dataVentilatedSlab->VentSlab(Item).Name,
                                                 "Design Size Maximum Cold Water Flow [m3/s]",
                                                 MaxVolColdWaterFlowDes);
                } else {
                    if (state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow > 0.0 && MaxVolColdWaterFlowDes > 0.0) {
                        MaxVolColdWaterFlowUser = state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataVentilatedSlab->cMO_VentilatedSlab,
                                                     state.dataVentilatedSlab->VentSlab(Item).Name,
                                                     "Design Size Maximum Cold Water Flow [m3/s]",
                                                     MaxVolColdWaterFlowDes,
                                                     "User-Specified Maximum Cold Water Flow [m3/s]",
                                                     MaxVolColdWaterFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser) / MaxVolColdWaterFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" +
                                                state.dataVentilatedSlab->VentSlab(Item).Name + "\".");
                                ShowContinueError(state, format("User-Specified Maximum Cold Water Flow of {:.5R} [m3/s]", MaxVolColdWaterFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Maximum Cold Water Flow of {:.5R} [m3/s]", MaxVolColdWaterFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
            CoolingCoilName = GetHXDXCoilName(
                state, state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh, state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
            CoolingCoilType = GetHXCoilType(
                state, state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh, state.dataVentilatedSlab->VentSlab(Item).CCoilName, ErrorsFound);
        } else {
            CoolingCoilName = state.dataVentilatedSlab->VentSlab(Item).CCoilName;
            CoolingCoilType = state.dataVentilatedSlab->VentSlab(Item).CCoilTypeCh;
        }
        WaterCoils::SetCoilDesFlow(state, CoolingCoilType, CoolingCoilName, state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow, ErrorsFound);
        WaterCoils::SetCoilDesFlow(state,
                                   state.dataVentilatedSlab->VentSlab(Item).HCoilTypeCh,
                                   state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                   state.dataVentilatedSlab->VentSlab(Item).MaxAirVolFlow,
                                   ErrorsFound);

        if (CurZoneEqNum > 0) {
            ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow = state.dataVentilatedSlab->VentSlab(Item).MaxVolHotWaterFlow;
            ZoneEqSizing(CurZoneEqNum).MaxCWVolFlow = state.dataVentilatedSlab->VentSlab(Item).MaxVolColdWaterFlow;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void CalcVentilatedSlab(EnergyPlusData &state,
                            int &Item,                     // number of the current ventilated slab being simulated
                            int const ZoneNum,             // number of zone being served
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            Real64 &PowerMet,              // power supplied (W)
                            Real64 &LatOutputProvided      // latent capacity supplied (kg/s)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine mainly controls the action of the Ventilated Slab
        // (or more exactly, it controls the amount of outside air brought in)
        // based on the user input for controls and the defined controls
        // algorithms.

        // METHODOLOGY EMPLOYED:
        // Ventilated slab is controlled based on user input and what is happening in the
        // simulation.  There are various cases to consider:
        // 1. OFF: Unit is schedule off or there is no load on it.  All flow
        //    rates are set to zero and the temperatures are set to zone conditions
        //    (except for the outside air inlet).
        // 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
        //    and variable percent control is specified.  The outside air fraction
        //    is set to the minimum outside air fraction (schedule based) and the
        //    heating coil is activated.
        // 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
        //    and fixed temperature control is specified.  The outside air fraction
        //    is varied in an attempt to obtain a mixed air temperature equal to
        //    the user specified temperature (schedule based).  The heating coil
        //    is activated, if necessary.
        // 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
        //    coil is present or it has been scheduled off.  Set the amount of
        //    outside air based on the control type.  Simulate the "mixing box".
        // 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
        //    a cooling coil is present and scheduled on.  Tries to use outside
        //    air as best as possible and then calls a cooling coil
        // Note: controls are strictly temperature based and do not factor
        // humidity into the equation (not an enthalpy economy cycle but rather
        // a simple return air economy cycle).  In addition, temperature predictions
        // are not strict energy balances here in the control routine though
        // in the mixing routine an energy balance is preserved.

        // REFERENCES:
        // ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

        // USE STATEMENTS:

        // Using/Aliasing
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

        using HeatingCoils::CheckHeatingCoilSchedule;
        using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetCurrentScheduleValue;
        using SteamCoils::CheckSteamCoilSchedule;
        using WaterCoils::CheckWaterCoilSchedule;

        // Locals
        Real64 QZnReq;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        Real64 const LowTempDiff(0.1); // Smallest allowed temperature difference for comparisons
        // (below this value the temperatures are assumed equal)
        Real64 const LowOAFracDiff(0.01); // Smallest allowed outside air fraction difference for comparison
        // (below this value the fractions are assumed equal)

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirMassFlow;  // air mass flow rate [kg/sec]
        int AirRelNode;      // outside air relief node
        int ControlNode;     // the hot water or cold water inlet node
        int InletNode;       // system air inlet node
        int FanOutletNode;   // system fan outlet node
        int ZoneAirInNode;   // zone supply air node
        Real64 MaxOAFrac;    // maximum possible outside air fraction
        Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
        Real64 MinOAFrac;    // minimum possible outside air fraction
        Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/sec]
        int OutletNode;      // air outlet node
        int OutsideAirNode;  // outside air node
        int MixoutNode;      // oa mixer outlet node
        int ReturnAirNode;   // return air node
        Real64 QUnitOut;     // heating or sens. cooling provided by fan coil unit [watts]
        Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
        Real64 Tdesired;     // desired temperature after mixing inlet and outdoor air [degrees C]
        Real64 Tinlet;       // temperature of air coming into the ventilated slab [degrees C]
        Real64 Toutdoor;     // temperature of outdoor air being introduced into the ventilated slab [degrees C]
        Real64 MaxSteamFlow;
        Real64 MinSteamFlow;
        Real64 RadInTemp;      // "Desired" radiant system air inlet temperature [Celsius]**setpoint
        Real64 SetPointTemp;   // temperature that will be used to control the radiant system [Celsius]
        Real64 SetPointTempHi; // Current high point in setpoint temperature range
        Real64 SetPointTempLo; // Current low point in setpoint temperature range
        Real64 AirTempHi;      // Current high point in water temperature range
        Real64 AirTempLo;      // Current low point in water temperature range
        Real64 AirTempHeatHi;  // Current high point in water temperature range
        Real64 AirTempCoolLo;  // Current low point in water temperature range
        Real64 CpFan;          // Intermediate calculational variable for specific heat of air <<NOV9 Updated
        Real64 ZoneRadNum;     // number of zone being served *********************
        int RadSurfNum;        // DO loop counter for the surfaces that comprise a particular radiant system
        std::string MSlabIn;
        std::string MSlabOut;
        std::string SlabName;
        int MSlabInletNode;
        int MSlabOutletNode;
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        static std::string const CurrentModuleObject("ZoneHVAC:VentilatedSlab");

        {
            auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).CoilOption);
            if (SELECT_CASE_var == state.dataVentilatedSlab->BothOption) {

                {
                    auto const SELECT_CASE_var1(state.dataVentilatedSlab->VentSlab(Item).HCoilType);

                    if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_WaterCoilType) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Heating:Water",
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_SteamCoilType) {
                        CheckSteamCoilSchedule(state,
                                               "Coil:Heating:Steam",
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_ElectricCoilType) {
                        CheckHeatingCoilSchedule(state,
                                                 "Coil:Heating:Electric",
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_GasCoilType) {
                        CheckHeatingCoilSchedule(state,
                                                 "Coil:Heating:Fuel",
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else {
                    }
                }

                {
                    auto const SELECT_CASE_var1(state.dataVentilatedSlab->VentSlab(Item).CCoilType);

                    if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilWaterCooling) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Cooling:Water",
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilDetailedCooling) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Cooling:Water:DetailedGeometry",
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
                        CheckHXAssistedCoolingCoilSchedule(state,
                                                           "CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else {
                    }
                }

            } else if (SELECT_CASE_var == state.dataVentilatedSlab->HeatingOption) {

                {
                    auto const SELECT_CASE_var1(state.dataVentilatedSlab->VentSlab(Item).HCoilType);

                    if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_WaterCoilType) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Heating:Water",
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_SteamCoilType) {
                        CheckSteamCoilSchedule(state,
                                               "Coil:Heating:Steam",
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_ElectricCoilType) {
                        CheckHeatingCoilSchedule(state,
                                                 "Coil:Heating:Electric",
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Heating_GasCoilType) {
                        CheckHeatingCoilSchedule(state,
                                                 "Coil:Heating:Fuel",
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue,
                                                 state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                    } else {
                    }
                }

            } else if (SELECT_CASE_var == state.dataVentilatedSlab->CoolingOption) {

                {
                    auto const SELECT_CASE_var1(state.dataVentilatedSlab->VentSlab(Item).CCoilType);

                    if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilWaterCooling) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Cooling:Water",
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilDetailedCooling) {
                        CheckWaterCoilSchedule(state,
                                               "Coil:Cooling:Water:DetailedGeometry",
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                               state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
                        CheckHXAssistedCoolingCoilSchedule(state,
                                                           "CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue,
                                                           state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
                    } else {
                    }
                }

            } else if (SELECT_CASE_var == state.dataVentilatedSlab->NoneOption) {
            }
        }

        // initialize local variables
        ControlNode = 0;
        QUnitOut = 0.0;
        LatentOutput = 0.0;
        MaxWaterFlow = 0.0;
        MinWaterFlow = 0.0;
        AirMassFlow = 0.0;
        InletNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
        OutletNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        FanOutletNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
        ZoneAirInNode = state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode;
        OutsideAirNode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;
        AirRelNode = state.dataVentilatedSlab->VentSlab(Item).AirReliefNode;
        MixoutNode = state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode;
        ReturnAirNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
        ZoneRadNum = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
        RadSurfNum = state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces;
        Tinlet = state.dataLoopNodes->Node(InletNode).Temp;
        Toutdoor = state.dataLoopNodes->Node(OutsideAirNode).Temp;

        // Control Type Check
        {
            auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).ControlType);
            if (SELECT_CASE_var == state.dataVentilatedSlab->MATControl) {
                SetPointTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->MRTControl) {
                SetPointTemp = state.dataHeatBal->ZoneMRT(ZoneNum);
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->OPTControl) {
                SetPointTemp = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->ODBControl) {
                SetPointTemp = state.dataEnvrn->OutDryBulbTemp;
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->OWBControl) {
                SetPointTemp = state.dataEnvrn->OutWetBulbTemp;
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->SURControl) {
                SetPointTemp = state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum));
            } else if (SELECT_CASE_var == state.dataVentilatedSlab->DPTZControl) {
                SetPointTemp = PsyTdpFnWPb(
                    state, state.dataHeatBalFanSys->ZoneAirHumRat(state.dataVentilatedSlab->VentSlab(Item).ZonePtr), state.dataEnvrn->OutBaroPress);
            } else {                // Should never get here
                SetPointTemp = 0.0; // Suppress uninitialized warning
                ShowSevereError(state, "Illegal control type in low temperature radiant system: " + state.dataVentilatedSlab->VentSlab(Item).Name);
                ShowFatalError(state, "Preceding condition causes termination.");
            }
        }

        // Load Check

        AirTempHeatHi = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).HotCtrlHiTempSchedPtr);
        AirTempCoolLo = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).ColdCtrlLoTempSchedPtr);

        if (((SetPointTemp >= AirTempHeatHi) && (SetPointTemp <= AirTempCoolLo)) ||
            (GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).SchedPtr) <= 0)) {

            // System is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ReturnAirNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(ReturnAirNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRateMinAvail = 0.0;
            AirMassFlow = 0.0;
            state.dataVentilatedSlab->HCoilOn = false;

            // Node condition
            state.dataLoopNodes->Node(InletNode).Temp = state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1));
            state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
            state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(FanOutletNode).Temp;

            // Node condition
            if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs) {
                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                    SlabName = state.dataVentilatedSlab->VentSlab(Item).SurfaceName(RadSurfNum);
                    MSlabIn = state.dataVentilatedSlab->VentSlab(Item).SlabIn(RadSurfNum);
                    MSlabOut = state.dataVentilatedSlab->VentSlab(Item).SlabOut(RadSurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).MSlabInNode = GetOnlySingleNode(state,
                                                                                             MSlabIn,
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             SlabName,
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::Internal,
                                                                                             NodeInputManager::compFluidStream::Primary,
                                                                                             ObjectIsNotParent);
                    state.dataVentilatedSlab->VentSlab(Item).MSlabOutNode = GetOnlySingleNode(state,
                                                                                              MSlabOut,
                                                                                              ErrorsFound,
                                                                                              CurrentModuleObject,
                                                                                              SlabName,
                                                                                              DataLoopNode::NodeFluidType::Air,
                                                                                              DataLoopNode::NodeConnectionType::Internal,
                                                                                              NodeInputManager::compFluidStream::Primary,
                                                                                              ObjectIsNotParent);
                    MSlabInletNode = state.dataVentilatedSlab->VentSlab(Item).MSlabInNode;
                    MSlabOutletNode = state.dataVentilatedSlab->VentSlab(Item).MSlabOutNode;

                    state.dataLoopNodes->Node(MSlabInletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
                    state.dataLoopNodes->Node(MSlabOutletNode).Temp = state.dataLoopNodes->Node(MSlabInletNode).Temp;
                }
            }

            CalcVentilatedSlabComps(state, Item, FirstHVACIteration, QUnitOut);

        } else { // System On

            if (SetPointTemp < AirTempHeatHi) { // HEATING MODE
                state.dataVentilatedSlab->OperatingMode = state.dataVentilatedSlab->HeatingMode;

                // Check the setpoint and temperature span
                SetPointTempHi = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).HotCtrlHiTempSchedPtr);
                SetPointTempLo = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).HotCtrlLoTempSchedPtr);
                if (SetPointTempHi < SetPointTempLo) {
                    ShowSevereError(state, "Heating setpoint temperature mismatch in" + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ShowContinueError(state, "High setpoint temperature is less than low setpoint temperature--check your schedule input");
                    ShowFatalError(state, "Preceding condition causes termination.");
                }
                AirTempHi = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).HotAirHiTempSchedPtr);
                AirTempLo = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).HotAirLoTempSchedPtr);

                if (AirTempHi < AirTempLo) {
                    ShowSevereError(state, "Heating Air temperature mismatch in" + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ShowContinueError(state, "High Air temperature is less than low Air temperature--check your schedule input");
                    ShowFatalError(state, "Preceding condition causes termination.");
                }

                if (SetPointTemp >= SetPointTempHi) {
                    // System is above high heating setpoint so we should be able to turn the system off
                    RadInTemp = AirTempLo;

                } else if (SetPointTemp <= SetPointTempLo) {
                    // System is running with its highest inlet temperature
                    RadInTemp = AirTempHi;
                } else {
                    // Interpolate to obtain the current radiant system inlet temperature
                    RadInTemp = AirTempHi - (AirTempHi - AirTempLo) * (SetPointTemp - SetPointTempLo) / (SetPointTempHi - SetPointTempLo);
                }

                state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).Temp = RadInTemp;

                ControlNode = state.dataVentilatedSlab->VentSlab(Item).HotControlNode;
                MaxWaterFlow = state.dataVentilatedSlab->VentSlab(Item).MaxHotWaterFlow;
                MinWaterFlow = state.dataVentilatedSlab->VentSlab(Item).MinHotWaterFlow;
                MaxSteamFlow = state.dataVentilatedSlab->VentSlab(Item).MaxHotSteamFlow;
                MinSteamFlow = state.dataVentilatedSlab->VentSlab(Item).MinHotSteamFlow;

                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment

                if (!FirstHVACIteration && state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_WaterCoilType) {
                    MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                    MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                }

                if (!FirstHVACIteration && state.dataVentilatedSlab->VentSlab(Item).HCoilType == state.dataVentilatedSlab->Heating_SteamCoilType) {
                    MaxSteamFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                    MinSteamFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                }

                state.dataVentilatedSlab->HCoilOn = true;

                if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                    MinOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MinOASchedPtr) *
                                (state.dataVentilatedSlab->VentSlab(Item).MinOutAirMassFlow / state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate);
                } else {
                    MinOAFrac = 0.0;
                }

                MinOAFrac = min(1.0, max(0.0, MinOAFrac));

                if ((!state.dataVentilatedSlab->VentSlab(Item).HCoilPresent) || (state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue <= 0.0)) {
                    // In heating mode, but there is no coil to provide heating.  This is handled
                    // differently than if there was a heating coil present.  Fixed temperature
                    // will still try to vary the amount of outside air to meet the desired
                    // mixed air temperature, while variable percent will go to full ventilation
                    // when it is most advantageous.

                    // If there are no coil, Slab In Node is assumed to be Fan Outlet Node

                    OutletNode = FanOutletNode;

                    {
                        auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).OAControlType);

                        if (SELECT_CASE_var == state.dataVentilatedSlab->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->VariablePercent) {
                            // This algorithm is probably a bit simplistic in that it just bounces
                            // back and forth between the maximum outside air and the minimum.  In
                            // reality, a system *might* vary between the two based on the load in
                            // the zone.  This simple flow control might cause some overcooling but
                            // chances are that if there is a cooling load and the zone temperature
                            // gets above the outside temperature that overcooling won't be significant.
                            Tinlet = state.dataLoopNodes->Node(InletNode).Temp;
                            Toutdoor = state.dataLoopNodes->Node(OutsideAirNode).Temp;

                            if (Tinlet >= Toutdoor) {

                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                            } else { // Tinlet < Toutdoor

                                MaxOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr);
                                state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            }

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr);
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataVentilatedSlab->OAMassFlowRate =
                                    ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * state.dataLoopNodes->Node(InletNode).MassFlowRate;
                                state.dataVentilatedSlab->OAMassFlowRate = max(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                                state.dataVentilatedSlab->OAMassFlowRate = min(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError(state,
                                               "Ventilated Slab simulation control: illogical condition for " +
                                                   state.dataVentilatedSlab->VentSlab(Item).Name);
                            }
                        }
                    }

                    CalcVentilatedSlabComps(state, Item, FirstHVACIteration, QUnitOut);

                } else { // Heating Coil present

                    {
                        auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).OAControlType);

                        if (SELECT_CASE_var == state.dataVentilatedSlab->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                                MaxOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr);
                            } else {
                                MaxOAFrac = 0.0;
                            }
                            MaxOAFrac = min(1.0, max(0.0, MinOAFrac));
                            state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->VariablePercent) {
                            // In heating mode, the ouside air for "variable percent" control
                            // is set to the minimum value

                            state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr);
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataVentilatedSlab->OAMassFlowRate =
                                    ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * state.dataLoopNodes->Node(InletNode).MassFlowRate;
                                state.dataVentilatedSlab->OAMassFlowRate = max(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                                state.dataVentilatedSlab->OAMassFlowRate = min(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError(state,
                                               "Ventilated Slab simulation control: illogical condition for " +
                                                   state.dataVentilatedSlab->VentSlab(Item).Name);
                            }
                        }
                    }

                    SimVentSlabOAMixer(state, Item);

                    if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->simulate(
                            state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                    } else if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                        Fans::SimulateFanComponents(state,
                                                    state.dataVentilatedSlab->VentSlab(Item).FanName,
                                                    FirstHVACIteration,
                                                    state.dataVentilatedSlab->VentSlab(Item).Fan_Index,
                                                    _,
                                                    ZoneCompTurnFansOn,
                                                    ZoneCompTurnFansOff);
                    }

                    CpFan = PsyCpAirFnW(state.dataLoopNodes->Node(FanOutletNode).HumRat);
                    QZnReq =
                        (state.dataLoopNodes->Node(OutletNode).MassFlowRate) * CpFan * (RadInTemp - state.dataLoopNodes->Node(FanOutletNode).Temp);

                    // Setup the coil configuration
                    {
                        auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).HCoilType);

                        if (SELECT_CASE_var == state.dataVentilatedSlab->Heating_WaterCoilType) {
                            // control water flow to obtain output matching QZnReq

                            ControlCompOutput(state,
                                              state.dataVentilatedSlab->VentSlab(Item).Name,
                                              state.dataVentilatedSlab->cMO_VentilatedSlab,
                                              Item,
                                              FirstHVACIteration,
                                              QZnReq,
                                              ControlNode,
                                              MaxWaterFlow,
                                              MinWaterFlow,
                                              0.001,
                                              state.dataVentilatedSlab->VentSlab(Item).ControlCompTypeNum,
                                              state.dataVentilatedSlab->VentSlab(Item).CompErrIndex,
                                              _,
                                              _,
                                              _,
                                              _,
                                              _,
                                              state.dataVentilatedSlab->VentSlab(Item).HWLoopNum,
                                              state.dataVentilatedSlab->VentSlab(Item).HWLoopSide,
                                              state.dataVentilatedSlab->VentSlab(Item).HWBranchNum);

                        } else if ((SELECT_CASE_var == state.dataVentilatedSlab->Heating_GasCoilType) ||
                                   (SELECT_CASE_var == state.dataVentilatedSlab->Heating_ElectricCoilType) ||
                                   (SELECT_CASE_var == state.dataVentilatedSlab->Heating_SteamCoilType)) {

                            CalcVentilatedSlabComps(state, Item, FirstHVACIteration, QUnitOut);
                        }
                    }

                } //  Coil/no coil block

            } else if (SetPointTemp > AirTempCoolLo) { // Cooling Mode

                state.dataVentilatedSlab->OperatingMode = state.dataVentilatedSlab->CoolingMode;

                SetPointTempHi = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).ColdCtrlHiTempSchedPtr);
                SetPointTempLo = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).ColdCtrlLoTempSchedPtr);
                if (SetPointTempHi < SetPointTempLo) {
                    ShowSevereError(state, "Cooling setpoint temperature mismatch in" + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ShowContinueError(state, "High setpoint temperature is less than low setpoint temperature--check your schedule input");
                    ShowFatalError(state, "Preceding condition causes termination.");
                }

                AirTempHi = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).ColdAirHiTempSchedPtr);
                AirTempLo = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).ColdAirLoTempSchedPtr);
                if (AirTempHi < AirTempLo) {
                    ShowSevereError(state, "Cooling Air temperature mismatch in" + state.dataVentilatedSlab->VentSlab(Item).Name);
                    ShowContinueError(state, "High Air temperature is less than low Air temperature--check your schedule input");
                    ShowFatalError(state, "Preceding condition causes termination.");
                }

                if (SetPointTemp <= SetPointTempLo) {
                    // System is below low cooling setpoint so we should be able to turn the system off
                    RadInTemp = AirTempHi;
                } else if (SetPointTemp >= SetPointTempHi) {
                    // System is running with its lowest inlet temperature
                    RadInTemp = AirTempLo;
                } else {
                    // Interpolate to obtain the current radiant system inlet temperature
                    RadInTemp = AirTempHi - (AirTempHi - AirTempLo) * (SetPointTemp - SetPointTempLo) / (SetPointTempHi - SetPointTempLo);
                }

                ControlNode = state.dataVentilatedSlab->VentSlab(Item).ColdControlNode;
                MaxWaterFlow = state.dataVentilatedSlab->VentSlab(Item).MaxColdWaterFlow;
                MinWaterFlow = state.dataVentilatedSlab->VentSlab(Item).MinColdWaterFlow;

                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if ((!FirstHVACIteration) && (ControlNode > 0) && (state.dataVentilatedSlab->VentSlab(Item).CCoilPresent)) {
                    MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                    MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                }
                state.dataVentilatedSlab->HCoilOn = false;

                if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                    MinOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MinOASchedPtr) *
                                (state.dataVentilatedSlab->VentSlab(Item).MinOutAirMassFlow / state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate);
                } else {
                    MinOAFrac = 0.0;
                }
                MinOAFrac = min(1.0, max(0.0, MinOAFrac));

                if ((!state.dataVentilatedSlab->VentSlab(Item).CCoilPresent) || (state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue <= 0.0)) {
                    // In cooling mode, but there is no coil to provide cooling.  This is handled
                    // differently than if there was a cooling coil present.  Fixed temperature
                    // will still try to vary the amount of outside air to meet the desired
                    // mixed air temperature, while variable percent will go to full ventilation
                    // when it is most advantageous.

                    // If there are no coil, Slab In Node is assumed to be Fan Outlet Node
                    OutletNode = FanOutletNode;

                    {
                        auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).OAControlType);

                        if (SELECT_CASE_var == state.dataVentilatedSlab->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                                MaxOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr);
                            } else {
                                MaxOAFrac = 0.0;
                            }
                            MaxOAFrac = min(1.0, max(0.0, MinOAFrac));
                            state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->VariablePercent) {
                            // This algorithm is probably a bit simplistic in that it just bounces
                            // back and forth between the maximum outside air and the minimum.  In
                            // reality, a system *might* vary between the two based on the load in
                            // the zone.  This simple flow control might cause some overcooling but
                            // chances are that if there is a cooling load and the zone temperature
                            // gets above the outside temperature that overcooling won't be significant.

                            Tinlet = state.dataLoopNodes->Node(InletNode).Temp;
                            Toutdoor = state.dataLoopNodes->Node(OutsideAirNode).Temp;

                            if (Tinlet <= Toutdoor) {

                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                            } else { // Tinlet > Toutdoor

                                MaxOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr);
                                state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            }

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr);
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataVentilatedSlab->OAMassFlowRate =
                                    ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * state.dataLoopNodes->Node(InletNode).MassFlowRate;
                                state.dataVentilatedSlab->OAMassFlowRate = max(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                                state.dataVentilatedSlab->OAMassFlowRate = min(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError(state,
                                               state.dataVentilatedSlab->cMO_VentilatedSlab + " simulation control: illogical condition for " +
                                                   state.dataVentilatedSlab->VentSlab(Item).Name);
                            }
                        }
                    }

                    CalcVentilatedSlabComps(state, Item, FirstHVACIteration, QUnitOut);

                } else {
                    // There is a cooling load and there is a cooling coil present (presumably).
                    // Variable percent will throttle outside air back to the minimum while
                    // fixed temperature will still try to vary the outside air amount to meet
                    // the desired mixed air temperature.

                    {
                        auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).OAControlType);

                        if (SELECT_CASE_var == state.dataVentilatedSlab->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                                MaxOAFrac = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).MaxOASchedPtr);
                            } else {
                                MaxOAFrac = 0.0;
                            }
                            MaxOAFrac = min(1.0, max(0.0, MinOAFrac));
                            state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->VariablePercent) {
                            // A cooling coil is present so let it try to do the cooling...
                            state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataVentilatedSlab->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state, state.dataVentilatedSlab->VentSlab(Item).TempSchedPtr);

                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataVentilatedSlab->OAMassFlowRate =
                                    ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * state.dataLoopNodes->Node(InletNode).MassFlowRate;
                                state.dataVentilatedSlab->OAMassFlowRate = max(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                                state.dataVentilatedSlab->OAMassFlowRate = min(state.dataVentilatedSlab->OAMassFlowRate,
                                                                               (MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MinOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataVentilatedSlab->OAMassFlowRate = MaxOAFrac * state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError(state,
                                               state.dataVentilatedSlab->cMO_VentilatedSlab + " simulation control: illogical condition for " +
                                                   state.dataVentilatedSlab->VentSlab(Item).Name);
                            }
                        }
                    }

                    // control water flow to obtain output matching Low Setpoint Temperateure
                    state.dataVentilatedSlab->HCoilOn = false;

                    SimVentSlabOAMixer(state, Item);
                    if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->simulate(
                            state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                    } else if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                        Fans::SimulateFanComponents(state,
                                                    state.dataVentilatedSlab->VentSlab(Item).FanName,
                                                    FirstHVACIteration,
                                                    state.dataVentilatedSlab->VentSlab(Item).Fan_Index,
                                                    _,
                                                    ZoneCompTurnFansOn,
                                                    ZoneCompTurnFansOff);
                    }

                    CpFan = PsyCpAirFnW(state.dataLoopNodes->Node(FanOutletNode).HumRat);
                    QZnReq =
                        (state.dataLoopNodes->Node(OutletNode).MassFlowRate) * CpFan * (RadInTemp - state.dataLoopNodes->Node(FanOutletNode).Temp);

                    ControlCompOutput(state,
                                      state.dataVentilatedSlab->VentSlab(Item).Name,
                                      state.dataVentilatedSlab->cMO_VentilatedSlab,
                                      Item,
                                      FirstHVACIteration,
                                      QZnReq,
                                      ControlNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      0.001,
                                      state.dataVentilatedSlab->VentSlab(Item).ControlCompTypeNum,
                                      state.dataVentilatedSlab->VentSlab(Item).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      _,
                                      _,
                                      state.dataVentilatedSlab->VentSlab(Item).CWLoopNum,
                                      state.dataVentilatedSlab->VentSlab(Item).CWLoopSide,
                                      state.dataVentilatedSlab->VentSlab(Item).CWBranchNum);
                }

            } // ...end of HEATING/COOLING IF-THEN block

            CalcVentilatedSlabRadComps(state, Item, FirstHVACIteration);

        } // ...end of system ON/OFF IF-THEN block

        // Resimulate fans if AirMassFlow is zero and FanElecPower is > 0, indicating that load or condensation controls shut off the ventilated slab
        // in CalcVentilatedSlabRadComps
        AirMassFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
        Real64 locFanElecPower = 0.0;
        if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            locFanElecPower = state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->fanPower();
        } else {
            locFanElecPower = Fans::GetFanPower(state, state.dataVentilatedSlab->VentSlab(Item).Fan_Index);
        }
        if ((AirMassFlow <= 0.0) && (locFanElecPower > 0.0)) {
            state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRateMinAvail = 0.0;
            if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->simulate(
                    state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            } else if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                Fans::SimulateFanComponents(state,
                                            state.dataVentilatedSlab->VentSlab(Item).FanName,
                                            FirstHVACIteration,
                                            state.dataVentilatedSlab->VentSlab(Item).Fan_Index,
                                            _,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            }
        }

        CalcVentilatedSlabCoilOutput(state, Item, PowerMet, LatOutputProvided);
    }

    void CalcVentilatedSlabComps(EnergyPlusData &state,
                                 int const Item,                // system index in ventilated slab array
                                 bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                                 Real64 &LoadMet                // load met by the system (watts)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine launches the individual component simulations.
        // This is called either when the system is off to carry null conditions
        // through the system or during control iterations to continue updating
        // what is going on within the unit.

        // METHODOLOGY EMPLOYED:
        // Simply calls the different components in order.  Only slight wrinkles
        // here are that the ventilatd slab system has it's own outside air mixed and
        // that a cooling coil must be present in order to call a cooling coil
        // simulation.  Other than that, the subroutine is very straightforward.

        // Using/Aliasing
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirMassFlow; // total mass flow through the system
        Real64 CpAirZn;     // specific heat of dry air at zone conditions (zone conditions same as system inlet)
        int HCoilInAirNode; // inlet node number for fan exit/coil inlet
        int InletNode;      // system air inlet node
        int OutletNode;     // system air outlet node
        // unused0309  INTEGER        :: HCoilOutAirNode
        Real64 QCoilReq; // Heat addition required from an electric/gas heating coil
        Real64 HCoilOutAirTemp;
        Real64 HCoilInAirTemp;
        // unused1208  REAL(r64)           :: RadInTemp       ! Set temperature for "Slab In Node"

        SimVentSlabOAMixer(state, Item);
        if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->simulate(
                state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        } else if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
            Fans::SimulateFanComponents(state,
                                        state.dataVentilatedSlab->VentSlab(Item).FanName,
                                        FirstHVACIteration,
                                        state.dataVentilatedSlab->VentSlab(Item).Fan_Index,
                                        _,
                                        ZoneCompTurnFansOn,
                                        ZoneCompTurnFansOff);
        }
        if ((state.dataVentilatedSlab->VentSlab(Item).CCoilPresent) && (state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue >= 0.0)) {
            if (state.dataVentilatedSlab->VentSlab(Item).CCoilType == state.dataVentilatedSlab->Cooling_CoilHXAssisted) {
                SimHXAssistedCoolingCoil(state,
                                         state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                         FirstHVACIteration,
                                         state.dataVentilatedSlab->On,
                                         0.0,
                                         state.dataVentilatedSlab->VentSlab(Item).CCoil_Index,
                                         ContFanCycCoil);
            } else {
                SimulateWaterCoilComponents(state,
                                            state.dataVentilatedSlab->VentSlab(Item).CCoilName,
                                            FirstHVACIteration,
                                            state.dataVentilatedSlab->VentSlab(Item).CCoil_Index);
            }
        }

        if ((state.dataVentilatedSlab->VentSlab(Item).HCoilPresent) && (state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue >= 0.0)) {

            {
                auto const SELECT_CASE_var(state.dataVentilatedSlab->VentSlab(Item).HCoilType);

                if (SELECT_CASE_var == state.dataVentilatedSlab->Heating_WaterCoilType) {

                    SimulateWaterCoilComponents(state,
                                                state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                FirstHVACIteration,
                                                state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);

                } else if (SELECT_CASE_var == state.dataVentilatedSlab->Heating_SteamCoilType) {

                    if (!state.dataVentilatedSlab->HCoilOn) {
                        QCoilReq = 0.0;
                    } else {
                        HCoilInAirNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
                        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(HCoilInAirNode).HumRat);
                        QCoilReq = state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                       (state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).Temp) -
                                   (state.dataLoopNodes->Node(HCoilInAirNode).Temp);
                    }

                    if (QCoilReq < 0.0) QCoilReq = 0.0; // a heating coil can only heat, not cool

                    SimulateSteamCoilComponents(state,
                                                state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                FirstHVACIteration,
                                                state.dataVentilatedSlab->VentSlab(Item).HCoil_Index,
                                                QCoilReq);

                } else if ((SELECT_CASE_var == state.dataVentilatedSlab->Heating_ElectricCoilType) ||
                           (SELECT_CASE_var == state.dataVentilatedSlab->Heating_GasCoilType)) {

                    if (!state.dataVentilatedSlab->HCoilOn) {
                        QCoilReq = 0.0;
                    } else {
                        HCoilInAirTemp = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).FanOutletNode).Temp;
                        HCoilOutAirTemp = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).Temp;
                        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);
                        QCoilReq = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).FanOutletNode).MassFlowRate * CpAirZn *
                                   (HCoilOutAirTemp - HCoilInAirTemp);
                    }

                    if (QCoilReq < 0.0) QCoilReq = 0.0; // a heating coil can only heat, not cool

                    SimulateHeatingCoilComponents(state,
                                                  state.dataVentilatedSlab->VentSlab(Item).HCoilName,
                                                  FirstHVACIteration,
                                                  QCoilReq,
                                                  state.dataVentilatedSlab->VentSlab(Item).HCoil_Index);
                }
            }
        }

        InletNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
        OutletNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        AirMassFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;

        LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                 PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));
    }

    void CalcVentilatedSlabCoilOutput(EnergyPlusData &state,
                                      int const Item,           // system index in ventilated slab array
                                      Real64 &PowerMet,         // power supplied (W)
                                      Real64 &LatOutputProvided // latent capacity supplied (kg/s)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  July 2015, M.J. Witte, Refactored coil output calcs in to this new routine

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the output from the coils

        // METHODOLOGY EMPLOYED:
        // Calculates the sensible and total enthalpy change from the fan outlet node to the slab inlet node.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirMassFlow; // total mass flow through the system
        int FanOutletNode;  // system fan outlet node
        int OutletNode;     // air outlet node
        Real64 SpecHumOut;  // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn;   // Specific humidity ratio of inlet air (kg moisture / kg moist air)
        Real64 QTotUnitOut; // total unit output [watts]
        Real64 QUnitOut;    // heating or sens. cooling provided by fan coil unit [watts]

        OutletNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        FanOutletNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
        AirMassFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;

        //        QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( FanOutletNode ).Enthalpy );
        QTotUnitOut = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat) -
                                     PsyHFnTdbW(state.dataLoopNodes->Node(FanOutletNode).Temp, state.dataLoopNodes->Node(FanOutletNode).HumRat));
        QUnitOut = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(FanOutletNode).HumRat) -
                                  PsyHFnTdbW(state.dataLoopNodes->Node(FanOutletNode).Temp, state.dataLoopNodes->Node(FanOutletNode).HumRat));
        // Limit sensible <= total when cooling (which is negative, so use max)
        QUnitOut = max(QUnitOut, QTotUnitOut);

        // Report variables...
        state.dataVentilatedSlab->VentSlab(Item).HeatCoilPower = max(0.0, QUnitOut);
        state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilPower = std::abs(min(0.0, QUnitOut));
        state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilPower = std::abs(min(0.0, QTotUnitOut));
        state.dataVentilatedSlab->VentSlab(Item).LateCoolCoilPower =
            state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilPower - state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilPower;
        if (state.dataVentilatedSlab->VentSlab(Item).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataVentilatedSlab->VentSlab(Item).ElecFanPower =
                state.dataHVACFan->fanObjs[state.dataVentilatedSlab->VentSlab(Item).Fan_Index]->fanPower();
        } else {
            state.dataVentilatedSlab->VentSlab(Item).ElecFanPower = Fans::GetFanPower(state, state.dataVentilatedSlab->VentSlab(Item).Fan_Index);
        }
        state.dataVentilatedSlab->VentSlab(Item).AirMassFlowRate = AirMassFlow;

        SpecHumOut = state.dataLoopNodes->Node(OutletNode).HumRat;
        SpecHumIn = state.dataLoopNodes->Node(FanOutletNode).HumRat;
        LatOutputProvided = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
        PowerMet = QUnitOut;
    }

    void CalcVentilatedSlabRadComps(EnergyPlusData &state,
                                    int const Item,                                // System index in ventilated slab array
                                    [[maybe_unused]] bool const FirstHVACIteration // flag for 1st HVAV iteration in the time step !unused1208
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine launches the individual component simulations.
        // This is called either when the system is off to carry null conditions
        // through the system or during control iterations to continue updating
        // what is going on within the system.

        // METHODOLOGY EMPLOYED:
        // Simply calls the different components in order.  Only slight wrinkles
        // here are that the Ventilated Slab has it's own outside air mixed and
        // that a cooling coil must be present in order to call a cooling coil
        // simulation.  Other than that, the subroutine is very straightforward.

        // Using/Aliasing
        using HeatingCoils::SimulateHeatingCoilComponents;
        using NodeInputManager::GetOnlySingleNode;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CondDeltaTemp(0.001); // How close the surface temperatures can get to the dewpoint temperature
        // of a space before the radiant cooling system shuts off the flow.
        Real64 const ZeroSystemResp(0.1); // Response below which the system response is really zero
        Real64 const TempCheckLimit(0.1); // Maximum allowed temperature difference between outlet temperature calculations
        static std::string const CurrentModuleObject("ZoneHVAC:VentilatedSlab");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstrNum;         // Index for construction number in Construct derived type
        Real64 CpAirZn;        // Intermediate calculational variable for specific heat of air
        Real64 DewPointTemp;   // Dew-point temperature based on the zone air conditions
        Real64 EpsMdotCpAirZn; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
        Real64 Mdot;           // Intermediate calculation variable for mass flow rate in a surface within the radiant system
        int RadSurfNum;        // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum2;       // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum3;       // DO loop counter for the surfaces that comprise a particular radiant system
        // unused0309  INTEGER  :: RadSurfNum4    ! DO loop counter for the surfaces that comprise a particular radiant system

        int SurfNum;  // Index for radiant surface in Surface derived type
        int SurfNum2; // Index for radiant surface in Surface derived type
        // unused0309  INTEGER  :: RadSurfNumNum
        Real64 TotalVentSlabRadPower; // Total heat source/sink to radiant system
        Real64 AirMassFlow;           // air mass flow rate in the radiant system, kg/s
        int SlabInNode;               // Node number of the air entering the radiant system
        Real64 AirOutletTempCheck;    // Radiant system air outlet temperature (calculated from mixing all outlet streams together)
        Real64 AirTempIn;             // Temperature of the air entering the radiant system, in C
        int ZoneNum;                  // number of zone being served
        Real64 ZoneMult;              // Zone multiplier for this system
        Real64 Ca;                    // Coefficients to relate the inlet air temperature to the heat source
        Real64 Cb;
        Real64 Cc;
        Real64 Cd;
        Real64 Ce;
        Real64 Cf;
        Real64 Cg;
        Real64 Ch;
        Real64 Ci;
        Real64 Cj;
        Real64 Ck;
        Real64 Cl;
        // For more info on Ca through Cl, refer Constant Flow Radiant System
        int FanOutletNode; // unit air outlet node
        int OAInletNode;   // unit air outlet node
        int MixoutNode;    // unit air outlet node
        int ReturnAirNode; // discription
        int ZoneAirInNode; // supply air node
        // For Phase 3
        Real64 CNumDS;
        Real64 CLengDS;
        Real64 CDiaDS;
        Real64 FlowFrac;
        Real64 MSlabAirInTemp;
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        std::string MSlabIn;
        std::string MSlabOut;
        std::string SlabName;
        int MSlabInletNode;
        int MSlabOutletNode;

        if (state.dataVentilatedSlab->FirstTimeFlag) {
            state.dataVentilatedSlab->AirTempOut.allocate(state.dataVentilatedSlab->MaxCloNumOfSurfaces);
            state.dataVentilatedSlab->FirstTimeFlag = false;
        }

        SlabInNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        FanOutletNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
        OAInletNode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;
        MixoutNode = state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode;
        ReturnAirNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
        ZoneAirInNode = state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode;

        // Set the conditions on the air side inlet
        ZoneNum = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
        ZoneMult = double(state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier);
        AirMassFlow = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).MassFlowRate / ZoneMult;

        if (state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->HeatingMode) {

            if ((!state.dataVentilatedSlab->VentSlab(Item).HCoilPresent) || (state.dataVentilatedSlab->VentSlab(Item).HCoilSchedValue <= 0.0)) {

                AirTempIn = state.dataLoopNodes->Node(FanOutletNode).Temp;
                state.dataLoopNodes->Node(SlabInNode).Temp =
                    state.dataLoopNodes->Node(FanOutletNode).Temp; // If coil not available or running, then coil in and out temps same

            } else {

                AirTempIn = state.dataLoopNodes->Node(SlabInNode).Temp;
            }
        }

        if (state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->CoolingMode) {

            if ((!state.dataVentilatedSlab->VentSlab(Item).CCoilPresent) || (state.dataVentilatedSlab->VentSlab(Item).CCoilSchedValue <= 0.0)) {

                AirTempIn = state.dataLoopNodes->Node(FanOutletNode).Temp;
                state.dataLoopNodes->Node(SlabInNode).Temp =
                    state.dataLoopNodes->Node(FanOutletNode).Temp; // If coil not available or running, then coil in and out temps same

            } else {

                AirTempIn = state.dataLoopNodes->Node(SlabInNode).Temp;
            }
        }

        if (AirMassFlow <= 0.0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.

            for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
                state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
                if (state.dataSurface->Surface(SurfNum).ExtBoundCond > 0 && state.dataSurface->Surface(SurfNum).ExtBoundCond != SurfNum)
                    state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum).ExtBoundCond) =
                        0.0; // Also zero the other side of an interzone
            }

            state.dataVentilatedSlab->VentSlab(Item).SlabOutTemp = state.dataVentilatedSlab->VentSlab(Item).SlabInTemp;

            // zero out node flows
            state.dataLoopNodes->Node(SlabInNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OAInletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
            AirMassFlow = 0.0;
        }

        if (AirMassFlow > 0.0) {

            if ((state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) ||
                (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone)) {

                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                    SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCpAirZn = CalcVentSlabHXEffectTerm(state,
                                                              Item,
                                                              AirTempIn,
                                                              AirMassFlow,
                                                              state.dataVentilatedSlab->VentSlab(Item).SurfaceFlowFrac(RadSurfNum),
                                                              state.dataVentilatedSlab->VentSlab(Item).CoreLength,
                                                              state.dataVentilatedSlab->VentSlab(Item).CoreDiameter,
                                                              state.dataVentilatedSlab->VentSlab(Item).CoreNumbers);

                    // Obtain the heat balance coefficients and calculate the intermediate coefficients
                    // linking the inlet air temperature to the heat source/sink to the radiant system.
                    // The coefficients are based on the Constant Flow Radiation System.

                    ConstrNum = state.dataSurface->Surface(SurfNum).Construction;

                    Ca = state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum);
                    Cb = state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum);
                    Cc = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum);

                    Cd = state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum);
                    Ce = state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum);
                    Cf = state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum);

                    Cg = state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum);
                    Ch = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceQ(0));
                    Ci = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceIn(0));
                    Cj = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceOut(0));

                    Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                    Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                    Mdot = AirMassFlow * state.dataVentilatedSlab->VentSlab(Item).SurfaceFlowFrac(RadSurfNum);
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);

                    state.dataHeatBalFanSys->QRadSysSource(SurfNum) = state.dataVentilatedSlab->VentSlab(Item).CoreNumbers * EpsMdotCpAirZn *
                                                                      (AirTempIn - Ck) /
                                                                      (1.0 + (EpsMdotCpAirZn * Cl / state.dataSurface->Surface(SurfNum).Area));

                    if (state.dataSurface->Surface(SurfNum).ExtBoundCond > 0 && state.dataSurface->Surface(SurfNum).ExtBoundCond != SurfNum)
                        state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum).ExtBoundCond) =
                            state.dataHeatBalFanSys->QRadSysSource(SurfNum);
                    // Also set the other side of an interzone!
                    state.dataVentilatedSlab->AirTempOut(RadSurfNum) =
                        AirTempIn - (state.dataHeatBalFanSys->QRadSysSource(SurfNum) / (Mdot * CpAirZn));

                    // "Temperature Comparison" Cut-off:
                    // Check to see whether or not the system should really be running.  If
                    // QRadSysSource is negative when we are in heating mode or QRadSysSource
                    // is positive when we are in cooling mode, then the radiant system will
                    // be doing the opposite of its intention.  In this case, the flow rate
                    // is set to zero to avoid heating in cooling mode or cooling in heating
                    // mode.

                    if (((state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->HeatingMode) &&
                         (state.dataHeatBalFanSys->QRadSysSource(SurfNum) <= 0.0)) ||
                        ((state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->CoolingMode) &&
                         (state.dataHeatBalFanSys->QRadSysSource(SurfNum) >= 0.0))) {

                        // IF (.not. WarmupFlag) THEN
                        //   TempComparisonErrorCount = TempComparisonErrorCount + 1
                        //   IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
                        //     CALL ShowWarningError(state, 'Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
                        //     CALL ShowContinueError(state, 'Flow to the following ventilated slab will be shut-off to avoid heating in cooling mode
                        //     or cooling &
                        //                             in heating mode')
                        //     CALL ShowContinueError(state, 'Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
                        //     CALL ShowContinueError(state, 'All node temperature are reseted at the ventilated slab surface temperature = '// &
                        //                            RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(RadSurfNum),1,2),2))
                        //     CALL ShowContinueErrorTimeStamp(state, ' ')
                        //   ELSE
                        //     CALL ShowRecurringWarningErrorAtEnd(state, 'Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
                        //                  '] Temperature Comparison Error shut-off occurrence continues.',  &
                        //                  VentSlab(Item)%CondErrCount)
                        //   END IF
                        // END IF

                        state.dataLoopNodes->Node(SlabInNode).MassFlowRate = 0.0;
                        state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
                        state.dataLoopNodes->Node(OAInletNode).MassFlowRate = 0.0;
                        state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
                        state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
                        AirMassFlow = 0.0;

                        for (RadSurfNum2 = 1; RadSurfNum2 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum2) {
                            SurfNum2 = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2);
                            state.dataHeatBalFanSys->QRadSysSource(SurfNum2) = 0.0;
                            if (state.dataSurface->Surface(SurfNum2).ExtBoundCond > 0 &&
                                state.dataSurface->Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum2).ExtBoundCond) =
                                    0.0; // Also zero the other side of an interzone

                            if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) {
                                //            state.dataLoopNodes->Node(Returnairnode)%Temp = MAT(Zonenum)
                                state.dataLoopNodes->Node(ReturnAirNode).Temp =
                                    state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum));
                                state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(ReturnAirNode).Temp;
                                state.dataLoopNodes->Node(SlabInNode).Temp = state.dataLoopNodes->Node(FanOutletNode).Temp;
                            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
                                state.dataLoopNodes->Node(ReturnAirNode).Temp = state.dataHeatBalFanSys->MAT(ZoneNum);
                                state.dataLoopNodes->Node(SlabInNode).Temp = state.dataLoopNodes->Node(ReturnAirNode).Temp;
                                state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                                state.dataLoopNodes->Node(ZoneAirInNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                            }
                        }
                        break; // outer do loop
                    }

                    // Condensation Cut-off:
                    // Check to see whether there are any surface temperatures within the radiant system that have
                    // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
                    // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
                    // conditions.

                    if (state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->CoolingMode) {
                        DewPointTemp = PsyTdpFnWPb(state,
                                                   state.dataHeatBalFanSys->ZoneAirHumRat(state.dataVentilatedSlab->VentSlab(Item).ZonePtr),
                                                   state.dataEnvrn->OutBaroPress);
                        for (RadSurfNum2 = 1; RadSurfNum2 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum2) {
                            if (state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2)) <
                                (DewPointTemp + CondDeltaTemp)) {
                                // Condensation warning--must shut off radiant system
                                state.dataLoopNodes->Node(SlabInNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(OAInletNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                                AirMassFlow = 0.0;
                                for (RadSurfNum3 = 1; RadSurfNum3 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum3) {
                                    SurfNum2 = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum3);
                                    state.dataHeatBalFanSys->QRadSysSource(SurfNum2) = 0.0;
                                    if (state.dataSurface->Surface(SurfNum2).ExtBoundCond > 0 &&
                                        state.dataSurface->Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                        state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum2).ExtBoundCond) =
                                            0.0; // Also zero the other side of an interzone
                                }
                                // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                                if (!state.dataGlobal->WarmupFlag) {
                                    ++state.dataVentilatedSlab->CondensationErrorCount;

                                    if (state.dataVentilatedSlab->VentSlab(Item).CondErrIndex == 0) {
                                        ShowWarningMessage(state,
                                                           state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                               state.dataVentilatedSlab->VentSlab(Item).Name + ']');
                                        ShowContinueError(
                                            state,
                                            "Surface [" +
                                                state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2)).Name +
                                                "] temperature below dew-point temperature--potential for condensation exists");
                                        ShowContinueError(state, "Flow to the ventilated slab system will be shut-off to avoid condensation");
                                        ShowContinueError(state,
                                                          format("Predicted radiant system surface temperature = {:.2R}",
                                                                 state.dataHeatBalSurf->TH(
                                                                     2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2))));
                                        ShowContinueError(
                                            state, format("Zone dew-point temperature + safety factor delta= {:.2R}", DewPointTemp + CondDeltaTemp));
                                        ShowContinueErrorTimeStamp(state, "");
                                    }
                                    if (state.dataVentilatedSlab->CondensationErrorCount == 1) {
                                        ShowContinueError(
                                            state, format("Note that there is a {:.4R} C safety built-in to the shut-off criteria", CondDeltaTemp));
                                        ShowContinueError(state, "Note also that this affects all surfaces that are part of this system");
                                    }
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                                       state.dataVentilatedSlab->VentSlab(Item).Name +
                                                                       "] condensation shut-off occurrence continues.",
                                                                   state.dataVentilatedSlab->VentSlab(Item).CondErrIndex,
                                                                   DewPointTemp,
                                                                   DewPointTemp,
                                                                   _,
                                                                   "C",
                                                                   "C");
                                }
                                break; // outer do loop
                            }
                        }
                    }
                }

                // Total Radiant Power
                AirOutletTempCheck = 0.0;
                TotalVentSlabRadPower = 0.0;
                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                    SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
                    TotalVentSlabRadPower += state.dataHeatBalFanSys->QRadSysSource(SurfNum);
                    AirOutletTempCheck +=
                        (state.dataVentilatedSlab->VentSlab(Item).SurfaceFlowFrac(RadSurfNum) * state.dataVentilatedSlab->AirTempOut(RadSurfNum));
                }
                TotalVentSlabRadPower *= ZoneMult;

                // Return Air temp Check
                if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) {
                    if (AirMassFlow > 0.0) {
                        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);
                        state.dataLoopNodes->Node(ReturnAirNode).Temp =
                            state.dataLoopNodes->Node(SlabInNode).Temp - (TotalVentSlabRadPower / (AirMassFlow * CpAirZn));
                        if ((std::abs(state.dataLoopNodes->Node(ReturnAirNode).Temp - AirOutletTempCheck) > TempCheckLimit) &&
                            (std::abs(TotalVentSlabRadPower) > ZeroSystemResp)) {

                            if (!state.dataGlobal->WarmupFlag) {
                                ++state.dataVentilatedSlab->EnergyImbalanceErrorCount;
                                if (state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex == 0) {
                                    ShowWarningMessage(state,
                                                       state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                           state.dataVentilatedSlab->VentSlab(Item).Name + ']');
                                    ShowContinueError(state, "Ventilated Slab (slab only type) air outlet temperature calculation mismatch.");
                                    ShowContinueError(state,
                                                      "This should not happen as it indicates a potential energy imbalance in the calculations.");
                                    ShowContinueError(state, "However, it could also result from improper input for the ventilated slab or");
                                    ShowContinueError(state, "illogical control temperatures.  Check your input for this ventilated slab and");
                                    ShowContinueError(state, "also look at the internal data shown below.");
                                    ShowContinueError(state,
                                                      format("Predicted return air temperature [C] from the overall energy balance = {:.4R}",
                                                             state.dataLoopNodes->Node(ReturnAirNode).Temp));
                                    ShowContinueError(state,
                                                      format("Predicted return air temperature [C] from the slab section energy balances = {:.4R}",
                                                             AirOutletTempCheck));
                                    ShowContinueError(state,
                                                      format("Total energy rate (power) [W] added to the slab = {:.4R}", TotalVentSlabRadPower));
                                    ShowContinueErrorTimeStamp(state, "");
                                }
                                ShowRecurringWarningErrorAtEnd(state,
                                                               state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                                   state.dataVentilatedSlab->VentSlab(Item).Name +
                                                                   "] temperature calculation mismatch occurrence continues.",
                                                               state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex);
                            }
                        }
                    } else {
                        state.dataLoopNodes->Node(ReturnAirNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                    }
                }

                if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
                    if (AirMassFlow > 0.0) {
                        state.dataLoopNodes->Node(ZoneAirInNode).Temp =
                            state.dataLoopNodes->Node(SlabInNode).Temp - (TotalVentSlabRadPower / (AirMassFlow * CpAirZn));
                        if ((std::abs(state.dataLoopNodes->Node(ZoneAirInNode).Temp - AirOutletTempCheck) > TempCheckLimit) &&
                            (std::abs(TotalVentSlabRadPower) > ZeroSystemResp)) {

                            if (!state.dataGlobal->WarmupFlag) {
                                ++state.dataVentilatedSlab->EnergyImbalanceErrorCount;
                                if (state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex == 0) {
                                    ShowWarningMessage(state,
                                                       state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                           state.dataVentilatedSlab->VentSlab(Item).Name + ']');
                                    ShowContinueError(state, "Ventilated Slab (slab only type) air outlet temperature calculation mismatch.");
                                    ShowContinueError(state,
                                                      "This should not happen as it indicates a potential energy imbalance in the calculations.");
                                    ShowContinueError(state, "However, it could also result from improper input for the ventilated slab or");
                                    ShowContinueError(state, "illogical control temperatures.  Check your input for this ventilated slab and");
                                    ShowContinueError(state, "also look at the internal data shown below.");
                                    ShowContinueError(state,
                                                      format("Predicted return air temperature [C] from the overall energy balance = {:.4R}",
                                                             state.dataLoopNodes->Node(ReturnAirNode).Temp));
                                    ShowContinueError(state,
                                                      format("Predicted return air temperature [C] from the slab section energy balances = {:.4R}",
                                                             AirOutletTempCheck));
                                    ShowContinueError(state,
                                                      format("Total energy rate (power) [W] added to the slab = {:.4R}", TotalVentSlabRadPower));
                                    ShowContinueErrorTimeStamp(state, "");
                                }
                                ShowRecurringWarningErrorAtEnd(state,
                                                               state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                                   state.dataVentilatedSlab->VentSlab(Item).Name +
                                                                   "] temperature calculation mismatch occurrence continues.",
                                                               state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex);
                            }
                        }
                        //       IF ((.NOT. FirstHVACIteration) .AND. &
                        //          (ABS(Node(ReturnAirNode)%Temp-MAT(Zonenum)) > VentSlabAirTempToler))THEN
                        //          NeedtoIterate = .TRUE.
                        //      END IF
                        //         state.dataLoopNodes->Node(ReturnAirNode)%Temp = MAT(Zonenum)
                    } else {
                        state.dataLoopNodes->Node(ZoneAirInNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                        state.dataLoopNodes->Node(ReturnAirNode).Temp = state.dataHeatBalFanSys->MAT(ZoneNum);
                    }
                }

                // Now that we have the source/sink term, we must redo the heat balances to obtain
                // the new SumHATsurf value for the zone.  Note that the difference between the new
                // SumHATsurf and the value originally calculated by the heat balance with a zero
                // source for all radiant systems in the zone is the load met by the system (approximately).
                HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
                HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

            } // SYSCONFIG. SLABONLY&SLABANDZONE

            if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs) {

                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {

                    CNumDS = state.dataVentilatedSlab->VentSlab(Item).CNumbers(RadSurfNum);
                    CLengDS = state.dataVentilatedSlab->VentSlab(Item).CLength(RadSurfNum);  // for check
                    CDiaDS = state.dataVentilatedSlab->VentSlab(Item).CDiameter(RadSurfNum); // for check
                    FlowFrac = 1.0;

                    SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);

                    // Determine the heat exchanger "effectiveness" term
                    EpsMdotCpAirZn = CalcVentSlabHXEffectTerm(state, Item, AirTempIn, AirMassFlow, FlowFrac, CLengDS, CDiaDS, CNumDS);

                    // Obtain the heat balance coefficients and calculate the intermediate coefficients
                    // linking the inlet air temperature to the heat source/sink to the radiant system.
                    // The coefficients are based on the Constant Flow Radiation System.

                    ConstrNum = state.dataSurface->Surface(SurfNum).Construction;

                    Ca = state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum);
                    Cb = state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum);
                    Cc = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum);

                    Cd = state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum);
                    Ce = state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum);
                    Cf = state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum);

                    Cg = state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum);
                    Ch = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceQ(0));
                    Ci = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceIn(0));
                    Cj = double(state.dataConstruction->Construct(ConstrNum).CTFTSourceOut(0));

                    Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                    Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                    Mdot = AirMassFlow * FlowFrac;
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);

                    state.dataHeatBalFanSys->QRadSysSource(SurfNum) =
                        CNumDS * EpsMdotCpAirZn * (AirTempIn - Ck) / (1.0 + (EpsMdotCpAirZn * Cl / state.dataSurface->Surface(SurfNum).Area));

                    if (state.dataSurface->Surface(SurfNum).ExtBoundCond > 0 && state.dataSurface->Surface(SurfNum).ExtBoundCond != SurfNum)
                        state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum).ExtBoundCond) =
                            state.dataHeatBalFanSys->QRadSysSource(SurfNum);
                    // Also set the other side of an interzone!

                    state.dataVentilatedSlab->AirTempOut(RadSurfNum) =
                        AirTempIn - (state.dataHeatBalFanSys->QRadSysSource(SurfNum) / (Mdot * CpAirZn));
                    AirTempIn = state.dataVentilatedSlab->AirTempOut(RadSurfNum);
                    // "Temperature Comparison" Cut-off:
                    // Check to see whether or not the system should really be running.  If
                    // QRadSysSource is negative when we are in heating mode or QRadSysSource
                    // is positive when we are in cooling mode, then the radiant system will
                    // be doing the opposite of its intention.  In this case, the flow rate
                    // is set to zero to avoid heating in cooling mode or cooling in heating
                    // mode.

                    if (RadSurfNum == 1) {
                        if (((state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->HeatingMode) &&
                             (state.dataHeatBalFanSys->QRadSysSource(SurfNum) <= 0.0)) ||
                            ((state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->CoolingMode) &&
                             (state.dataHeatBalFanSys->QRadSysSource(SurfNum) >= 0.0))) {
                            // IF (.not. WarmupFlag) THEN
                            //  TempComparisonErrorCount = TempComparisonErrorCount + 1
                            //  IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
                            //    CALL ShowWarningError(state, 'Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
                            //    CALL ShowContinueError(state, 'Flow to the following ventilated slab will be shut-off to avoid heating in cooling
                            //    mode or cooling &
                            //                            in heating mode')
                            //    CALL ShowContinueError(state, 'Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
                            //    CALL ShowContinueError(state, 'Surface Name  = '//TRIM(VentSlab(Item)%SurfaceName(RadSurfNum)))
                            //    CALL ShowContinueError(state, 'All node temperature are reseted at the surface temperature of control zone = '// &
                            //                           RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(1),1,2),2))
                            //    CALL ShowContinueErrorTimeStamp(state, ' ')
                            //  ELSE
                            //    CALL ShowRecurringWarningErrorAtEnd(state, 'Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
                            //                 ']  shut-off occurrence continues due to temperature comparison error.',  &
                            //                 VentSlab(Item)%CondErrCount)
                            //  END IF
                            // END IF

                            state.dataLoopNodes->Node(SlabInNode).MassFlowRate = 0.0;
                            state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
                            state.dataLoopNodes->Node(OAInletNode).MassFlowRate = 0.0;
                            state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
                            state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
                            AirMassFlow = 0.0;

                            for (RadSurfNum2 = 1; RadSurfNum2 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum2) {
                                SurfNum2 = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2);
                                state.dataHeatBalFanSys->QRadSysSource(SurfNum2) = 0.0;
                                if (state.dataSurface->Surface(SurfNum2).ExtBoundCond > 0 &&
                                    state.dataSurface->Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                    state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum2).ExtBoundCond) =
                                        0.0; // Also zero the other side of an interzone
                            }
                            state.dataLoopNodes->Node(ReturnAirNode).Temp =
                                state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(1));
                            state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(ReturnAirNode).Temp;
                            state.dataLoopNodes->Node(SlabInNode).Temp = state.dataLoopNodes->Node(FanOutletNode).Temp;
                            // Each Internal node is reseted at the surface temperature

                            break; // outer do loop
                        }
                    }
                    // Condensation Cut-off:
                    // Check to see whether there are any surface temperatures within the radiant system that have
                    // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
                    // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
                    // conditions.

                    if (state.dataVentilatedSlab->OperatingMode == state.dataVentilatedSlab->CoolingMode) {
                        DewPointTemp = PsyTdpFnWPb(state,
                                                   state.dataHeatBalFanSys->ZoneAirHumRat(state.dataVentilatedSlab->VentSlab(Item).ZPtr(RadSurfNum)),
                                                   state.dataEnvrn->OutBaroPress);
                        for (RadSurfNum2 = 1; RadSurfNum2 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum2) {
                            if (state.dataHeatBalSurf->TH(2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2)) <
                                (DewPointTemp + CondDeltaTemp)) {
                                // Condensation warning--must shut off radiant system
                                state.dataLoopNodes->Node(SlabInNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(FanOutletNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(OAInletNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(MixoutNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(ReturnAirNode).MassFlowRate = 0.0;
                                state.dataLoopNodes->Node(FanOutletNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                                AirMassFlow = 0.0;
                                for (RadSurfNum3 = 1; RadSurfNum3 <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum3) {
                                    SurfNum2 = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum3);
                                    state.dataHeatBalFanSys->QRadSysSource(SurfNum2) = 0.0;
                                    if (state.dataSurface->Surface(SurfNum2).ExtBoundCond > 0 &&
                                        state.dataSurface->Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                        state.dataHeatBalFanSys->QRadSysSource(state.dataSurface->Surface(SurfNum2).ExtBoundCond) =
                                            0.0; // Also zero the other side of an interzone
                                }
                                // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                                if (!state.dataGlobal->WarmupFlag) {
                                    ++state.dataVentilatedSlab->CondensationErrorCount;
                                    if (state.dataVentilatedSlab->VentSlab(Item).CondErrIndex == 0) {
                                        ShowWarningMessage(state,
                                                           state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                               state.dataVentilatedSlab->VentSlab(Item).Name + ']');
                                        ShowContinueError(
                                            state,
                                            "Surface [" +
                                                state.dataSurface->Surface(state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2)).Name +
                                                "] temperature below dew-point temperature--potential for condensation exists");
                                        ShowContinueError(state, "Flow to the ventilated slab system will be shut-off to avoid condensation");
                                        ShowContinueError(state,
                                                          format("Predicted radiant system surface temperature = {:.2R}",
                                                                 state.dataHeatBalSurf->TH(
                                                                     2, 1, state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum2))));
                                        ShowContinueError(
                                            state, format("Zone dew-point temperature + safety factor delta= {:.2R}", DewPointTemp + CondDeltaTemp));
                                        ShowContinueErrorTimeStamp(state, "");
                                    }
                                    if (state.dataVentilatedSlab->CondensationErrorCount == 1) {
                                        ShowContinueError(
                                            state, format("Note that there is a {:.4R} C safety built-in to the shut-off criteria", CondDeltaTemp));
                                        ShowContinueError(state, "Note also that this affects all surfaces that are part of this system");
                                    }
                                    ShowRecurringWarningErrorAtEnd(state,
                                                                   state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                                       state.dataVentilatedSlab->VentSlab(Item).Name +
                                                                       "] condensation shut-off occurrence continues.",
                                                                   state.dataVentilatedSlab->VentSlab(Item).CondErrIndex,
                                                                   DewPointTemp,
                                                                   DewPointTemp,
                                                                   _,
                                                                   "C",
                                                                   "C");
                                }
                                break; // outer do loop
                            }
                        }
                    }
                }

                // Total Radiant Power
                AirOutletTempCheck = 0.0;
                TotalVentSlabRadPower = 0.0;
                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                    SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
                    TotalVentSlabRadPower += state.dataHeatBalFanSys->QRadSysSource(SurfNum);
                    AirOutletTempCheck = state.dataVentilatedSlab->AirTempOut(RadSurfNum);
                }
                TotalVentSlabRadPower *= ZoneMult;

                // Intenal Node Temperature Check

                MSlabAirInTemp = state.dataLoopNodes->Node(SlabInNode).Temp;

                for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
                    SlabName = state.dataVentilatedSlab->VentSlab(Item).SurfaceName(RadSurfNum);
                    MSlabIn = state.dataVentilatedSlab->VentSlab(Item).SlabIn(RadSurfNum);
                    MSlabOut = state.dataVentilatedSlab->VentSlab(Item).SlabOut(RadSurfNum);
                    state.dataVentilatedSlab->VentSlab(Item).MSlabInNode = GetOnlySingleNode(state,
                                                                                             MSlabIn,
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             SlabName,
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::Internal,
                                                                                             NodeInputManager::compFluidStream::Primary,
                                                                                             ObjectIsNotParent);
                    state.dataVentilatedSlab->VentSlab(Item).MSlabOutNode = GetOnlySingleNode(state,
                                                                                              MSlabOut,
                                                                                              ErrorsFound,
                                                                                              CurrentModuleObject,
                                                                                              SlabName,
                                                                                              DataLoopNode::NodeFluidType::Air,
                                                                                              DataLoopNode::NodeConnectionType::Internal,
                                                                                              NodeInputManager::compFluidStream::Primary,
                                                                                              ObjectIsNotParent);
                    MSlabInletNode = state.dataVentilatedSlab->VentSlab(Item).MSlabInNode;
                    MSlabOutletNode = state.dataVentilatedSlab->VentSlab(Item).MSlabOutNode;
                    SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);

                    if (AirMassFlow > 0.0) {

                        CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);

                        state.dataLoopNodes->Node(MSlabInletNode).Temp = MSlabAirInTemp;
                        state.dataLoopNodes->Node(MSlabOutletNode).Temp = state.dataLoopNodes->Node(MSlabInletNode).Temp -
                                                                          (state.dataHeatBalFanSys->QRadSysSource(SurfNum) / (AirMassFlow * CpAirZn));
                        MSlabAirInTemp = state.dataLoopNodes->Node(MSlabOutletNode).Temp;
                    } else {
                        state.dataLoopNodes->Node(MSlabInletNode).Temp = state.dataLoopNodes->Node(ReturnAirNode).Temp;
                        state.dataLoopNodes->Node(MSlabOutletNode).Temp = state.dataLoopNodes->Node(MSlabInletNode).Temp;
                    }
                }

                // Return Air temp Check
                if (AirMassFlow > 0.0) {

                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);
                    state.dataLoopNodes->Node(ReturnAirNode).Temp =
                        state.dataLoopNodes->Node(SlabInNode).Temp - (TotalVentSlabRadPower / (AirMassFlow * CpAirZn));

                    if ((std::abs(state.dataLoopNodes->Node(ReturnAirNode).Temp - AirOutletTempCheck) > TempCheckLimit) &&
                        (std::abs(TotalVentSlabRadPower) > ZeroSystemResp)) { // Return air temperature check did not match calculated temp

                        if (!state.dataGlobal->WarmupFlag) {
                            ++state.dataVentilatedSlab->EnergyImbalanceErrorCount;
                            if (state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex == 0) {
                                ShowWarningMessage(
                                    state, state.dataVentilatedSlab->cMO_VentilatedSlab + " [" + state.dataVentilatedSlab->VentSlab(Item).Name + ']');
                                ShowContinueError(state, "Ventilated Slab (slab only type) air outlet temperature calculation mismatch.");
                                ShowContinueError(state, "This should not happen as it indicates a potential energy imbalance in the calculations.");
                                ShowContinueError(state, "However, it could also result from improper input for the ventilated slab or");
                                ShowContinueError(state, "illogical control temperatures.  Check your input for this ventilated slab and");
                                ShowContinueError(state, "also look at the internal data shown below.");
                                ShowContinueError(state,
                                                  format("Predicted return air temperature [C] from the overall energy balance = {:.4R}",
                                                         state.dataLoopNodes->Node(ReturnAirNode).Temp));
                                ShowContinueError(state,
                                                  format("Predicted return air temperature [C] from the slab section energy balances = {:.4R}",
                                                         AirOutletTempCheck));
                                ShowContinueError(state, format("Total energy rate (power) [W] added to the slab = {:.4R}", TotalVentSlabRadPower));
                                ShowContinueErrorTimeStamp(state, "");
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           state.dataVentilatedSlab->cMO_VentilatedSlab + " [" +
                                                               state.dataVentilatedSlab->VentSlab(Item).Name +
                                                               "] temperature calculation mismatch occurrence continues.",
                                                           state.dataVentilatedSlab->VentSlab(Item).EnrgyImbalErrIndex);
                        }
                    }

                } else {
                    state.dataLoopNodes->Node(ReturnAirNode).Temp = state.dataLoopNodes->Node(SlabInNode).Temp;
                }

                // Now that we have the source/sink term, we must redo the heat balances to obtain
                // the new SumHATsurf value for the zone.  Note that the difference between the new
                // SumHATsurf and the value originally calculated by the heat balance with a zero
                // source for all radiant systems in the zone is the load met by the system (approximately).

                HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state);
                HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state);

            } // SeriesSlabs

        } //(AirMassFlow > 0.0d0)
    }

    void SimVentSlabOAMixer(EnergyPlusData &state, int const Item) // System index in Ventilated Slab array
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This responsibility of this subroutine is to set the air flow rates
        // through the mixing box portion of the Ventilated Slab and then perform
        // an energy balance to arrive at outlet conditions which then would
        // serve as inlet conditions to the coils (or outlet conditions for
        // the device).  There is some question as to whether this needs to be
        // called every time the coils and fan are called since how the fans and
        // coil operate won't presumable change how the mixer operates.  The
        // method in which this routine is called is slightly cleaner though
        // from a code readability standpoint though less efficient.

        // METHODOLOGY EMPLOYED:
        // The OAMassFlowRate has already been calculated in the main control
        // algorithm.  Use this flow rate to establish all of the other flow
        // rates and perform an energy balance on the mixing of the return and
        // outdoor air streams.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirRelNode;     // relief air node number in ventilated slab loop
        int InletNode;      // inlet node number for ventilated slab loop
        Real64 OAFraction;  // Outside air fraction of inlet air
        int OAMixOutNode;   // outside air mixer outlet node for ventilated slab loop
        int OutsideAirNode; // outside air node number in ventilated slab loop

        AirRelNode = state.dataVentilatedSlab->VentSlab(Item).AirReliefNode;
        InletNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;
        OAMixOutNode = state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode;
        OutsideAirNode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;

        // "Resolve" the air flow rates...

        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = state.dataVentilatedSlab->OAMassFlowRate;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = state.dataVentilatedSlab->OAMassFlowRate;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->OAMassFlowRate;

        state.dataLoopNodes->Node(AirRelNode).MassFlowRate = state.dataVentilatedSlab->OAMassFlowRate;
        state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = state.dataVentilatedSlab->OAMassFlowRate;
        state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = state.dataVentilatedSlab->OAMassFlowRate;

        state.dataLoopNodes->Node(OAMixOutNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        state.dataLoopNodes->Node(OAMixOutNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        state.dataLoopNodes->Node(OAMixOutNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRate;

        // "Inlet" conditions for InletNode and OutsideAirNode have already
        // been set elsewhere so we just need to set the "outlet" conditions
        state.dataLoopNodes->Node(AirRelNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
        state.dataLoopNodes->Node(AirRelNode).Press = state.dataLoopNodes->Node(InletNode).Press;
        state.dataLoopNodes->Node(AirRelNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;
        state.dataLoopNodes->Node(AirRelNode).Enthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;

        if (state.dataLoopNodes->Node(InletNode).MassFlowRate > 0.0) {

            OAFraction = state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate / state.dataLoopNodes->Node(InletNode).MassFlowRate;

        } else {
            OAFraction = 0.0;
        }

        state.dataLoopNodes->Node(InletNode).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat);

        // Perform an energy and moisture mass balance on the mixing portion of the OA Mixer of the ventilated slab
        state.dataLoopNodes->Node(OAMixOutNode).Enthalpy =
            OAFraction * state.dataLoopNodes->Node(OutsideAirNode).Enthalpy + (1.0 - OAFraction) * state.dataLoopNodes->Node(InletNode).Enthalpy;
        state.dataLoopNodes->Node(OAMixOutNode).HumRat =
            OAFraction * state.dataLoopNodes->Node(OutsideAirNode).HumRat + (1.0 - OAFraction) * state.dataLoopNodes->Node(InletNode).HumRat;

        // Find the other key state points based on calculated conditions
        state.dataLoopNodes->Node(OAMixOutNode).Temp =
            PsyTdbFnHW(state.dataLoopNodes->Node(OAMixOutNode).Enthalpy, state.dataLoopNodes->Node(OAMixOutNode).HumRat);
        state.dataLoopNodes->Node(OAMixOutNode).Press = state.dataLoopNodes->Node(InletNode).Press;
    }

    void UpdateVentilatedSlab(EnergyPlusData &state,
                              int const Item, // Index for the ventilated slab under consideration within the derived types
                              [[maybe_unused]] bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep !unused1208
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does any updating that needs to be done for low
        // temperature radiant heating and cooling systems.  One of the most
        // important functions of this routine is to update the average heat
        // source/sink for a particular system over the various system time
        // steps that make up the zone time step.  For hydronic systems,
        // this routine must also set the outlet water conditions.

        // METHODOLOGY EMPLOYED:
        // For the source/sink average update, if the system time step elapsed
        // is still what it used to be, then either we are still iterating or
        // we had to go back and shorten the time step.  As a result, we have
        // to subtract out the previous value that we added.  If the system
        // time step elapsed is different, then we just need to add the new
        // values to the running average.

        // Using/Aliasing
        auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpAppAir;        // Specific heat of air
        int RadSurfNum;         // DO loop counter for radiant surfaces in the ventilated slab
        int SurfNum;            // Surface index number for the current ventilated slab
        int AirInletNode;       // Node number for the air side inlet of the ventilated slab
        Real64 TotalHeatSource; // Total heat source or sink for a particular system (sum of all surface source/sinks)
        int TotRadSurfaces;     // Total number of radiant surfaces in this system
        Real64 AirMassFlow;     // Flow rate of water in the radiant system
        int AirOutletNode;      // Node number for the water side outlet of the radiant system
        int FanOutNode;         // Node number for the water side outlet of the radiant system
        Real64 ZoneMult;        // Zone multiplier
        int ZoneNum;            // Zone for this ventilated slab
        int MixOutNode;         // Node number for the water side outlet of the radiant system
        int OANode;             // Node number for the water side outlet of the radiant system
        Real64 OAFraction;      // Outside air fraction of inlet air
        int ZoneInletNode;      // Node number for the air side inlet of the ventilated slab

        ZoneNum = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
        TotRadSurfaces = state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces;
        MixOutNode = state.dataVentilatedSlab->VentSlab(Item).OAMixerOutNode;
        OANode = state.dataVentilatedSlab->VentSlab(Item).OutsideAirNode;
        AirOutletNode = state.dataVentilatedSlab->VentSlab(Item).RadInNode;
        FanOutNode = state.dataVentilatedSlab->VentSlab(Item).FanOutletNode;
        AirMassFlow = state.dataLoopNodes->Node(AirOutletNode).MassFlowRate;
        ZoneInletNode = state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode;
        CpAppAir = PsyCpAirFnW(state.dataLoopNodes->Node(AirOutletNode).HumRat);
        AirInletNode = state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode;

        for (RadSurfNum = 1; RadSurfNum <= TotRadSurfaces; ++RadSurfNum) {

            SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);

            if (state.dataVentilatedSlab->LastSysTimeElapsed(SurfNum) == SysTimeElapsed) {
                // Still iterating or reducing system time step, so subtract old values which were
                // not valid
                state.dataVentilatedSlab->QRadSysSrcAvg(SurfNum) -= state.dataVentilatedSlab->LastQRadSysSrc(SurfNum) *
                                                                    state.dataVentilatedSlab->LastTimeStepSys(SurfNum) /
                                                                    state.dataGlobal->TimeStepZone;
            }

            // Update the running average and the "last" values with the current values of the appropriate variables
            state.dataVentilatedSlab->QRadSysSrcAvg(SurfNum) +=
                state.dataHeatBalFanSys->QRadSysSource(SurfNum) * TimeStepSys / state.dataGlobal->TimeStepZone;

            state.dataVentilatedSlab->LastQRadSysSrc(SurfNum) = state.dataHeatBalFanSys->QRadSysSource(SurfNum);
            state.dataVentilatedSlab->LastSysTimeElapsed(SurfNum) = SysTimeElapsed;
            state.dataVentilatedSlab->LastTimeStepSys(SurfNum) = TimeStepSys;
        }

        // First sum up all of the heat sources/sinks associated with this system
        TotalHeatSource = 0.0;
        for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
            SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
            TotalHeatSource += state.dataHeatBalFanSys->QRadSysSource(SurfNum);
        }
        ZoneNum = state.dataVentilatedSlab->VentSlab(Item).ZonePtr;
        ZoneMult = double(state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier);
        TotalHeatSource *= ZoneMult;

        // Update the heating side of things

        if ((CpAppAir > 0.0) && (AirMassFlow > 0.0)) {

            if ((state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) ||
                (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs)) {
                state.dataLoopNodes->Node(AirInletNode) = state.dataLoopNodes->Node(AirInletNode);
                state.dataLoopNodes->Node(AirInletNode).Temp =
                    state.dataLoopNodes->Node(AirOutletNode).Temp - TotalHeatSource / AirMassFlow / CpAppAir;
                state.dataLoopNodes->Node(AirInletNode).MassFlowRate = state.dataLoopNodes->Node(AirOutletNode).MassFlowRate;
                state.dataLoopNodes->Node(AirInletNode).HumRat = state.dataLoopNodes->Node(AirOutletNode).HumRat;

            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
                state.dataLoopNodes->Node(ZoneInletNode) = state.dataLoopNodes->Node(ZoneInletNode);
                state.dataLoopNodes->Node(ZoneInletNode).Temp =
                    state.dataLoopNodes->Node(AirOutletNode).Temp - TotalHeatSource / AirMassFlow / CpAppAir;
                state.dataLoopNodes->Node(ZoneInletNode).MassFlowRate = state.dataLoopNodes->Node(AirOutletNode).MassFlowRate;
                state.dataLoopNodes->Node(ZoneInletNode).HumRat = state.dataLoopNodes->Node(AirOutletNode).HumRat;
                state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode).Temp = state.dataHeatBalFanSys->MAT(ZoneNum);
            }

        } else {
            if ((state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) ||
                (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs)) {
                state.dataLoopNodes->Node(FanOutNode) = state.dataLoopNodes->Node(AirOutletNode);
                state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;

            } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
                state.dataLoopNodes->Node(ZoneInletNode) = state.dataLoopNodes->Node(AirInletNode);
                state.dataLoopNodes->Node(FanOutNode) = state.dataLoopNodes->Node(AirOutletNode); // Fan Resolve
                state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
            }
        }

        // Resolve mixouttemp

        if (state.dataLoopNodes->Node(AirInletNode).MassFlowRate > 0.0) {

            OAFraction = state.dataLoopNodes->Node(OANode).MassFlowRate / state.dataLoopNodes->Node(AirInletNode).MassFlowRate;

        } else {
            OAFraction = 0.0;
        }

        if (OAFraction <= 0.0) {

            state.dataLoopNodes->Node(MixOutNode).HumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
            state.dataLoopNodes->Node(MixOutNode).Temp = state.dataLoopNodes->Node(AirInletNode).Temp;

        } else {

            state.dataLoopNodes->Node(MixOutNode).Enthalpy =
                OAFraction * state.dataLoopNodes->Node(OANode).Enthalpy + (1.0 - OAFraction) * state.dataLoopNodes->Node(AirInletNode).Enthalpy;
            state.dataLoopNodes->Node(MixOutNode).HumRat =
                OAFraction * state.dataLoopNodes->Node(OANode).HumRat + (1.0 - OAFraction) * state.dataLoopNodes->Node(AirInletNode).HumRat;

            state.dataLoopNodes->Node(MixOutNode).Temp =
                PsyTdbFnHW(state.dataLoopNodes->Node(MixOutNode).Enthalpy, state.dataLoopNodes->Node(MixOutNode).HumRat);
        }
    }

    Real64 CalcVentSlabHXEffectTerm(EnergyPlusData &state,
                                    int const Item,            // Index number of radiant system under consideration
                                    Real64 const Temperature,  // Temperature of air entering the radiant system, in C
                                    Real64 const AirMassFlow,  // Mass flow rate of water in the radiant system, in kg/s
                                    Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
                                    Real64 const CoreLength,   // Length of tubing in the radiant system, in m
                                    Real64 const CoreDiameter, // Inside diameter of the tubing in the radiant system, in m
                                    Real64 const CoreNumbers)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   December 2000
        //       MODIFIED       June 2008 (air properties)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the radiant system "heat exchanger"
        // effectiveness term.  This is equal to the mass flow rate of water
        // times the specific heat of water times the effectiveness of
        // the heat exchanger (radiant system "coil").

        // METHODOLOGY EMPLOYED:
        // Assumes that the only real heat transfer term that we have to
        // deal with is the convection from the water to the tube.  The
        // other assumptions are that the tube inside surface temperature
        // is equal to the "source location temperature" and that it is
        // a CONSTANT throughout the radiant system.  This is to make
        // the problem more tractable and to fit with other system assumptions
        // that were made elsewhere in the radiant system model.

        // REFERENCES:
        // Property data for air shown below as parameters taken from
        //   Mills, Heat Transfer, Table A.7.
        // Heat exchanger information also from Incropera and DeWitt.
        // Code based loosely on code from IBLAST program (research version)

        // Using/Aliasing
        // Return value
        Real64 CalcVentSlabHXEffectTerm;

        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfPropDivisions(13);
        Real64 const MaxExpPower(50.0); // Maximum power after which EXP argument would be zero for DP variables
        static Array1D<Real64> const Temps(
            NumOfPropDivisions, {1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85}); // Temperature, in C
        static Array1D<Real64> const Mu(NumOfPropDivisions,
                                        {0.0000088,
                                         0.0000176,
                                         0.00001781,
                                         0.00001802,
                                         0.000018225,
                                         0.00001843,
                                         0.00001865,
                                         0.00001887,
                                         0.00001908,
                                         0.00001929,
                                         0.0000195,
                                         0.00001971,
                                         0.00001992}); // Viscosity, in Ns/m2
        static Array1D<Real64> const Conductivity(
            NumOfPropDivisions,
            {0.01275, 0.0255, 0.0258, 0.0261, 0.0264, 0.0267, 0.02705, 0.0274, 0.02775, 0.0281, 0.0284, 0.0287, 0.01435}); // Conductivity, in W/mK
        static Array1D<Real64> const Pr(NumOfPropDivisions, 0.69); // Prandtl number (dimensionless)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 CpAppAir;
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;
        Real64 SysAirMassFlow; // Specific heat of air

        // First find out where we are in the range of temperatures
        Index = 1;
        while (Index <= NumOfPropDivisions) {
            if (Temperature < Temps(Index)) break; // DO loop
            ++Index;
        }

        // Initialize thermal properties of Air
        if (Index == 1) {
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else if (Index > NumOfPropDivisions) {
            Index = NumOfPropDivisions;
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else {
            InterpFrac = (Temperature - Temps(Index - 1)) / (Temps(Index) - Temps(Index - 1));
            MUactual = Mu(Index - 1) + InterpFrac * (Mu(Index) - Mu(Index - 1));
            Kactual = Conductivity(Index - 1) + InterpFrac * (Conductivity(Index) - Conductivity(Index - 1));
            PRactual = Pr(Index - 1) + InterpFrac * (Pr(Index) - Pr(Index - 1));
        }
        // arguments are glycol name, temperature, and concentration
        CpAppAir = PsyCpAirFnW(state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).HumRat);
        SysAirMassFlow = AirMassFlow / CoreNumbers;

        // Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
        ReD = 4.0 * SysAirMassFlow * FlowFraction / (DataGlobalConstants::Pi * MUactual * CoreDiameter);

        // Calculate the Nusselt number based on what flow regime one is in
        if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation

            NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);

        } else { // Laminar flow --> use constant surface temperature relation

            NuD = 3.66;
        }

        // Calculate the NTU parameter
        // NTU = UA/[(Mdot*Cp)min]
        // where: U = h (convection coefficient) and h = (k)(Nu)/D
        //        A = Pi*D*TubeLength
        NTU = DataGlobalConstants::Pi * Kactual * NuD * CoreLength / (SysAirMassFlow * CpAppAir); // FlowFraction cancels out here

        // Calculate Epsilon*MassFlowRate*Cp
        if (NTU > MaxExpPower) {
            CalcVentSlabHXEffectTerm = FlowFraction * SysAirMassFlow * CpAppAir;
        } else {
            CalcVentSlabHXEffectTerm = (1.0 - std::exp(-NTU)) * FlowFraction * SysAirMassFlow * CpAppAir;
        }

        return CalcVentSlabHXEffectTerm;
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
        using namespace DataHeatBalSurface;

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
                    // The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
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

    void ReportVentilatedSlab(EnergyPlusData &state, int const Item) // Index for the ventilated slab under consideration within the derived types
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simply produces output for the low temperature radiant system.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        int RadSurfNum;               // DO loop counter for radiant surfaces in the system
        int SurfNum;                  // Surface number (index) in Surface derived type
        Real64 TotalVentSlabRadPower; // Total source/sink power for the radiant system (sum of all surfaces of the system)
        Real64 ZoneMult;              // Total zone multiplier to apply to the system level variables

        // Slab Part
        TotalVentSlabRadPower = 0.0;
        ZoneMult = 1.0;

        for (RadSurfNum = 1; RadSurfNum <= state.dataVentilatedSlab->VentSlab(Item).NumOfSurfaces; ++RadSurfNum) {
            SurfNum = state.dataVentilatedSlab->VentSlab(Item).SurfacePtr(RadSurfNum);
            TotalVentSlabRadPower += state.dataHeatBalFanSys->QRadSysSource(SurfNum);
        }
        ZoneMult = double(state.dataHeatBal->Zone(state.dataVentilatedSlab->VentSlab(Item).ZonePtr).Multiplier *
                          state.dataHeatBal->Zone(state.dataVentilatedSlab->VentSlab(Item).ZonePtr).ListMultiplier);
        TotalVentSlabRadPower *= ZoneMult;
        state.dataVentilatedSlab->VentSlab(Item).RadHeatingPower = 0.0;
        state.dataVentilatedSlab->VentSlab(Item).RadCoolingPower = 0.0;

        if (TotalVentSlabRadPower >= 0.01) {

            state.dataVentilatedSlab->VentSlab(Item).RadHeatingPower = +TotalVentSlabRadPower;
        } else {

            state.dataVentilatedSlab->VentSlab(Item).RadCoolingPower = -TotalVentSlabRadPower;
        }

        state.dataVentilatedSlab->VentSlab(Item).RadHeatingEnergy =
            state.dataVentilatedSlab->VentSlab(Item).RadHeatingPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVentilatedSlab->VentSlab(Item).RadCoolingEnergy =
            state.dataVentilatedSlab->VentSlab(Item).RadCoolingPower * TimeStepSys * DataGlobalConstants::SecInHour;

        // Coil Part
        state.dataVentilatedSlab->VentSlab(Item).HeatCoilEnergy =
            state.dataVentilatedSlab->VentSlab(Item).HeatCoilPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilEnergy =
            state.dataVentilatedSlab->VentSlab(Item).SensCoolCoilPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVentilatedSlab->VentSlab(Item).LateCoolCoilEnergy =
            state.dataVentilatedSlab->VentSlab(Item).LateCoolCoilPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilEnergy =
            state.dataVentilatedSlab->VentSlab(Item).TotCoolCoilPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataVentilatedSlab->VentSlab(Item).ElecFanEnergy =
            state.dataVentilatedSlab->VentSlab(Item).ElecFanPower * TimeStepSys * DataGlobalConstants::SecInHour;

        if ((state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabOnly) ||
            (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SeriesSlabs)) {
            state.dataVentilatedSlab->VentSlab(Item).SlabInTemp = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).Temp;
            state.dataVentilatedSlab->VentSlab(Item).SlabOutTemp =
                state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode).Temp;

        } else if (state.dataVentilatedSlab->VentSlab(Item).SysConfg == state.dataVentilatedSlab->SlabAndZone) {
            state.dataVentilatedSlab->VentSlab(Item).SlabInTemp = state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).RadInNode).Temp;
            state.dataVentilatedSlab->VentSlab(Item).ZoneInletTemp =
                state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).ZoneAirInNode).Temp;
            state.dataVentilatedSlab->VentSlab(Item).SlabOutTemp =
                state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode).Temp;
        }

        state.dataVentilatedSlab->VentSlab(Item).ReturnAirTemp =
            state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).ReturnAirNode).Temp;
        state.dataVentilatedSlab->VentSlab(Item).FanOutletTemp =
            state.dataLoopNodes->Node(state.dataVentilatedSlab->VentSlab(Item).FanOutletNode).Temp;

        if (state.dataVentilatedSlab->VentSlab(Item).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(state, state.dataSize->CurZoneEqNum, 0, state.dataVentilatedSlab->VentSlab(Item).FirstPass);
            }
        }
    }

    //*****************************************************************************************

} // namespace VentilatedSlab

} // namespace EnergyPlus
