// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/ExhaustAirSystemManager.hh>
// #include <EnergyPlus/ReturnAirPathManager.hh> //2022-01-14: replace with exhaust system
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh> //2022-01-14: may not needed for this exhaust system

namespace EnergyPlus {

namespace ExhaustAirSystemManager {
    // Module containing the routines dealing with the AirLoopHVAC:ReturnPath (formerly Return Air Path)

    // MODULE INFORMATION:
    //       AUTHOR         
    //       DATE WRITTEN   January 2022
    //       MODIFIED       
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To manage the exhaust air system.

    void SimExhaustAirSystem(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Russ Taylor
        //       DATE WRITTEN:    Nov 1997

        // Locals
        int ExhaustAirSystemNum;

        // Obtains and Allocates Mixer related parameters from input file
        // 2022-01-14: To do: Need to define a correponding state.dataExhAirSystemMgr data structure
        if (state.dataExhAirSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetExhaustAirSystemInput(state);
            state.dataExhAirSystemMrg->GetInputFlag = false;
        }

        for (ExhaustAirSystemNum = 1; ExhaustAirSystemNum <= state.dataZoneEquip->NumExhaustAirSystems; ++ExhaustAirSystemNum) {
            CalcExhaustAirSystem(state, ExhaustAirSystemNum);
        }
    }

    void GetExhaustAirSystemInput(EnergyPlusData &state)
    {
        // 2022-01-11: before setting up a seperate set of files for exhaust systems (which will be done later
        // use this function to develop the input processing function of exhaust systems

        // SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: Process exhaust system inputs

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;

        // Locals
        int PathNum;
        int CompNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int Counter;
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool ErrorsFound( false );
        ////////////////////////////////////////////////////////////////////////////////////
        bool IsNotOK; // Flag to verify name

        bool ErrorsFound = false;

        // 2022-01-12: This means a correponding data structure should be build for the exhaust path as well
        // 2022-01-12: So here is another item to be built first: stateZoneEquip->ExhaustAirPath
        // 2022-01-13: Not sure if the following is needed with the json helper method?
        if (allocated(state.dataZoneEquip->ExhaustAirSystem)) {
            return;
        }

        // 2022-01-12: After setting the exhaust air system structs and a few related definitions
        // auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        // cCurrentModuleObject = "AirLoopHVAC:ExhaustSystem";
        // state.dataZoneEquip->NumExhaustAirSystems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // 2022-01-12: More processing code here:
        // 2022-01-13: Use the json helper to process input
        constexpr const char *RoutineName("GetExhaustAirSystemInput: ");
        std::string cCurrentModuleObject = "AirLoopHVAC:ExhaustSystem";
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto const instances = ip->epJSON.find(cCurrentModuleObject);
        if (instances != ip->epJSON.end()) {
            auto const &objectSchemaProps = ip->getObjectSchemaProps(state, cCurrentModuleObject);
            auto &instancesValue = instances.value();
            int numExhaustSystems = instancesValue.size();
            int exhSysNum = 0;

            if (numExhaustSystems > 0) {
                state.dataZoneEquip->ExhaustAirSystem.allocate(numExhaustSystems);
            }

            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                ++exhSysNum;
                auto const &objectFields = instance.value();
                auto &thisExhSys = state.dataZoneEquip->ExhaustAirSystem(exhSysNum);
                thisExhSys.Name = UtilityRoutines::MakeUPPERCase(instance.key());
                ip->markObjectAsUsed(cCurrentModuleObject, instance.key());

                std::string availSchName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "availability_manager_list_name");
                int availMgrNum = 0; // UtilityRoutines::FindItemInList(availSchName, state.dataSystemAvailabilityManager->SchedSysAvailMgrData);
                if (availMgrNum > 0) {
                    // normal conditions
                } else if (availMgrNum == 0) {
                    // black or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    availMgrNum = 0;
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Avaiability Manager Name =" + availSchName + "not found.");
                    // ErrorsFound = true;
                }
                // 2022-01-13: To do: Add related data structure to store Availability Manager  (and for all other fields as well)

                std::string zoneMixerName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "airloophvac_zonemixer_name");
                // int zoneMixerNum = UtilityRoutines::FindItemInList(zoneMixerName, state); // 2022-01-13: need some kind of function overload
                // definition?
                ValidateComponent(state, "AirLoopHVAC:ZoneMixer", zoneMixerName, IsNotOK, "AirLoopHVAC:ExhaustSystem");
                if (IsNotOK) {
                    // zoneMixerNum = 0;
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + "not found.");
                    ErrorsFound = true;
                } else {
                    // normal conditions
                }
                // 2022-01-13: To do: Add related data struct to store zoneMixer number (actually need a local zone num definition as well)

                std::string centralfanType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_object_type");
                int centralfanTypeNum = 0; // UtilityRoutines::FindItemInList(centralfanType, state.dataFans); // 2022-01-13: need some kind of
                                           // function overload definition?
                // 2022-01-13: To-do match fan object types and determine what to do
                /* */

                std::string centralfanName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_name");
                int fanNum = 0; // UtilityRoutines::FindItemInList(centralfanName, state.dataFans); // 2022-01-13: need some kind of function overload
                                // definition?
                if (fanNum > 0) {
                    // normal conditions
                } else if (fanNum == 0) {
                    // black or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    fanNum = 0;
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Fan Name =" + centralfanName + "not found.");
                    ErrorsFound = true;
                }
            }
            state.dataZoneEquip->NumReturnAirPaths = numExhaustSystems;

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
            /* */
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting AirLoopHVAC:ExhaustSystem.  Preceding condition(s) causes termination.");
        }
    }

    void InitExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: This subroutine

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:
    }

    void CalcExhaustAirSystem(EnergyPlusData &state, int &ExhaustAirSystemNum)
    {
        //// SUBROUTINE INFORMATION:
        ////       AUTHOR:          
        ////       DATE WRITTEN:    Jan 2022

        //// PURPOSE OF THIS SUBROUTINE: This subroutine

        //// METHODOLOGY EMPLOYED:

        //// REFERENCES:

        //// USE STATEMENTS:

        //// Using/Aliasing
        //using MixerComponent::SimAirMixer;
        //using ZonePlenum::SimAirZonePlenum;

        //// Locals
        //int ComponentNum;

        //for (ComponentNum = 1; ComponentNum <= state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).NumOfComponents; ++ComponentNum) {
        //    switch (state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentTypeEnum(ComponentNum)) {
        //    case DataZoneEquipment::AirLoopHVACZone::Mixer: // 'AirLoopHVAC:ZoneMixer'

        //        if (!(state.dataAirflowNetwork->AirflowNetworkFanActivated &&
        //              state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
        //            SimAirMixer(state,
        //                        state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentName(ComponentNum),
        //                        state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentIndex(ComponentNum));
        //        }

        //        break;

        //    case DataZoneEquipment::AirLoopHVACZone::ReturnPlenum: // 'AirLoopHVAC:ReturnPlenum'

        //        SimAirZonePlenum(state,
        //                         state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentName(ComponentNum),
        //                         DataZoneEquipment::AirLoopHVACZone::ReturnPlenum,
        //                         state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentIndex(ComponentNum));
        //        break;

        //    default:
        //        ShowSevereError(state,
        //                        "Invalid AirLoopHVAC:ReturnPath Component=" +
        //                            state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentType(ComponentNum));
        //        ShowContinueError(state, "Occurs in AirLoopHVAC:ReturnPath =" + state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).Name);
        //        ShowFatalError(state, "Preceding condition causes termination.");
        //        break;
        //    }
        //}
    }

    void ReportExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: This subroutine

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:
    }

} // namespace ReturnAirPathManager

} // namespace EnergyPlus
