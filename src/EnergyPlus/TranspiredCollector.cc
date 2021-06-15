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
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/TranspiredCollector.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace TranspiredCollector {

    // Module containing routines and data dealing with the Transpired Collectors

    // MODULE INFORMATION:
    //       AUTHOR         B.T. Griffith
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Ecapsulates data and routines for simulating unglazed transpired solar collectors (UTSC)
    //   as a component on the HVAC air system.

    // METHODOLOGY EMPLOYED:
    // Two modes, passive and active.  Active is when air is purposely drawn through collector.
    // Passive is when air exchanges are driven by Natural Ventilation rather than outside air system

    // REFERENCES:
    // Heat Exchange effectiveness relations:
    // Kutscher, C.F. 1994. Heat exchange effectiveness and pressure drop for air flow through perforated plates
    //     with and without crosswind. Journal of Heat Transfer. May 1994, Vol. 116, p. 391.
    //     American Society of Mechanical Engineers.
    // Van Decker, G.W.E., K.G.T. Hollands, and A.P. Brunger. 2001. Heat-exchange relations for unglazed transpired
    //     solar collectors with circular holes on a square of triangular pitch. Solar Energy. Vol. 71, No. 1. pp 33-45, 2001.
    // .

    // OTHER NOTES:
    // EnergyPlus implementation is unique and adds new modeling not described in Literature.
    //   See EngineeringReference for details

    // Using/Aliasing
    using DataVectorTypes::Vector;

    void SimTranspiredCollector(EnergyPlusData &state,
                                std::string_view CompName, // component name
                                int &CompIndex               // component index (to reduce string compares during simulation)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage simulation of Transpired Collectors

        // METHODOLOGY EMPLOYED:
        // Setup to avoid string comparisons after first call

        // Using/Aliasing
        using DataHVACGlobals::TempControlTol;

        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int UTSCNum(0); // local number index for UTSC

        if (state.dataTranspiredCollector->GetInputFlag) {
            GetTranspiredCollectorInput(state);
            state.dataTranspiredCollector->GetInputFlag = false;
        }

        // Find the correct transpired collector with the Component name and/or index
        if (CompIndex == 0) {
            UTSCNum = UtilityRoutines::FindItemInList(CompName, state.dataTranspiredCollector->UTSC);
            if (UTSCNum == 0) {
                ShowFatalError(state, "Transpired Collector not found=" + std::string{CompName});
            }
            CompIndex = UTSCNum;
        } else {
            UTSCNum = CompIndex;
            if (UTSCNum > state.dataTranspiredCollector->NumUTSC || UTSCNum < 1) {
                ShowFatalError(state,
                               format("SimTranspiredCollector: Invalid CompIndex passed={}, Number of Transpired Collectors={}, UTSC name={}",
                                      UTSCNum,
                                      state.dataTranspiredCollector->NumUTSC,
                                      CompName));
            }
            if (state.dataTranspiredCollector->CheckEquipName(UTSCNum)) {
                if (CompName != state.dataTranspiredCollector->UTSC(UTSCNum).Name) {
                    ShowFatalError(state,
                                   format("SimTranspiredCollector: Invalid CompIndex passed={}, Transpired Collector name={}, stored Transpired "
                                          "Collector Name for that index={}",
                                          UTSCNum,
                                          CompName,
                                          state.dataTranspiredCollector->UTSC(UTSCNum).Name));
                }
                state.dataTranspiredCollector->CheckEquipName(UTSCNum) = false;
            }
        }

        InitTranspiredCollector(state, CompIndex);

        // Control point of deciding if transpired collector is active or not.
        auto &UTSC_CI(state.dataTranspiredCollector->UTSC(CompIndex));
        auto &InletNode(UTSC_CI.InletNode);
        auto &ControlNode(UTSC_CI.ControlNode);
        UTSC_CI.IsOn = false;
        if ((GetCurrentScheduleValue(state, UTSC_CI.SchedPtr) > 0.0) &&
            (UTSC_CI.InletMDot > 0.0)) { // availability Schedule | OA system is setting mass flow
            bool ControlLTSet(false);
            bool ControlLTSchedule(false);
            bool ZoneLTSchedule(false);
            assert(equal_dimensions(InletNode, ControlNode));
            assert(equal_dimensions(InletNode, UTSC_CI.ZoneNode));
            for (int i = InletNode.l(), e = InletNode.u(); i <= e; ++i) {
                if (state.dataLoopNodes->Node(InletNode(i)).Temp + TempControlTol < state.dataLoopNodes->Node(ControlNode(i)).TempSetPoint)
                    ControlLTSet = true;
                if (state.dataLoopNodes->Node(InletNode(i)).Temp + TempControlTol < GetCurrentScheduleValue(state, UTSC_CI.FreeHeatSetPointSchedPtr))
                    ControlLTSchedule = true;
                if (state.dataLoopNodes->Node(UTSC_CI.ZoneNode(i)).Temp + TempControlTol <
                    GetCurrentScheduleValue(state, UTSC_CI.FreeHeatSetPointSchedPtr))
                    ZoneLTSchedule = true;
            }
            if (ControlLTSet || (ControlLTSchedule && ZoneLTSchedule))
                UTSC_CI.IsOn = true; // heating required | free heating helpful | free heating helpful
        }

        if (state.dataTranspiredCollector->UTSC(UTSCNum).IsOn) {
            CalcActiveTranspiredCollector(state, UTSCNum);
        } else {
            CalcPassiveTranspiredCollector(state, UTSCNum);
        }

        UpdateTranspiredCollector(state, UTSCNum);
    }

    void GetTranspiredCollectorInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Retrieve user input and set up data structure

        // METHODOLOGY EMPLOYED:
        // usual EnergyPlus input
        // Extensible UTSC object for underlying heat transfer surfaces and for multisystem

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataHeatBalance::MediumRough;
        using DataHeatBalance::MediumSmooth;
        using DataHeatBalance::Rough;
        using DataHeatBalance::Smooth;
        using DataHeatBalance::VeryRough;
        using DataHeatBalance::VerySmooth;
        using DataLoopNode::ObjectIsNotParent;
        using DataSurfaces::OtherSideCondModeledExt;
        using DataSurfaces::SurfaceData;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Array1D_string Alphas; // Alpha items for extensible
        // Solar Collectors:Unglazed Transpired object
        int Item;                    // Item to be "gotten"
        Array1D<Real64> Numbers(11); // Numeric items for object
        int NumAlphas;               // Number of Alphas for each GetObjectItem call
        int NumNumbers;              // Number of Numbers for each GetObjectItem call
        int MaxNumAlphas;            // argumenet for call to GetObjectDefMaxArgs
        int MaxNumNumbers;           // argumenet for call to GetObjectDefMaxArgs
        int Dummy;                   // argumenet for call to GetObjectDefMaxArgs
        int IOStatus;                // Used in GetObjectItem
        bool ErrorsFound(false);     // Set to true if errors in input, fatal at end of routine
        int Found;
        int AlphaOffset; // local temp var
        std::string Roughness;
        int ThisSurf;         // do loop counter
        Real64 AvgAzimuth;    // temp for error checking
        Real64 AvgTilt;       // temp for error checking
        int SurfID;           // local surface "pointer"
        Real64 TiltRads;      // average tilt of collector in radians
        Real64 tempHdeltaNPL; // temporary variable for bouyancy length scale
        int NumUTSCSplitter(0);
        Array1D_string AlphasSplit; // Alpha items for extensible
        // Solar Collectors:Unglazed Transpired object
        int ItemSplit;                        // Item to be "gotten"
        Array1D<Real64> NumbersSplit(1);      // Numeric items for object
        int NumAlphasSplit;                   // Number of Alphas for each GetObjectItem call
        int NumNumbersSplit;                  // Number of Numbers for each GetObjectItem call
        int MaxNumAlphasSplit;                // argumenet for call to GetObjectDefMaxArgs
        int MaxNumNumbersSplit;               // argumenet for call to GetObjectDefMaxArgs
        int IOStatusSplit;                    // Used in GetObjectItem
        int NumOASys;                         // do loop counter
        int ACountBase;                       // counter for alhpasSplit
        Array1D_bool SplitterNameOK;          // check for correct association of
        std::string CurrentModuleObject;      // for ease in renaming.
        std::string CurrentModuleMultiObject; // for ease in renaming.

        CurrentModuleObject = "SolarCollector:UnglazedTranspired";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers);

        if (MaxNumNumbers != 11) {
            ShowSevereError(state,
                            format("GetTranspiredCollectorInput: {} Object Definition indicates not = 11 Number Objects, Number Indicated={}",
                                   CurrentModuleObject,
                                   MaxNumNumbers));
            ErrorsFound = true;
        }
        Alphas.allocate(MaxNumAlphas);
        Numbers = 0.0;
        Alphas = "";

        state.dataTranspiredCollector->NumUTSC = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        CurrentModuleMultiObject = "SolarCollector:UnglazedTranspired:Multisystem";
        NumUTSCSplitter = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleMultiObject);

        state.dataTranspiredCollector->UTSC.allocate(state.dataTranspiredCollector->NumUTSC);
        state.dataTranspiredCollector->CheckEquipName.dimension(state.dataTranspiredCollector->NumUTSC, true);
        SplitterNameOK.dimension(NumUTSCSplitter, false);

        for (Item = 1; Item <= state.dataTranspiredCollector->NumUTSC; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            // first handle alphas
            state.dataTranspiredCollector->UTSC(Item).Name = Alphas(1);

            // now check for multisystem
            if (NumUTSCSplitter > 0) {
                state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
                    state, CurrentModuleMultiObject, Dummy, MaxNumAlphasSplit, MaxNumNumbersSplit);

                if (MaxNumNumbersSplit != 0) {
                    ShowSevereError(state,
                                    format("GetTranspiredCollectorInput: {} Object Definition indicates not = 0 Number Objects, Number Indicated={}",
                                           CurrentModuleMultiObject,
                                           MaxNumNumbersSplit));
                    ErrorsFound = true;
                }
                if (!allocated(AlphasSplit)) AlphasSplit.allocate(MaxNumAlphasSplit);
                NumbersSplit = 0.0;
                AlphasSplit = "";
                for (ItemSplit = 1; ItemSplit <= NumUTSCSplitter; ++ItemSplit) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(
                        state, CurrentModuleMultiObject, ItemSplit, AlphasSplit, NumAlphasSplit, NumbersSplit, NumNumbersSplit, IOStatusSplit);
                    if (!(UtilityRoutines::SameString(AlphasSplit(1), Alphas(1)))) continue;
                    SplitterNameOK(ItemSplit) = true;
                    state.dataTranspiredCollector->UTSC(Item).NumOASysAttached = std::floor(NumAlphasSplit / 4.0);
                    if (mod((NumAlphasSplit), 4) != 1) {
                        ShowSevereError(state,
                                        "GetTranspiredCollectorInput: " + CurrentModuleMultiObject +
                                            " Object Definition indicates not uniform quadtuples of nodes for " + AlphasSplit(1));
                        ErrorsFound = true;
                    }
                    state.dataTranspiredCollector->UTSC(Item).InletNode.allocate(state.dataTranspiredCollector->UTSC(Item).NumOASysAttached);
                    state.dataTranspiredCollector->UTSC(Item).InletNode = 0;
                    state.dataTranspiredCollector->UTSC(Item).OutletNode.allocate(state.dataTranspiredCollector->UTSC(Item).NumOASysAttached);
                    state.dataTranspiredCollector->UTSC(Item).OutletNode = 0;
                    state.dataTranspiredCollector->UTSC(Item).ControlNode.allocate(state.dataTranspiredCollector->UTSC(Item).NumOASysAttached);
                    state.dataTranspiredCollector->UTSC(Item).ControlNode = 0;
                    state.dataTranspiredCollector->UTSC(Item).ZoneNode.allocate(state.dataTranspiredCollector->UTSC(Item).NumOASysAttached);
                    state.dataTranspiredCollector->UTSC(Item).ZoneNode = 0;
                    for (NumOASys = 1; NumOASys <= state.dataTranspiredCollector->UTSC(Item).NumOASysAttached; ++NumOASys) {
                        ACountBase = (NumOASys - 1) * 4 + 2;
                        state.dataTranspiredCollector->UTSC(Item).InletNode(NumOASys) = GetOnlySingleNode(state,
                                                                                                          AlphasSplit(ACountBase),
                                                                                                          ErrorsFound,
                                                                                                          CurrentModuleObject,
                                                                                                          AlphasSplit(1),
                                                                                                          DataLoopNode::NodeFluidType::Air,
                                                                                                          DataLoopNode::NodeConnectionType::Inlet,
                                                                                                          NumOASys,
                                                                                                          ObjectIsNotParent);

                        state.dataTranspiredCollector->UTSC(Item).OutletNode(NumOASys) = GetOnlySingleNode(state,
                                                                                                           AlphasSplit(ACountBase + 1),
                                                                                                           ErrorsFound,
                                                                                                           CurrentModuleObject,
                                                                                                           AlphasSplit(1),
                                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                                                           NumOASys,
                                                                                                           ObjectIsNotParent);
                        TestCompSet(state,
                                    CurrentModuleObject,
                                    AlphasSplit(1),
                                    AlphasSplit(ACountBase),
                                    AlphasSplit(ACountBase + 1),
                                    "Transpired Collector Air Nodes"); // appears that test fails by design??
                        state.dataTranspiredCollector->UTSC(Item).ControlNode(NumOASys) = GetOnlySingleNode(state,
                                                                                                            AlphasSplit(ACountBase + 2),
                                                                                                            ErrorsFound,
                                                                                                            CurrentModuleObject,
                                                                                                            AlphasSplit(1),
                                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                                            DataLoopNode::NodeConnectionType::Sensor,
                                                                                                            1,
                                                                                                            ObjectIsNotParent);

                        state.dataTranspiredCollector->UTSC(Item).ZoneNode(NumOASys) = GetOnlySingleNode(state,
                                                                                                         AlphasSplit(ACountBase + 3),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphasSplit(1),
                                                                                                         DataLoopNode::NodeFluidType::Air,
                                                                                                         DataLoopNode::NodeConnectionType::Sensor,
                                                                                                         1,
                                                                                                         ObjectIsNotParent);

                    } // Each OA System in a Multisystem
                      // DEALLOCATE(AlphasSplit)
                }     // each Multisystem present
            }         // any UTSC Multisystem present

            state.dataTranspiredCollector->UTSC(Item).OSCMName = Alphas(2);
            Found = UtilityRoutines::FindItemInList(state.dataTranspiredCollector->UTSC(Item).OSCMName, state.dataSurface->OSCM);
            if (Found == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cAlphaFieldNames(2) + " not found=" + state.dataTranspiredCollector->UTSC(Item).OSCMName +
                                    " in " + CurrentModuleObject + " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
            }
            state.dataTranspiredCollector->UTSC(Item).OSCMPtr = Found;
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                state.dataTranspiredCollector->UTSC(Item).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataTranspiredCollector->UTSC(Item).SchedPtr = GetScheduleIndex(state, Alphas(3));
                if (state.dataTranspiredCollector->UTSC(Item).SchedPtr == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cAlphaFieldNames(3) + "not found=" + Alphas(3) + " in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
            }

            // now if UTSC(Item)%NumOASysAttached still not set, assume no multisystem
            if (state.dataTranspiredCollector->UTSC(Item).NumOASysAttached == 0) {
                state.dataTranspiredCollector->UTSC(Item).NumOASysAttached = 1;
                state.dataTranspiredCollector->UTSC(Item).InletNode.allocate(1);
                state.dataTranspiredCollector->UTSC(Item).InletNode(1) = 0;
                state.dataTranspiredCollector->UTSC(Item).OutletNode.allocate(1);
                state.dataTranspiredCollector->UTSC(Item).OutletNode(1) = 0;
                state.dataTranspiredCollector->UTSC(Item).ControlNode.allocate(1);
                state.dataTranspiredCollector->UTSC(Item).ControlNode(1) = 0;
                state.dataTranspiredCollector->UTSC(Item).ZoneNode.allocate(1);
                state.dataTranspiredCollector->UTSC(Item).ZoneNode(1) = 0;

                state.dataTranspiredCollector->UTSC(Item).InletNode(1) = GetOnlySingleNode(state,
                                                                                           Alphas(4),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           Alphas(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                                           1,
                                                                                           ObjectIsNotParent);
                state.dataTranspiredCollector->UTSC(Item).OutletNode(1) = GetOnlySingleNode(state,
                                                                                            Alphas(5),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            Alphas(1),
                                                                                            DataLoopNode::NodeFluidType::Air,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            1,
                                                                                            ObjectIsNotParent);
                TestCompSet(state, CurrentModuleObject, Alphas(1), Alphas(4), Alphas(5), "Transpired Collector Air Nodes");

                state.dataTranspiredCollector->UTSC(Item).ControlNode(1) = GetOnlySingleNode(state,
                                                                                             Alphas(6),
                                                                                             ErrorsFound,
                                                                                             CurrentModuleObject,
                                                                                             Alphas(1),
                                                                                             DataLoopNode::NodeFluidType::Air,
                                                                                             DataLoopNode::NodeConnectionType::Sensor,
                                                                                             1,
                                                                                             ObjectIsNotParent);
                state.dataTranspiredCollector->UTSC(Item).ZoneNode(1) = GetOnlySingleNode(state,
                                                                                          Alphas(7),
                                                                                          ErrorsFound,
                                                                                          CurrentModuleObject,
                                                                                          Alphas(1),
                                                                                          DataLoopNode::NodeFluidType::Air,
                                                                                          DataLoopNode::NodeConnectionType::Sensor,
                                                                                          1,
                                                                                          ObjectIsNotParent);
            } // no splitter

            state.dataTranspiredCollector->UTSC(Item).FreeHeatSetPointSchedPtr = GetScheduleIndex(state, Alphas(8));
            if (state.dataTranspiredCollector->UTSC(Item).FreeHeatSetPointSchedPtr == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cAlphaFieldNames(8) + " not found=" + Alphas(8) + " in " + CurrentModuleObject + " =" +
                                    state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
                continue;
            }

            if (UtilityRoutines::SameString(Alphas(9), "Triangle")) {
                state.dataTranspiredCollector->UTSC(Item).Layout = state.dataTranspiredCollector->Layout_Triangle;
            } else if (UtilityRoutines::SameString(Alphas(9), "Square")) {
                state.dataTranspiredCollector->UTSC(Item).Layout = state.dataTranspiredCollector->Layout_Square;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cAlphaFieldNames(9) + " has incorrect entry of " + Alphas(9) + " in " + CurrentModuleObject +
                                    " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
                continue;
            }

            if (UtilityRoutines::SameString(Alphas(10), "Kutscher1994")) {
                state.dataTranspiredCollector->UTSC(Item).Correlation = state.dataTranspiredCollector->Correlation_Kutscher1994;
            } else if (UtilityRoutines::SameString(Alphas(10), "VanDeckerHollandsBrunger2001")) {
                state.dataTranspiredCollector->UTSC(Item).Correlation = state.dataTranspiredCollector->Correlation_VanDeckerHollandsBrunger2001;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cAlphaFieldNames(10) + " has incorrect entry of " + Alphas(9) + " in " + CurrentModuleObject +
                                    " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
                continue;
            }

            Roughness = Alphas(11);
            // Select the correct Number for the associated ascii name for the roughness type
            if (UtilityRoutines::SameString(Roughness, "VeryRough")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = VeryRough;
            if (UtilityRoutines::SameString(Roughness, "Rough")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = Rough;
            if (UtilityRoutines::SameString(Roughness, "MediumRough")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = MediumRough;
            if (UtilityRoutines::SameString(Roughness, "MediumSmooth")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = MediumSmooth;
            if (UtilityRoutines::SameString(Roughness, "Smooth")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = Smooth;
            if (UtilityRoutines::SameString(Roughness, "VerySmooth")) state.dataTranspiredCollector->UTSC(Item).CollRoughness = VerySmooth;

            // Was it set?
            if (state.dataTranspiredCollector->UTSC(Item).CollRoughness == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cAlphaFieldNames(11) + " has incorrect entry of " + Alphas(11) + " in " + CurrentModuleObject +
                                    " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
            }

            AlphaOffset = 11;
            state.dataTranspiredCollector->UTSC(Item).NumSurfs = NumAlphas - AlphaOffset;
            if (state.dataTranspiredCollector->UTSC(Item).NumSurfs == 0) {
                ShowSevereError(state,
                                "No underlying surfaces specified in " + CurrentModuleObject + " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                ErrorsFound = true;
                continue;
            }
            state.dataTranspiredCollector->UTSC(Item).SurfPtrs.allocate(state.dataTranspiredCollector->UTSC(Item).NumSurfs);
            state.dataTranspiredCollector->UTSC(Item).SurfPtrs = 0;
            for (ThisSurf = 1; ThisSurf <= state.dataTranspiredCollector->UTSC(Item).NumSurfs; ++ThisSurf) {
                Found = UtilityRoutines::FindItemInList(Alphas(ThisSurf + AlphaOffset), state.dataSurface->Surface);
                if (Found == 0) {
                    ShowSevereError(state,
                                    "Surface Name not found=" + Alphas(ThisSurf + AlphaOffset) + " in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
                // check that surface is appropriate, Heat transfer, Sun, Wind,
                if (!state.dataSurface->Surface(Found).HeatTransSurf) {
                    ShowSevereError(state,
                                    "Surface " + Alphas(ThisSurf + AlphaOffset) + " not of Heat Transfer type in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
                if (!state.dataSurface->Surface(Found).ExtSolar) {
                    ShowSevereError(state,
                                    "Surface " + Alphas(ThisSurf + AlphaOffset) + " not exposed to sun in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
                if (!state.dataSurface->Surface(Found).ExtWind) {
                    ShowSevereError(state,
                                    "Surface " + Alphas(ThisSurf + AlphaOffset) + " not exposed to wind in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
                if (state.dataSurface->Surface(Found).ExtBoundCond != OtherSideCondModeledExt) {
                    ShowSevereError(state,
                                    "Surface " + Alphas(ThisSurf + AlphaOffset) +
                                        " does not have OtherSideConditionsModel for exterior boundary conditions in " + CurrentModuleObject + " =" +
                                        state.dataTranspiredCollector->UTSC(Item).Name);
                    ErrorsFound = true;
                    continue;
                }
                // check surface orientation, warn if upside down
                if ((state.dataSurface->Surface(Found).Tilt < -95.0) || (state.dataSurface->Surface(Found).Tilt > 95.0)) {
                    ShowWarningError(state, "Suspected input problem with collector surface = " + Alphas(ThisSurf + AlphaOffset));
                    ShowContinueError(
                        state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataTranspiredCollector->UTSC(Item).Name);
                    ShowContinueError(state, "Surface used for solar collector faces down");
                    ShowContinueError(
                        state, format("Surface tilt angle (degrees from ground outward normal) = {:.2R}", state.dataSurface->Surface(Found).Tilt));
                }

                state.dataTranspiredCollector->UTSC(Item).SurfPtrs(ThisSurf) = Found;
            }

            if (ErrorsFound) continue; // previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

            // now that we should have all the surfaces, do some preperations and checks.

            // are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
            Real64 const surfaceArea(sum_sub(state.dataSurface->Surface, &SurfaceData::Area, state.dataTranspiredCollector->UTSC(Item).SurfPtrs));
            //            AvgAzimuth = sum( Surface( UTSC( Item ).SurfPtrs ).Azimuth * Surface( UTSC( Item ).SurfPtrs ).Area ) / sum( Surface(
            // UTSC(  Item
            //).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            AvgAzimuth =
                sum_product_sub(
                    state.dataSurface->Surface, &SurfaceData::Azimuth, &SurfaceData::Area, state.dataTranspiredCollector->UTSC(Item).SurfPtrs) /
                surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage
            //            AvgTilt = sum( Surface( UTSC( Item ).SurfPtrs ).Tilt * Surface( UTSC( Item ).SurfPtrs ).Area ) / sum( Surface( UTSC(
            // Item
            //).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            AvgTilt = sum_product_sub(
                          state.dataSurface->Surface, &SurfaceData::Tilt, &SurfaceData::Area, state.dataTranspiredCollector->UTSC(Item).SurfPtrs) /
                      surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage
            for (ThisSurf = 1; ThisSurf <= state.dataTranspiredCollector->UTSC(Item).NumSurfs; ++ThisSurf) {
                SurfID = state.dataTranspiredCollector->UTSC(Item).SurfPtrs(ThisSurf);
                if (std::abs(state.dataSurface->Surface(SurfID).Azimuth - AvgAzimuth) > 15.0) {
                    ShowWarningError(state,
                                     "Surface " + state.dataSurface->Surface(SurfID).Name +
                                         " has Azimuth different from others in the group associated with " + CurrentModuleObject + " =" +
                                         state.dataTranspiredCollector->UTSC(Item).Name);
                }
                if (std::abs(state.dataSurface->Surface(SurfID).Tilt - AvgTilt) > 10.0) {
                    ShowWarningError(state,
                                     "Surface " + state.dataSurface->Surface(SurfID).Name +
                                         " has Tilt different from others in the group associated with " + CurrentModuleObject + " =" +
                                         state.dataTranspiredCollector->UTSC(Item).Name);
                }

                // test that there are no windows.  Now allow windows
                // If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
                //      Call ShowWarningError(state, 'Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
                //         //'subtracted in the group of surfaces associated with '//TRIM(UTSC(Item)%Name))
                // endif
            }
            state.dataTranspiredCollector->UTSC(Item).Tilt = AvgTilt;
            state.dataTranspiredCollector->UTSC(Item).Azimuth = AvgAzimuth;

            // find area weighted centroid.
            //    UTSC(Item)%Centroid%x = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%x*Surface(UTSC(Item)%SurfPtrs)%Area) &
            //                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
            //    UTSC(Item)%Centroid%y = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%y*Surface(UTSC(Item)%SurfPtrs)%Area) &
            //                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
            //            UTSC( Item ).Centroid.z = sum( Surface( UTSC( Item ).SurfPtrs ).Centroid.z * Surface( UTSC( Item ).SurfPtrs ).Area ) /
            // sum(  Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            state.dataTranspiredCollector->UTSC(Item).Centroid.z = sum_product_sub(state.dataSurface->Surface,
                                                                                   &SurfaceData::Centroid,
                                                                                   &Vector::z,
                                                                                   state.dataSurface->Surface,
                                                                                   &SurfaceData::Area,
                                                                                   state.dataTranspiredCollector->UTSC(Item).SurfPtrs) /
                                                                   surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage

            // now handle numbers from input object
            state.dataTranspiredCollector->UTSC(Item).HoleDia = Numbers(1);
            state.dataTranspiredCollector->UTSC(Item).Pitch = Numbers(2);
            state.dataTranspiredCollector->UTSC(Item).LWEmitt = Numbers(3);
            state.dataTranspiredCollector->UTSC(Item).SolAbsorp = Numbers(4);
            state.dataTranspiredCollector->UTSC(Item).Height = Numbers(5);
            state.dataTranspiredCollector->UTSC(Item).PlenGapThick = Numbers(6);
            if (state.dataTranspiredCollector->UTSC(Item).PlenGapThick <= 0.0) {
                ShowSevereError(
                    state, "Plenum gap must be greater than Zero in " + CurrentModuleObject + " =" + state.dataTranspiredCollector->UTSC(Item).Name);
                continue;
            }
            state.dataTranspiredCollector->UTSC(Item).PlenCrossArea = Numbers(7);
            state.dataTranspiredCollector->UTSC(Item).AreaRatio = Numbers(8);
            state.dataTranspiredCollector->UTSC(Item).CollectThick = Numbers(9);
            state.dataTranspiredCollector->UTSC(Item).Cv = Numbers(10);
            state.dataTranspiredCollector->UTSC(Item).Cd = Numbers(11);

            // Fill out data we now know
            // sum areas of HT surface areas
            //            UTSC( Item ).ProjArea = sum( Surface( UTSC( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced
            // by  below
            state.dataTranspiredCollector->UTSC(Item).ProjArea = surfaceArea;
            if (state.dataTranspiredCollector->UTSC(Item).ProjArea == 0) {
                ShowSevereError(state,
                                "Gross area of underlying surfaces is zero in " + CurrentModuleObject + " =" +
                                    state.dataTranspiredCollector->UTSC(Item).Name);
                continue;
            }
            state.dataTranspiredCollector->UTSC(Item).ActualArea =
                state.dataTranspiredCollector->UTSC(Item).ProjArea * state.dataTranspiredCollector->UTSC(Item).AreaRatio;
            //  need to update this for slots as well as holes
            {
                auto const SELECT_CASE_var(state.dataTranspiredCollector->UTSC(Item).Layout);
                if (SELECT_CASE_var == state.dataTranspiredCollector->Layout_Triangle) { // 'TRIANGLE'
                    state.dataTranspiredCollector->UTSC(Item).Porosity =
                        0.907 * pow_2(state.dataTranspiredCollector->UTSC(Item).HoleDia /
                                      state.dataTranspiredCollector->UTSC(Item).Pitch);       // Kutscher equation, Triangle layout
                } else if (SELECT_CASE_var == state.dataTranspiredCollector->Layout_Square) { // 'SQUARE'
                    state.dataTranspiredCollector->UTSC(Item).Porosity =
                        (DataGlobalConstants::Pi / 4.0) * pow_2(state.dataTranspiredCollector->UTSC(Item).HoleDia) /
                        pow_2(state.dataTranspiredCollector->UTSC(Item).Pitch); // Waterloo equation, square layout
                }
            }
            TiltRads = std::abs(AvgTilt) * DataGlobalConstants::DegToRadians;
            tempHdeltaNPL = std::sin(TiltRads) * state.dataTranspiredCollector->UTSC(Item).Height / 4.0;
            state.dataTranspiredCollector->UTSC(Item).HdeltaNPL = max(tempHdeltaNPL, state.dataTranspiredCollector->UTSC(Item).PlenGapThick);

            SetupOutputVariable(state,
                                "Solar Collector Heat Exchanger Effectiveness",
                                OutputProcessor::Unit::None,
                                state.dataTranspiredCollector->UTSC(Item).HXeff,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Leaving Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataTranspiredCollector->UTSC(Item).TairHX,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Outside Face Suction Velocity",
                                OutputProcessor::Unit::m_s,
                                state.dataTranspiredCollector->UTSC(Item).Vsuction,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Surface Temperature",
                                OutputProcessor::Unit::C,
                                state.dataTranspiredCollector->UTSC(Item).Tcoll,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Plenum Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataTranspiredCollector->UTSC(Item).Tplen,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataTranspiredCollector->UTSC(Item).SensHeatingRate,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataTranspiredCollector->UTSC(Item).SensHeatingEnergy,
                                "System",
                                "Sum",
                                state.dataTranspiredCollector->UTSC(Item).Name,
                                _,
                                "SolarAir",
                                "HeatProduced",
                                _,
                                "System");

            SetupOutputVariable(state,
                                "Solar Collector Natural Ventilation Air Change Rate",
                                OutputProcessor::Unit::ach,
                                state.dataTranspiredCollector->UTSC(Item).PassiveACH,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Natural Ventilation Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataTranspiredCollector->UTSC(Item).PassiveMdotVent,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Wind Natural Ventilation Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataTranspiredCollector->UTSC(Item).PassiveMdotWind,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Buoyancy Natural Ventilation Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataTranspiredCollector->UTSC(Item).PassiveMdotTherm,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Incident Solar Radiation",
                                OutputProcessor::Unit::W_m2,
                                state.dataTranspiredCollector->UTSC(Item).Isc,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector System Efficiency",
                                OutputProcessor::Unit::None,
                                state.dataTranspiredCollector->UTSC(Item).UTSCEfficiency,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
            SetupOutputVariable(state,
                                "Solar Collector Surface Efficiency",
                                OutputProcessor::Unit::None,
                                state.dataTranspiredCollector->UTSC(Item).UTSCCollEff,
                                "System",
                                "Average",
                                state.dataTranspiredCollector->UTSC(Item).Name);
        }

        for (ItemSplit = 1; ItemSplit <= NumUTSCSplitter; ++ItemSplit) {
            if (!SplitterNameOK(ItemSplit)) {
                ShowSevereError(state, "Did not find a match, check names for Solar Collectors:Transpired Collector:Multisystem");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "GetTranspiredCollectorInput: Errors found in input");
        }

        Alphas.deallocate();
    }

    void InitTranspiredCollector(EnergyPlusData &state, int const UTSCNum) // compindex already checked in calling routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
        //       RE-ENGINEERED  na

        // Using/Aliasing
        auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;
        auto &SetPointErrorFlag = state.dataHVACGlobal->SetPointErrorFlag;
        using namespace DataLoopNode;
        using DataSurfaces::SurfaceData;
        using EMSManager::CheckIfNodeSetPointManagedByEMS;

        int UTSCUnitNum;
        int ControlNode;
        int SplitBranch;
        int thisUTSC;
        Real64 Tamb;

        if (state.dataTranspiredCollector->MyOneTimeFlag) {
            // do various one time setups and pitch adjustments across all UTSC
            for (thisUTSC = 1; thisUTSC <= state.dataTranspiredCollector->NumUTSC; ++thisUTSC) {
                if (state.dataTranspiredCollector->UTSC(thisUTSC).Layout == state.dataTranspiredCollector->Layout_Triangle) {
                    {
                        auto const SELECT_CASE_var(state.dataTranspiredCollector->UTSC(thisUTSC).Correlation);
                        if (SELECT_CASE_var == state.dataTranspiredCollector->Correlation_Kutscher1994) { // Kutscher1994
                            state.dataTranspiredCollector->UTSC(thisUTSC).Pitch = state.dataTranspiredCollector->UTSC(thisUTSC).Pitch;
                        } else if (SELECT_CASE_var ==
                                   state.dataTranspiredCollector->Correlation_VanDeckerHollandsBrunger2001) { // VanDeckerHollandsBrunger2001
                            state.dataTranspiredCollector->UTSC(thisUTSC).Pitch /= 1.6;
                        }
                    }
                }
                if (state.dataTranspiredCollector->UTSC(thisUTSC).Layout == state.dataTranspiredCollector->Layout_Square) {
                    {
                        auto const SELECT_CASE_var(state.dataTranspiredCollector->UTSC(thisUTSC).Correlation);
                        if (SELECT_CASE_var == state.dataTranspiredCollector->Correlation_Kutscher1994) { // Kutscher1994
                            state.dataTranspiredCollector->UTSC(thisUTSC).Pitch *= 1.6;
                        } else if (SELECT_CASE_var ==
                                   state.dataTranspiredCollector->Correlation_VanDeckerHollandsBrunger2001) { // VanDeckerHollandsBrunger2001
                            state.dataTranspiredCollector->UTSC(thisUTSC).Pitch = state.dataTranspiredCollector->UTSC(thisUTSC).Pitch;
                        }
                    }
                }
            }

            state.dataTranspiredCollector->MyEnvrnFlag.dimension(state.dataTranspiredCollector->NumUTSC, true);
            state.dataTranspiredCollector->MyOneTimeFlag = false;
        } // first time

        // Check that setpoint is active (from test by RJL in HVACEvapComponent)
        if (!state.dataGlobal->SysSizingCalc && state.dataTranspiredCollector->MySetPointCheckFlag && DoSetPointTest) {
            for (UTSCUnitNum = 1; UTSCUnitNum <= state.dataTranspiredCollector->NumUTSC; ++UTSCUnitNum) {
                for (SplitBranch = 1; SplitBranch <= state.dataTranspiredCollector->UTSC(UTSCUnitNum).NumOASysAttached; ++SplitBranch) {
                    ControlNode = state.dataTranspiredCollector->UTSC(UTSCUnitNum).ControlNode(SplitBranch);
                    if (ControlNode > 0) {
                        if (state.dataLoopNodes->Node(ControlNode).TempSetPoint == SensedNodeFlagValue) {
                            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                                ShowSevereError(state,
                                                "Missing temperature setpoint for UTSC " + state.dataTranspiredCollector->UTSC(UTSCUnitNum).Name);
                                ShowContinueError(state, " use a Setpoint Manager to establish a setpoint at the unit control node.");
                                SetPointErrorFlag = true;
                            } else {
                                // need call to EMS to check node
                                CheckIfNodeSetPointManagedByEMS(
                                    state, ControlNode, EMSManager::SPControlType::iTemperatureSetPoint, SetPointErrorFlag);
                                if (SetPointErrorFlag) {
                                    ShowSevereError(state,
                                                    "Missing temperature setpoint for UTSC " + state.dataTranspiredCollector->UTSC(UTSCUnitNum).Name);
                                    ShowContinueError(state, " use a Setpoint Manager to establish a setpoint at the unit control node.");
                                    ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                                }
                            }
                        }
                    }
                }
            }
            state.dataTranspiredCollector->MySetPointCheckFlag = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && state.dataTranspiredCollector->MyEnvrnFlag(UTSCNum)) {
            state.dataTranspiredCollector->UTSC(UTSCNum).TplenLast = 22.5;
            state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast = 22.0;

            state.dataTranspiredCollector->MyEnvrnFlag(UTSCNum) = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataTranspiredCollector->MyEnvrnFlag(UTSCNum) = true;
        }

        // determine average ambient temperature
        Real64 sum_area = 0.0;
        for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
            sum_area += state.dataSurface->Surface(SurfNum).Area;
        }
        if (!state.dataEnvrn->IsRain) {
            Real64 sum_produc_area_drybulb = 0.0;
            for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
                sum_produc_area_drybulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutDryBulbTemp(SurfNum);
            }
            Tamb = sum_produc_area_drybulb / sum_area;
        } else { // when raining we use wet bulb not drybulb
            Real64 sum_produc_area_wetbulb = 0.0;
            for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
                sum_produc_area_wetbulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutWetBulbTemp(SurfNum);
            }
            Tamb = sum_produc_area_wetbulb / sum_area;
        }

        // inits for each iteration
        //        UTSC( UTSCNum ).InletMDot = sum( Node( UTSC( UTSCNum ).InletNode ).MassFlowRate ); //Autodesk:F2C++ Array subscript usage:
        // Replaced by below
        state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot =
            sum_sub(state.dataLoopNodes->Node,
                    &DataLoopNode::NodeData::MassFlowRate,
                    state.dataTranspiredCollector->UTSC(UTSCNum).InletNode); // Autodesk:F2C++ Functions handle array subscript usage
        state.dataTranspiredCollector->UTSC(UTSCNum).IsOn = false;           // intialize then turn on if appropriate
        state.dataTranspiredCollector->UTSC(UTSCNum).Tplen = state.dataTranspiredCollector->UTSC(UTSCNum).TplenLast;
        state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll = state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast;
        state.dataTranspiredCollector->UTSC(UTSCNum).TairHX = Tamb;
        state.dataTranspiredCollector->UTSC(UTSCNum).MdotVent = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).HXeff = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).Isc = 0.0;

        state.dataTranspiredCollector->UTSC(UTSCNum).UTSCEfficiency = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).UTSCCollEff = 0.0;
    }

    void CalcActiveTranspiredCollector(EnergyPlusData &state, int const UTSCNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using ConvectionCoefficients::InitExteriorConvectionCoeff;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
        using DataSurfaces::SurfaceData;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using namespace DataHeatBalance; // , ONLY: QRadSWOutIncident, Construct, Material

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const nu(15.66e-6); // kinematic viscosity (m**2/s) for air at 300 K
        // (Mills 1999 Heat Transfer)
        Real64 const k(0.0267); // thermal conductivity (W/m K) for air at 300 K
        // (Mills 1999 Heat Transfer)
        Real64 const Sigma(5.6697e-08); // Stefan-Boltzmann constant

        // following arrays are used to temporarily hold results from multiple underlying surfaces
        Array1D<Real64> HSkyARR;
        Array1D<Real64> HGroundARR;
        Array1D<Real64> HAirARR;
        Array1D<Real64> HPlenARR;
        Array1D<Real64> LocalWindArr;

        // working variables
        Real64 RhoAir;          // density of air
        Real64 CpAir;           // specific heat of air
        Real64 holeArea;        // area of perforations, includes corrugation of surface
        Real64 Tamb;            // outdoor drybulb
        Real64 A;               // projected area of collector, from sum of underlying surfaces
        Real64 Vholes;          // mean velocity of air as it passes through collector holes
        Real64 Vsuction;        // mean velocity of air as is approaches the collector
        Real64 Vplen;           // mean velocity of air inside plenum
        Real64 HcPlen;          // surface convection heat transfer coefficient for plenum surfaces
        Real64 D;               // hole diameter
        Real64 ReD;             // Reynolds number for holes
        Real64 P;               // pitch, distance betweeen holes
        Real64 Por;             // porosity, area fraction of collector that is open because of holes
        Real64 Mdot;            // mass flow rate of suction air
        Real64 QdotSource;      // energy flux for source/sink inside collector surface (for hybrid PV UTSC)
        int ThisSurf;           // do loop counter
        int NumSurfs;           // number of underlying HT surfaces associated with UTSC
        int Roughness;          // parameters for surface roughness, defined in DataHeatBalance
        Real64 SolAbs;          // solar absorptivity of collector
        Real64 AbsExt;          // thermal emmittance of collector
        Real64 TempExt;         // collector temperature
        int SurfPtr;            // index of surface in main surface structure
        Real64 HMovInsul;       // dummy for call to InitExteriorConvectionCoeff
        Real64 HExt;            // dummy for call to InitExteriorConvectionCoeff
        int ConstrNum;          // index of construction in main construction structure
        Real64 AbsThermSurf;    // thermal emmittance of underlying wall.
        Real64 TsoK;            // underlying surface temperature in Kelvin
        Real64 TscollK;         // collector temperature in Kelvin  (lagged)
        Real64 AreaSum;         // sum of contributing surfaces for area-weighted averages.
        Real64 Vwind;           // localized, and area-weighted average for wind speed
        Real64 HrSky;           // radiation coeff for sky, area-weighted average
        Real64 HrGround;        // radiation coeff for ground, area-weighted average
        Real64 HrAtm;           // radiation coeff for air (bulk atmosphere), area-weighted average
        Real64 Isc;             // Incoming combined solar radiation, area-weighted average
        Real64 HrPlen;          // radiation coeff for plenum surfaces, area-weighted average
        Real64 Tso;             // temperature of underlying surface, area-weighted average
        Real64 HcWind;          // convection coeff for high speed wind situations
        Real64 NuD;             // nusselt number for Reynolds based on hole
        Real64 U;               // overall heat exchanger coefficient
        Real64 HXeff;           // effectiveness for heat exchanger
        Real64 t;               // collector thickness
        Real64 ReS;             // Reynolds number based on suction velocity and pitch
        Real64 ReW;             // Reynolds number based on Wind and pitch
        Real64 ReB;             // Reynolds number based on hole velocity and pitch
        Real64 ReH;             // Reynolds number based on hole velocity and diameter
        Real64 Tscoll;          // temperature of collector
        Real64 TaHX;            // leaving air temperature from heat exchanger (entering plenum)
        Real64 Taplen;          // Air temperature in plen and outlet node.
        Real64 SensHeatingRate; // Rate at which the system is heating outdoor air
        Real64 AlessHoles;      // Area for Kutscher's relation

        // Active UTSC calculation
        // first do common things for both correlations
        Real64 sum_area = 0.0;
        for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
            sum_area += state.dataSurface->Surface(SurfNum).Area;
        }
        if (!state.dataEnvrn->IsRain) {
            Real64 sum_produc_area_drybulb = 0.0;
            for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
                sum_produc_area_drybulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutDryBulbTemp(SurfNum);
            }
            Tamb = sum_produc_area_drybulb / sum_area;
        } else { // when raining we use wet bulb not drybulb
            Real64 sum_produc_area_wetbulb = 0.0;
            for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
                sum_produc_area_wetbulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutWetBulbTemp(SurfNum);
            }
            Tamb = sum_produc_area_wetbulb / sum_area;
        }

        RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tamb, state.dataEnvrn->OutHumRat);

        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);

        holeArea = state.dataTranspiredCollector->UTSC(UTSCNum).ActualArea * state.dataTranspiredCollector->UTSC(UTSCNum).Porosity;

        A = state.dataTranspiredCollector->UTSC(UTSCNum).ProjArea;

        Vholes = state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot / RhoAir / holeArea;

        Vplen = state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot / RhoAir / state.dataTranspiredCollector->UTSC(UTSCNum).PlenCrossArea;

        Vsuction = state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot / RhoAir / A;

        if ((Vsuction < 0.001) || (Vsuction > 0.08)) { // warn that collector is not sized well
            if (state.dataTranspiredCollector->UTSC(UTSCNum).VsucErrIndex == 0) {
                ShowWarningMessage(state,
                                   "Solar Collector:Unglazed Transpired=\"" + state.dataTranspiredCollector->UTSC(UTSCNum).Name +
                                       "\", Suction velocity is outside of range for a good design");
                ShowContinueErrorTimeStamp(state, format("Suction velocity ={:.4R}", Vsuction));
                if (Vsuction < 0.003) {
                    ShowContinueError(state, "Velocity is low -- suggest decreasing area of transpired collector");
                }
                if (Vsuction > 0.08) {
                    ShowContinueError(state, "Velocity is high -- suggest increasing area of transpired collector");
                }
                ShowContinueError(state, "Occasional suction velocity messages are not unexpected when simulating actual conditions");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "Solar Collector:Unglazed Transpired=\"" + state.dataTranspiredCollector->UTSC(UTSCNum).Name +
                                               "\", Suction velocity is outside of range",
                                           state.dataTranspiredCollector->UTSC(UTSCNum).VsucErrIndex,
                                           Vsuction,
                                           Vsuction,
                                           _,
                                           "[m/s]",
                                           "[m/s]");
        }

        HcPlen = 5.62 + 3.92 * Vplen;

        D = state.dataTranspiredCollector->UTSC(UTSCNum).HoleDia;

        ReD = Vholes * D / nu;

        P = state.dataTranspiredCollector->UTSC(UTSCNum).Pitch;

        Por = state.dataTranspiredCollector->UTSC(UTSCNum).Porosity;

        Mdot = state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot;

        QdotSource = state.dataTranspiredCollector->UTSC(UTSCNum).QdotSource; // for hybrid PV transpired collectors

        // loop through underlying surfaces and collect needed data
        // now collect average values for things associated with the underlying surface(s)
        NumSurfs = state.dataTranspiredCollector->UTSC(UTSCNum).NumSurfs;
        HSkyARR.dimension(NumSurfs, 0.0);
        HGroundARR.dimension(NumSurfs, 0.0);
        HAirARR.dimension(NumSurfs, 0.0);
        LocalWindArr.dimension(NumSurfs, 0.0);
        // ALLOCATE(IscARR(NumSurfs))
        // IscARR = 0.0
        HPlenARR.dimension(NumSurfs, 0.0);
        //  ALLOCATE(TsoARR(NumSurfs))
        //  TsoARR = 0.0

        Roughness = state.dataTranspiredCollector->UTSC(UTSCNum).CollRoughness;
        SolAbs = state.dataTranspiredCollector->UTSC(UTSCNum).SolAbsorp;
        AbsExt = state.dataTranspiredCollector->UTSC(UTSCNum).LWEmitt;
        TempExt = state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast;
        for (ThisSurf = 1; ThisSurf <= NumSurfs; ++ThisSurf) {
            SurfPtr = state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs(ThisSurf);
            // Initializations for this surface
            HMovInsul = 0.0;
            HExt = 0.0;
            LocalWindArr(ThisSurf) = state.dataSurface->SurfOutWindSpeed(SurfPtr);
            InitExteriorConvectionCoeff(
                state, SurfPtr, HMovInsul, Roughness, AbsExt, TempExt, HExt, HSkyARR(ThisSurf), HGroundARR(ThisSurf), HAirARR(ThisSurf));
            ConstrNum = state.dataSurface->Surface(SurfPtr).Construction;
            AbsThermSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;
            TsoK = state.dataHeatBalSurf->TH(1, 1, SurfPtr) + DataGlobalConstants::KelvinConv;
            TscollK = state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast + DataGlobalConstants::KelvinConv;
            HPlenARR(ThisSurf) = Sigma * AbsExt * AbsThermSurf * (pow_4(TscollK) - pow_4(TsoK)) / (TscollK - TsoK);
        }
        //        AreaSum = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
        auto Area(
            array_sub(state.dataSurface->Surface,
                      &SurfaceData::Area,
                      state.dataTranspiredCollector->UTSC(UTSCNum)
                          .SurfPtrs)); // Autodesk:F2C++ Copy of subscripted Area array for use below: This makes a copy so review wrt performance
        AreaSum = sum(Area);
        // now figure area-weighted averages from underlying surfaces.
        //        Vwind = sum( LocalWindArr * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage:
        // Replaced by below
        Vwind = sum(LocalWindArr * Area) / AreaSum;
        LocalWindArr.deallocate();
        //        HrSky = sum( HSkyARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced
        // by  below
        HrSky = sum(HSkyARR * Area) / AreaSum;
        HSkyARR.deallocate();
        //        HrGround = sum( HGroundARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage:
        // Replaced by below
        HrGround = sum(HGroundARR * Area) / AreaSum;
        HGroundARR.deallocate();
        //        HrAtm = sum( HAirARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage: Replaced
        // by  below
        HrAtm = sum(HAirARR * Area) / AreaSum;
        HAirARR.deallocate();
        //        HrPlen = sum( HPlenARR * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array subscript usage:
        // Replaced  by below
        HrPlen = sum(HPlenARR * Area) / AreaSum;
        HPlenARR.deallocate();

        //        Isc = sum( QRadSWOutIncident( UTSC( UTSCNum ).SurfPtrs ) * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum;
        ////Autodesk:F2C++ Array subscript usage: Replaced by below
        Isc = sum_product_sub(state.dataHeatBal->SurfQRadSWOutIncident,
                              state.dataSurface->Surface,
                              &SurfaceData::Area,
                              state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) /
              AreaSum; // Autodesk:F2C++ Functions handle array subscript usage
        //        Tso = sum( TH( UTSC( UTSCNum ).SurfPtrs, 1, 1 ) * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / AreaSum; //Autodesk:F2C++ Array
        // subscript usage: Replaced by below
        Tso = sum_product_sub(state.dataHeatBalSurf->TH(1, 1, _),
                              state.dataSurface->Surface,
                              &SurfaceData::Area,
                              state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) /
              AreaSum; // Autodesk:F2C++ Functions handle array subscript usage

        if (Vwind > 5.0) {
            HcWind = 5.62 + 3.9 * (Vwind - 5.0); // McAdams forced convection correlation
        } else {
            HcWind = 0.0;
        }

        if (state.dataEnvrn->IsRain) HcWind = 1000.0;

        HXeff = 0.0; // init

        {
            auto const SELECT_CASE_var(state.dataTranspiredCollector->UTSC(UTSCNum).Correlation);

            if (SELECT_CASE_var == state.dataTranspiredCollector->Correlation_Kutscher1994) { // Kutscher1994

                AlessHoles = A - holeArea;

                NuD = 2.75 * ((std::pow(P / D, -1.2) * std::pow(ReD, 0.43)) + (0.011 * Por * ReD * std::pow(Vwind / Vsuction, 0.48)));
                U = k * NuD / D;
                HXeff = 1.0 - std::exp(-1.0 * ((U * AlessHoles) / (Mdot * CpAir)));

            } else if (SELECT_CASE_var == state.dataTranspiredCollector->Correlation_VanDeckerHollandsBrunger2001) { // VanDeckerHollandsBrunger2001
                t = state.dataTranspiredCollector->UTSC(UTSCNum).CollectThick;
                ReS = Vsuction * P / nu;
                ReW = Vwind * P / nu;
                ReB = Vholes * P / nu;
                ReH = (Vsuction * D) / (nu * Por);
                if (ReD > 0.0) {
                    if (ReW > 0.0) {
                        HXeff = (1.0 - std::pow(1.0 + ReS * max(1.733 * std::pow(ReW, -0.5), 0.02136), -1.0)) *
                                (1.0 - std::pow(1.0 + 0.2273 * std::sqrt(ReB), -1.0)) * std::exp(-0.01895 * (P / D) - (20.62 / ReH) * (t / D));
                    } else {
                        HXeff = (1.0 - std::pow(1.0 + ReS * 0.02136, -1.0)) * (1.0 - std::pow(1.0 + 0.2273 * std::sqrt(ReB), -1.0)) *
                                std::exp(-0.01895 * (P / D) - (20.62 / ReH) * (t / D));
                    }
                } else {
                    HXeff = 0.0;
                }
            }
        }

        // now calculate collector temperature

        Tscoll = (Isc * SolAbs + HrAtm * Tamb + HrSky * state.dataEnvrn->SkyTemp + HrGround * Tamb + HrPlen * Tso + HcWind * Tamb +
                  (Mdot * CpAir / A) * Tamb - (Mdot * CpAir / A) * (1.0 - HXeff) * Tamb + QdotSource) /
                 (HrAtm + HrSky + HrGround + HrPlen + HcWind + (Mdot * CpAir / A) * HXeff);

        // Heat exchanger leaving temperature
        TaHX = HXeff * Tscoll + (1.0 - HXeff) * Tamb;

        // now calculate plenum air temperature

        Taplen = (Mdot * CpAir * TaHX + HcPlen * A * Tso) / (Mdot * CpAir + HcPlen * A);

        // calculate Sensible Heating Rate
        if (Taplen > Tamb) {
            SensHeatingRate = Mdot * CpAir * (Taplen - Tamb);
        } else {
            SensHeatingRate = 0.0;
        }

        // now fill results into derived types
        state.dataTranspiredCollector->UTSC(UTSCNum).Isc = Isc;
        state.dataTranspiredCollector->UTSC(UTSCNum).HXeff = HXeff;
        state.dataTranspiredCollector->UTSC(UTSCNum).Tplen = Taplen;
        state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll = Tscoll;
        state.dataTranspiredCollector->UTSC(UTSCNum).HrPlen = HrPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).HcPlen = HcPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).TairHX = TaHX;
        state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot = Mdot;
        state.dataTranspiredCollector->UTSC(UTSCNum).InletTempDB = Tamb;
        state.dataTranspiredCollector->UTSC(UTSCNum).Vsuction = Vsuction;
        state.dataTranspiredCollector->UTSC(UTSCNum).PlenumVelocity = Vplen;
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutTemp = Taplen;
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutHumRat = state.dataEnvrn->OutHumRat; // stays the same with sensible heating
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutEnth =
            PsyHFnTdbW(state.dataTranspiredCollector->UTSC(UTSCNum).SupOutTemp, state.dataTranspiredCollector->UTSC(UTSCNum).SupOutHumRat);
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutMassFlow = Mdot;
        state.dataTranspiredCollector->UTSC(UTSCNum).SensHeatingRate = SensHeatingRate;
        state.dataTranspiredCollector->UTSC(UTSCNum).SensHeatingEnergy = SensHeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveACH = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotVent = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotWind = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotTherm = 0.0;
        if (Isc > 10.0) {
            state.dataTranspiredCollector->UTSC(UTSCNum).UTSCEfficiency = SensHeatingRate / (Isc * A);
            if (TaHX > Tamb) {
                state.dataTranspiredCollector->UTSC(UTSCNum).UTSCCollEff = Mdot * CpAir * (TaHX - Tamb) / (Isc * A);
            } else {
                state.dataTranspiredCollector->UTSC(UTSCNum).UTSCCollEff = 0.0;
            }
        } else {
            state.dataTranspiredCollector->UTSC(UTSCNum).UTSCEfficiency = 0.0;
            state.dataTranspiredCollector->UTSC(UTSCNum).UTSCCollEff = 0.0;
        }
    }

    void CalcPassiveTranspiredCollector(EnergyPlusData &state, int const UTSCNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // model the effect of the a ventilated baffle covering the outside of a heat transfer surface.

        // METHODOLOGY EMPLOYED:
        // All the work is done in a subroutine .

        // REFERENCES:
        // Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

        // Using/Aliasing
        using DataSurfaces::SurfaceData;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyWFnTdbTwbPb;

        // local working variables
        Real64 AspRat; // Aspect Ratio of gap
        Real64 TmpTscoll;
        Real64 TmpTaPlen;
        Real64 RhoAir;
        Real64 holeArea;
        Real64 Tamb;
        Real64 HrPlen;
        Real64 HcPlen;
        Real64 Isc;
        Real64 MdotVent;
        Real64 VdotWind;
        Real64 VdotThermal;
        Real64 Twbamb;
        Real64 OutHumRatAmb;

        //        Tamb = sum( Surface( UTSC( UTSCNum ).SurfPtrs ).OutDryBulbTemp * Surface( UTSC( UTSCNum ).SurfPtrs ).Area ) / sum( Surface(
        // UTSC( UTSCNum ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
        Real64 sum_area = 0.0;
        Real64 sum_produc_area_drybulb = 0.0;
        Real64 sum_produc_area_wetbulb = 0.0;
        for (int SurfNum : state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs) {
            sum_area += state.dataSurface->Surface(SurfNum).Area;
            sum_produc_area_wetbulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutWetBulbTemp(SurfNum);
            sum_produc_area_drybulb += state.dataSurface->Surface(SurfNum).Area * state.dataSurface->SurfOutDryBulbTemp(SurfNum);
        }
        Tamb = sum_produc_area_drybulb / sum_area;
        Twbamb = sum_produc_area_wetbulb / sum_area;

        OutHumRatAmb = PsyWFnTdbTwbPb(state, Tamb, Twbamb, state.dataEnvrn->OutBaroPress);

        RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tamb, OutHumRatAmb);
        holeArea = state.dataTranspiredCollector->UTSC(UTSCNum).ActualArea * state.dataTranspiredCollector->UTSC(UTSCNum).Porosity;

        AspRat = state.dataTranspiredCollector->UTSC(UTSCNum).Height / state.dataTranspiredCollector->UTSC(UTSCNum).PlenGapThick;
        TmpTscoll = state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast;
        TmpTaPlen = state.dataTranspiredCollector->UTSC(UTSCNum).TplenLast;

        // all the work is done in this routine located in GeneralRoutines.cc

        CalcPassiveExteriorBaffleGap(state,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).SurfPtrs,
                                     holeArea,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).Cv,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).Cd,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).HdeltaNPL,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).SolAbsorp,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).LWEmitt,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).Tilt,
                                     AspRat,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).PlenGapThick,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).CollRoughness,
                                     state.dataTranspiredCollector->UTSC(UTSCNum).QdotSource,
                                     TmpTscoll,
                                     TmpTaPlen,
                                     HcPlen,
                                     HrPlen,
                                     Isc,
                                     MdotVent,
                                     VdotWind,
                                     VdotThermal);

        // now fill results into derived types
        state.dataTranspiredCollector->UTSC(UTSCNum).Isc = Isc;
        state.dataTranspiredCollector->UTSC(UTSCNum).Tplen = TmpTaPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll = TmpTscoll;
        state.dataTranspiredCollector->UTSC(UTSCNum).HrPlen = HrPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).HcPlen = HcPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).TairHX = Tamb;
        state.dataTranspiredCollector->UTSC(UTSCNum).InletMDot = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).InletTempDB = Tamb;
        state.dataTranspiredCollector->UTSC(UTSCNum).Vsuction = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).PlenumVelocity = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutTemp = TmpTaPlen;
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutHumRat = OutHumRatAmb;
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutEnth = PsyHFnTdbW(TmpTaPlen, OutHumRatAmb);
        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutMassFlow = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).SensHeatingRate = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).SensHeatingEnergy = 0.0;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveACH =
            (MdotVent / RhoAir) *
            (1.0 / (state.dataTranspiredCollector->UTSC(UTSCNum).ProjArea * state.dataTranspiredCollector->UTSC(UTSCNum).PlenGapThick)) *
            DataGlobalConstants::SecInHour;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotVent = MdotVent;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotWind = VdotWind * RhoAir;
        state.dataTranspiredCollector->UTSC(UTSCNum).PassiveMdotTherm = VdotThermal * RhoAir;
        state.dataTranspiredCollector->UTSC(UTSCNum).UTSCEfficiency = 0.0;
    }

    void UpdateTranspiredCollector(EnergyPlusData &state, int const UTSCNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.T. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int OutletNode;
        int InletNode;
        int thisOSCM;
        int thisOASys;

        // update "last" values in Derived type
        state.dataTranspiredCollector->UTSC(UTSCNum).TplenLast = state.dataTranspiredCollector->UTSC(UTSCNum).Tplen;
        state.dataTranspiredCollector->UTSC(UTSCNum).TcollLast = state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll;

        // Set the outlet air nodes of the UTSC

        if (state.dataTranspiredCollector->UTSC(UTSCNum).IsOn) { // Active
            if (state.dataTranspiredCollector->UTSC(UTSCNum).NumOASysAttached == 1) {
                OutletNode = state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode(1);
                InletNode = state.dataTranspiredCollector->UTSC(UTSCNum).InletNode(1);
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataTranspiredCollector->UTSC(UTSCNum).SupOutMassFlow;
                state.dataLoopNodes->Node(OutletNode).Temp = state.dataTranspiredCollector->UTSC(UTSCNum).SupOutTemp;
                state.dataLoopNodes->Node(OutletNode).HumRat = state.dataTranspiredCollector->UTSC(UTSCNum).SupOutHumRat;
                state.dataLoopNodes->Node(OutletNode).Enthalpy = state.dataTranspiredCollector->UTSC(UTSCNum).SupOutEnth;
            } else if (state.dataTranspiredCollector->UTSC(UTSCNum).NumOASysAttached > 1) {
                for (thisOASys = 1; thisOASys <= state.dataTranspiredCollector->UTSC(UTSCNum).NumOASysAttached; ++thisOASys) {
                    state.dataLoopNodes->Node(state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode(thisOASys)).MassFlowRate =
                        state.dataLoopNodes->Node(state.dataTranspiredCollector->UTSC(UTSCNum).InletNode(thisOASys))
                            .MassFlowRate; // system gets what it asked for at inlet
                    state.dataLoopNodes->Node(state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode(thisOASys)).Temp =
                        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutTemp;
                    state.dataLoopNodes->Node(state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode(thisOASys)).HumRat =
                        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutHumRat;
                    state.dataLoopNodes->Node(state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode(thisOASys)).Enthalpy =
                        state.dataTranspiredCollector->UTSC(UTSCNum).SupOutEnth;
                }
            }
        } else { // Passive and/or bypassed           Note Array assignments in following
                 // Autodesk:F2C++ Array subscript usage: Replaced by below
            //            Node( UTSC( UTSCNum ).OutletNode ).MassFlowRate = Node( UTSC( UTSCNum ).InletNode ).MassFlowRate;
            //            Node( UTSC( UTSCNum ).OutletNode ).Temp = Node( UTSC( UTSCNum ).InletNode ).Temp;
            //            Node( UTSC( UTSCNum ).OutletNode ).HumRat = Node( UTSC( UTSCNum ).InletNode ).HumRat;
            //            Node( UTSC( UTSCNum ).OutletNode ).Enthalpy = Node( UTSC( UTSCNum ).InletNode ).Enthalpy;
            auto const &OutletNode(state.dataTranspiredCollector->UTSC(UTSCNum).OutletNode);
            auto const &InletNode(state.dataTranspiredCollector->UTSC(UTSCNum).InletNode);
            assert(OutletNode.size() == InletNode.size());
            for (int io = OutletNode.l(), ii = InletNode.l(), eo = OutletNode.u(); io <= eo; ++io, ++ii) {
                auto &outNode(state.dataLoopNodes->Node(OutletNode(io)));
                auto const &inNode(state.dataLoopNodes->Node(InletNode(ii)));
                outNode.MassFlowRate = inNode.MassFlowRate;
                outNode.Temp = inNode.Temp;
                outNode.HumRat = inNode.HumRat;
                outNode.Enthalpy = inNode.Enthalpy;
            }
        }

        // update the OtherSideConditionsModel coefficients.
        thisOSCM = state.dataTranspiredCollector->UTSC(UTSCNum).OSCMPtr;

        state.dataSurface->OSCM(thisOSCM).TConv = state.dataTranspiredCollector->UTSC(UTSCNum).Tplen;
        state.dataSurface->OSCM(thisOSCM).HConv = state.dataTranspiredCollector->UTSC(UTSCNum).HcPlen;
        state.dataSurface->OSCM(thisOSCM).TRad = state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll;
        state.dataSurface->OSCM(thisOSCM).HRad = state.dataTranspiredCollector->UTSC(UTSCNum).HrPlen;
    }

    void SetUTSCQdotSource(EnergyPlusData &state,
                           int const UTSCNum,
                           Real64 const QSource // source term in Watts
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // object oriented "Set" routine for updating sink term without exposing variables

        // METHODOLOGY EMPLOYED:
        // update derived type with new data , turn power into W/m2

        state.dataTranspiredCollector->UTSC(UTSCNum).QdotSource = QSource / state.dataTranspiredCollector->UTSC(UTSCNum).ProjArea;
    }

    void GetTranspiredCollectorIndex(EnergyPlusData &state, int const SurfacePtr, int &UTSCIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // object oriented "Get" routine for establishing correct integer index from outside this module

        // METHODOLOGY EMPLOYED:
        // mine Surface derived type for correct index/number of surface
        // mine UTSC derived type that has the surface.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UTSCNum;  // temporary
        int ThisSurf; // temporary
        int thisUTSC;
        bool Found;

        if (state.dataTranspiredCollector->GetInputFlag) {
            GetTranspiredCollectorInput(state);
            state.dataTranspiredCollector->GetInputFlag = false;
        }

        if (SurfacePtr == 0) {
            ShowFatalError(state,
                           "Invalid surface passed to GetTranspiredCollectorIndex, Surface name = " + state.dataSurface->Surface(SurfacePtr).Name);
        }

        UTSCNum = 0;
        Found = false;
        for (thisUTSC = 1; thisUTSC <= state.dataTranspiredCollector->NumUTSC; ++thisUTSC) {
            for (ThisSurf = 1; ThisSurf <= state.dataTranspiredCollector->UTSC(thisUTSC).NumSurfs; ++ThisSurf) {
                if (SurfacePtr == state.dataTranspiredCollector->UTSC(thisUTSC).SurfPtrs(ThisSurf)) {
                    Found = true;
                    UTSCNum = thisUTSC;
                }
            }
        }

        if (!Found) {
            ShowFatalError(state,
                           "Did not find surface in UTSC description in GetTranspiredCollectorIndex, Surface name = " +
                               state.dataSurface->Surface(SurfacePtr).Name);
        } else {

            UTSCIndex = UTSCNum;
        }
    }

    void GetUTSCTsColl(EnergyPlusData &state, int const UTSCNum, Real64 &TsColl)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // object oriented "Get" routine for collector surface temperature

        // METHODOLOGY EMPLOYED:
        // access derived type

        TsColl = state.dataTranspiredCollector->UTSC(UTSCNum).Tcoll;
    }

    int GetAirInletNodeNum(EnergyPlusData &state, std::string const &UTSCName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given UTSC and returns the air inlet node number.
        // If incorrect UTSC name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichUTSC;

        if (state.dataTranspiredCollector->GetInputFlag) {
            GetTranspiredCollectorInput(state);
            state.dataTranspiredCollector->GetInputFlag = false;
        }

        WhichUTSC = UtilityRoutines::FindItemInList(UTSCName, state.dataTranspiredCollector->UTSC);
        if (WhichUTSC != 0) {
            NodeNum = state.dataTranspiredCollector->UTSC(WhichUTSC).InletNode(1);
        } else {
            ShowSevereError(state, "GetAirInletNodeNum: Could not find TranspiredCollector = \"" + UTSCName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

    int GetAirOutletNodeNum(EnergyPlusData &state, std::string const &UTSCName, bool &ErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May 2019
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given UTSC and returns the air outlet node number.
        // If incorrect UTSC name is given, ErrorsFound is returned as true and node number as zero.

        // Return value
        int NodeNum; // node number returned

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichUTSC;

        if (state.dataTranspiredCollector->GetInputFlag) {
            GetTranspiredCollectorInput(state);
            state.dataTranspiredCollector->GetInputFlag = false;
        }

        WhichUTSC = UtilityRoutines::FindItemInList(UTSCName, state.dataTranspiredCollector->UTSC);
        if (WhichUTSC != 0) {
            NodeNum = state.dataTranspiredCollector->UTSC(WhichUTSC).OutletNode(1);
        } else {
            ShowSevereError(state, "GetAirOutletNodeNum: Could not find TranspiredCollector = \"" + UTSCName + "\"");
            ErrorsFound = true;
            NodeNum = 0;
        }

        return NodeNum;
    }

} // namespace TranspiredCollector

} // namespace EnergyPlus
