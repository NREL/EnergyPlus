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
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/Pipes.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace Pipes {

    // Module containing the routines dealing with the <module_name>

    // MODULE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       Rahul Chillar , Jan 2005
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Added steam pipe to the module: RC

    // Using/Aliasing
    using namespace DataHVACGlobals;
    using namespace DataLoopNode;
    using DataPlant::TypeOf_Pipe;
    using DataPlant::TypeOf_PipeSteam;

    PlantComponent *LocalPipeData::factory(EnergyPlusData &state, int objectType, std::string const &objectName)
    {
        // Process the input data for pipes if it hasn't been done already
        if (state.dataPipes->GetPipeInputFlag) {
            GetPipeInput(state);
            state.dataPipes->GetPipeInputFlag = false;
        }
        // Now look for this particular pipe in the list
        for (auto &pipe : state.dataPipes->LocalPipe) {
            if (pipe.TypeOf == objectType && pipe.Name == objectName) {
                return &pipe;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state, "LocalPipeDataFactory: Error getting inputs for pipe named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void LocalPipeData::simulate(EnergyPlusData &state,
                                 [[maybe_unused]] const PlantLocation &calledFromLocation,
                                 [[maybe_unused]] bool const FirstHVACIteration,
                                 [[maybe_unused]] Real64 &CurLoad,
                                 [[maybe_unused]] bool const RunFlag)
    {
        if (this->OneTimeInit) {
            int FoundOnLoop = 0;
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                this->Name, this->TypeOf, this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex, errFlag, _, _, FoundOnLoop, _, _);
            if (FoundOnLoop == 0) {
                ShowFatalError(state, "SimPipes: Pipe=\"" + this->Name + "\" not found on a Plant Loop."); // LCOV_EXCL_LINE
            }
            if (errFlag) {
                ShowFatalError(state, "SimPipes: Program terminated due to previous condition(s)."); // LCOV_EXCL_LINE
            }
            this->OneTimeInit = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && this->EnvrnFlag) {
            PlantUtilities::InitComponentNodes(0.0,
                                               state.dataPlnt->PlantLoop(this->LoopNum).MaxMassFlowRate,
                                               this->InletNodeNum,
                                               this->OutletNodeNum,
                                               this->LoopNum,
                                               this->LoopSide,
                                               this->BranchIndex,
                                               this->CompIndex);
            this->EnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) this->EnvrnFlag = true;

        PlantUtilities::SafeCopyPlantNode(state, this->InletNodeNum, this->OutletNodeNum, this->LoopNum);
    }

    void GetPipeInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine needs a description.

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using BranchNodeConnections::TestCompSet;
        using NodeInputManager::GetOnlySingleNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PipeNum;
        int NumWaterPipes;
        int NumSteamPipes;
        int PipeSteamNum;
        int PipeWaterNum;
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // GET NUMBER OF ALL EQUIPMENT TYPES
        NumWaterPipes = inputProcessor->getNumObjectsFound(state, "Pipe:Adiabatic");
        NumSteamPipes = inputProcessor->getNumObjectsFound(state, "Pipe:Adiabatic:Steam");
        state.dataPipes->NumLocalPipes = NumWaterPipes + NumSteamPipes;
        state.dataPipes->LocalPipe.allocate(state.dataPipes->NumLocalPipes);
        state.dataPipes->LocalPipeUniqueNames.reserve(static_cast<unsigned>(state.dataPipes->NumLocalPipes));

        cCurrentModuleObject = "Pipe:Adiabatic";
        for (PipeWaterNum = 1; PipeWaterNum <= NumWaterPipes; ++PipeWaterNum) {
            PipeNum = PipeWaterNum;
            inputProcessor->getObjectItem(state, cCurrentModuleObject, PipeWaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat);
            GlobalNames::VerifyUniqueInterObjectName(state, state.dataPipes->LocalPipeUniqueNames, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            state.dataPipes->LocalPipe(PipeNum).Name = cAlphaArgs(1);
            state.dataPipes->LocalPipe(PipeNum).TypeOf = TypeOf_Pipe;

            state.dataPipes->LocalPipe(PipeNum).InletNodeNum = GetOnlySingleNode(state,
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            state.dataPipes->LocalPipe(PipeNum).OutletNodeNum = GetOnlySingleNode(state,
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Pipe Nodes");
        }

        PipeNum = NumWaterPipes;
        cCurrentModuleObject = "Pipe:Adiabatic:Steam";

        for (PipeSteamNum = 1; PipeSteamNum <= NumSteamPipes; ++PipeSteamNum) {
            ++PipeNum;
            inputProcessor->getObjectItem(state, cCurrentModuleObject, PipeSteamNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat);
            GlobalNames::VerifyUniqueInterObjectName(state, state.dataPipes->LocalPipeUniqueNames, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            state.dataPipes->LocalPipe(PipeNum).Name = cAlphaArgs(1);
            state.dataPipes->LocalPipe(PipeNum).TypeOf = TypeOf_PipeSteam;
            state.dataPipes->LocalPipe(PipeNum).InletNodeNum = GetOnlySingleNode(state,
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            state.dataPipes->LocalPipe(PipeNum).OutletNodeNum = GetOnlySingleNode(state,
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(state, cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Pipe Nodes");
        }

        if (ErrorsFound) {
            ShowFatalError(state, "GetPipeInput: Errors getting input for pipes"); // LCOV_EXCL_LINE
        }
    }

} // namespace Pipes

} // namespace EnergyPlus
