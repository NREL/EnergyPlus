// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <BranchNodeConnections.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Pipes.hh>
#include <Plant/PlantLocation.hh>
#include <PlantComponent.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

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

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataHVACGlobals;
    using namespace DataLoopNode;
    using DataPlant::TypeOf_Pipe;
    using DataPlant::TypeOf_PipeSteam;

    // Use statements for access to subroutines in other modules

    // Data
    // MODULE PARAMETER DEFINITIONS
    // na

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    int NumLocalPipes(0);
    bool GetPipeInputFlag(true);

    // SUBROUTINE SPECIFICATIONS FOR MODULE Pipe

    // Object Data
    Array1D<LocalPipeData> LocalPipe; // dimension to number of pipes
    std::unordered_map<std::string, std::string> LocalPipeUniqueNames;

    // Functions
    void clear_state()
    {
        NumLocalPipes = 0;
        GetPipeInputFlag = true;
        LocalPipe.deallocate();
        LocalPipeUniqueNames.clear();
    }

    PlantComponent *LocalPipeData::factory(int objectType, std::string objectName)
    {
        // Process the input data for pipes if it hasn't been done already
        if (GetPipeInputFlag) {
            GetPipeInput();
            GetPipeInputFlag = false;
        }
        // Now look for this particular pipe in the list
        for (auto &pipe : LocalPipe) {
            if (pipe.TypeOf == objectType && pipe.Name == objectName) {
                return &pipe;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalPipeDataFactory: Error getting inputs for pipe named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void LocalPipeData::simulate(const PlantLocation &EP_UNUSED(calledFromLocation),
                                 bool const EP_UNUSED(FirstHVACIteration),
                                 Real64 &EP_UNUSED(CurLoad),
                                 bool const EP_UNUSED(RunFlag))
    {
        if (this->OneTimeInit) {
            int FoundOnLoop = 0;
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name, this->TypeOf, this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex, _, _,
                                                    FoundOnLoop, _, _, errFlag);
            if (FoundOnLoop == 0) {
                ShowFatalError("SimPipes: Pipe=\"" + this->Name + "\" not found on a Plant Loop."); // LCOV_EXCL_LINE
            }
            if (errFlag) {
                ShowFatalError("SimPipes: Program terminated due to previous condition(s)."); // LCOV_EXCL_LINE
            }
            this->OneTimeInit = false;
        }

        if (DataGlobals::BeginEnvrnFlag && this->EnvrnFlag) {
            PlantUtilities::InitComponentNodes(0.0, DataPlant::PlantLoop(this->LoopNum).MaxMassFlowRate, this->InletNodeNum, this->OutletNodeNum,
                                               this->LoopNum, this->LoopSide, this->BranchIndex, this->CompIndex);
            this->EnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) this->EnvrnFlag = true;

        PlantUtilities::SafeCopyPlantNode(this->InletNodeNum, this->OutletNodeNum, this->LoopNum);
    }

    void GetPipeInput()
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
        static bool ErrorsFound(false);

        // GET NUMBER OF ALL EQUIPMENT TYPES
        NumWaterPipes = inputProcessor->getNumObjectsFound("Pipe:Adiabatic");
        NumSteamPipes = inputProcessor->getNumObjectsFound("Pipe:Adiabatic:Steam");
        NumLocalPipes = NumWaterPipes + NumSteamPipes;
        LocalPipe.allocate(NumLocalPipes);
        LocalPipeUniqueNames.reserve(static_cast<unsigned>(NumLocalPipes));

        cCurrentModuleObject = "Pipe:Adiabatic";
        for (PipeWaterNum = 1; PipeWaterNum <= NumWaterPipes; ++PipeWaterNum) {
            PipeNum = PipeWaterNum;
            inputProcessor->getObjectItem(cCurrentModuleObject, PipeWaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat);
            GlobalNames::VerifyUniqueInterObjectName(LocalPipeUniqueNames, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            LocalPipe(PipeNum).Name = cAlphaArgs(1);
            LocalPipe(PipeNum).TypeOf = TypeOf_Pipe;

            LocalPipe(PipeNum).InletNodeNum = GetOnlySingleNode(cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water,
                                                                NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            LocalPipe(PipeNum).OutletNodeNum = GetOnlySingleNode(cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water,
                                                                 NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Pipe Nodes");
        }

        PipeNum = NumWaterPipes;
        cCurrentModuleObject = "Pipe:Adiabatic:Steam";

        for (PipeSteamNum = 1; PipeSteamNum <= NumSteamPipes; ++PipeSteamNum) {
            ++PipeNum;
            inputProcessor->getObjectItem(cCurrentModuleObject, PipeSteamNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat);
            GlobalNames::VerifyUniqueInterObjectName(LocalPipeUniqueNames, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            LocalPipe(PipeNum).Name = cAlphaArgs(1);
            LocalPipe(PipeNum).TypeOf = TypeOf_PipeSteam;
            LocalPipe(PipeNum).InletNodeNum = GetOnlySingleNode(cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Steam,
                                                                NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            LocalPipe(PipeNum).OutletNodeNum = GetOnlySingleNode(cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Steam,
                                                                 NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Pipe Nodes");
        }

        if (ErrorsFound) {
            ShowFatalError("GetPipeInput: Errors getting input for pipes"); // LCOV_EXCL_LINE
        }
    }

} // namespace Pipes

} // namespace EnergyPlus
