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

#ifndef PlantUtilities_hh_INCLUDED
#define PlantUtilities_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/Enums.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace PlantUtilities {

    // Functions
    void InitComponentNodes(EnergyPlusData &state,
                            Real64 MinCompMdot,
                            Real64 MaxCompMdot,
                            int InletNode,   // component's inlet node index in node structure
                            int OutletNode,  // component's outlet node index in node structure
                            int LoopNum,     // plant loop index for PlantLoop structure
                            int LoopSideNum, // Loop side index for PlantLoop structure
                            int BranchIndex, // branch index for PlantLoop
                            int CompIndex    // component index for PlantLoop
    );

    void SetComponentFlowRate(EnergyPlusData &state,
                              Real64 &CompFlow, // [kg/s]
                              int InletNode,    // component's inlet node index in node structure
                              int OutletNode,   // component's outlet node index in node structure
                              int LoopNum,      // plant loop index for PlantLoop structure
                              int LoopSideNum,  // Loop side index for PlantLoop structure
                              int BranchIndex,  // branch index for PlantLoop
                              int CompIndex     // component index for PlantLoop
    );

    void SetActuatedBranchFlowRate(EnergyPlusData &state,
                                   Real64 &CompFlow,
                                   int ActuatedNode,
                                   int LoopNum,
                                   int LoopSideNum,
                                   int BranchNum,
                                   bool ResetMode // flag to indicate if this is a real flow set, or a reset flow setting.
    );

    Real64
    RegulateCondenserCompFlowReqOp(EnergyPlusData &state, int LoopNum, int LoopSideNum, int BranchNum, int CompNum, Real64 TentativeFlowRequest);

    bool AnyPlantSplitterMixerLacksContinuity(EnergyPlusData &state);

    void CheckPlantMixerSplitterConsistency(EnergyPlusData &state, int LoopNum, int LoopSideNum, bool FirstHVACIteration);

    void CheckForRunawayPlantTemps(EnergyPlusData &state, int LoopNum, int LoopSideNum);

    void SetAllFlowLocks(EnergyPlusData &state, DataPlant::iFlowLock Value);

    void ResetAllPlantInterConnectFlags(EnergyPlusData &state);

    void PullCompInterconnectTrigger(EnergyPlusData &state,
                                     int LoopNum,                           // component's loop index
                                     int LoopSide,                          // component's loop side number
                                     int BranchNum,                         // Component's branch number
                                     int CompNum,                           // Component's comp number
                                     int &UniqueCriteriaCheckIndex,         // An integer given to this particular check
                                     int ConnectedLoopNum,                  // Component's interconnected loop number
                                     int ConnectedLoopSide,                 // Component's interconnected loop side number
                                     DataPlant::iCriteriaType CriteriaType, // The criteria check to use, see DataPlant: SimFlagCriteriaTypes
                                     Real64 CriteriaValue                   // The value of the criteria check to evaluate
    );

    void UpdateChillerComponentCondenserSide(EnergyPlusData &state,
                                             int LoopNum,                   // component's loop index
                                             int LoopSide,                  // component's loop side number
                                             int TypeOfNum,                 // Component's type index
                                             int InletNodeNum,              // Component's inlet node pointer
                                             int OutletNodeNum,             // Component's outlet node pointer
                                             Real64 ModelCondenserHeatRate, // model's heat rejection rate at condenser (W)
                                             Real64 ModelInletTemp,         // model's inlet temperature (C)
                                             Real64 ModelOutletTemp,        // model's outlet temperature (C)
                                             Real64 ModelMassFlowRate,      // model's condenser water mass flow rate (kg/s)
                                             bool FirstHVACIteration);

    void UpdateComponentHeatRecoverySide(EnergyPlusData &state,
                                         int LoopNum,                  // component's loop index
                                         int LoopSide,                 // component's loop side number
                                         int TypeOfNum,                // Component's type index
                                         int InletNodeNum,             // Component's inlet node pointer
                                         int OutletNodeNum,            // Component's outlet node pointer
                                         Real64 ModelRecoveryHeatRate, // model's heat rejection rate at recovery (W)
                                         Real64 ModelInletTemp,        // model's inlet temperature (C)
                                         Real64 ModelOutletTemp,       // model's outlet temperature (C)
                                         Real64 ModelMassFlowRate,     // model's condenser water mass flow rate (kg/s)
                                         bool FirstHVACIteration);

    void UpdateAbsorberChillerComponentGeneratorSide(EnergyPlusData &state,
                                                     int LoopNum,                                // component's loop index
                                                     int LoopSide,                               // component's loop side number
                                                     int TypeOfNum,                              // Component's type index
                                                     int InletNodeNum,                           // Component's inlet node pointer
                                                     int OutletNodeNum,                          // Component's outlet node pointer
                                                     DataLoopNode::NodeFluidType HeatSourceType, // Type of fluid in Generator loop
                                                     Real64 ModelGeneratorHeatRate,              // model's generator heat rate (W)
                                                     Real64 ModelMassFlowRate,                   // model's generator mass flow rate (kg/s)
                                                     bool FirstHVACIteration);

    void InterConnectTwoPlantLoopSides(EnergyPlusData &state,
                                       int Loop1Num,
                                       int Loop1LoopSideNum,
                                       int Loop2Num,
                                       int Loop2LoopSideNum,
                                       int PlantComponentTypeOfNum,
                                       bool Loop1DemandsOnLoop2);

    void ShiftPlantLoopSideCallingOrder(EnergyPlusData &state, int OldIndex, int NewIndex);

    void RegisterPlantCompDesignFlow(EnergyPlusData &state,
                                     int ComponentInletNodeNum, // the component's water inlet node number
                                     Real64 DesPlantFlow        // the component's design fluid volume flow rate [m3/s]
    );

    void SafeCopyPlantNode(EnergyPlusData &state,
                           int InletNodeNum,
                           int OutletNodeNum,
                           Optional_int_const LoopNum = _,
                           Optional<Real64 const> OutletTemp = _ // set on outlet node if present and water.
    );

    Real64 BoundValueToNodeMinMaxAvail(EnergyPlusData &state, Real64 ValueToBound, int NodeNumToBoundWith);

    void TightenNodeMinMaxAvails(EnergyPlusData &state, int NodeNum, Real64 NewMinAvail, Real64 NewMaxAvail);

    Real64 BoundValueToWithinTwoValues(Real64 ValueToBound, Real64 LowerBound, Real64 UpperBound);

    bool IntegerIsWithinTwoValues(int ValueToCheck, int LowerBound, int UpperBound);

    void LogPlantConvergencePoints(EnergyPlusData &state, bool FirstHVACIteration);

    void ScanPlantLoopsForObject(EnergyPlusData &state,
                                 std::string_view CompName,
                                 int CompType,
                                 int &LoopNum,
                                 int &LoopSideNum,
                                 int &BranchNum,
                                 int &CompNum,
                                 bool &errFlag,
                                 Optional<Real64 const> LowLimitTemp = _,
                                 Optional<Real64 const> HighLimitTemp = _,
                                 Optional_int CountMatchPlantLoops = _,
                                 Optional_int_const InletNodeNumber = _,
                                 Optional_int_const SingleLoopSearch = _);

    void ScanPlantLoopsForNodeNum(EnergyPlusData &state,
                                  std::string_view const CallerName, // really used for error messages
                                  int NodeNum,                       // index in Node structure of node to be scanned
                                  int &LoopNum,                      // return value for plant loop
                                  int &LoopSideNum,                  // return value for plant loop side
                                  int &BranchNum,
                                  Optional_int CompNum = _);

    bool AnyPlantLoopSidesNeedSim(EnergyPlusData &state);

    void SetAllPlantSimFlagsToValue(EnergyPlusData &state, bool Value);

    void ShowBranchesOnLoop(EnergyPlusData &state, int LoopNum); // Loop number of loop

    int MyPlantSizingIndex(EnergyPlusData &state,
                           std::string const &CompType,          // component description
                           std::string_view CompName,            // user name of component
                           int NodeNumIn,                        // component water inlet node
                           int NodeNumOut,                       // component water outlet node
                           bool &ErrorsFound,                    // set to true if there's an error
                           Optional_bool_const SupressErrors = _ // used for WSHP's where condenser loop may not be on a plant loop
    );

    bool verifyTwoNodeNumsOnSamePlantLoop(EnergyPlusData &state, int nodeIndexA, int nodeIndexB);

    struct CriteriaData
    {
        // Members
        int CallingCompLoopNum;        // for debug error handling
        int CallingCompLoopSideNum;    // for debug error handling
        int CallingCompBranchNum;      // for debug error handling
        int CallingCompCompNum;        // for debug error handling
        Real64 ThisCriteriaCheckValue; // the previous value, to check the current against

        // Default Constructor
        CriteriaData() : CallingCompLoopNum(0), CallingCompLoopSideNum(0), CallingCompBranchNum(0), CallingCompCompNum(0), ThisCriteriaCheckValue(0.0)
        {
        }
    };

} // namespace PlantUtilities

struct PlantUtilitiesData : BaseGlobalStruct
{

    Array1D<PlantUtilities::CriteriaData> CriteriaChecks; // stores criteria information

    void clear_state() override
    {
        this->CriteriaChecks.deallocate();
    }
};

} // namespace EnergyPlus

#endif
