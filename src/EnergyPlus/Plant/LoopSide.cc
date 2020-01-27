// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Plant/LoopSide.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantPressureSystem.hh>
#include <EnergyPlus/Pumps.hh>

namespace EnergyPlus {
namespace DataPlant {

    void HalfLoopData::ValidateFlowControlPaths()
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This routine will scan all the loop side paths and validate the component topology according
        //  to current topology rules and regulations.

        // METHODOLOGY EMPLOYED:
        // Scan this loop side and begin by scanning the first branch, then follow with the remainder of the flow paths
        //  - this would be from splitter outlet nodes all the way to the loop side outlet node.
        // The current rules are that "other types" of components (as defined below in the references) can be placed along each
        //  flow path as needed.  At this point, any number of "load-range based" components can be placed along the flow
        //  path.  After this, the user is allowed to place another set of any number of "other types" of components.
        // The key restriction is that an "other type" of component may not be sandwiched by "load-range based" components.
        // This is due to the load range based needing to be simulated all at once along each flow path.

        // REFERENCES:
        // "other types" of components: basically not load-range based heat transfer components.  This would include:
        //    - demand based components such as coils
        //    - component setpoint based operating components
        //    - heat exchanger components including waterside economizers
        // "load-range based" components are heat transfer components which are controlled based on a single load range.
        //    - currently only one load range based scheme is available at a given time, although other control types
        //      may be enabled, such as component setpoint.
        // Pumps are separate components since the pump heat is not accounted for in the flow path order.
        //  Improvements during the demand side rewrite has allowed pumps to be placed as -not- the first component on a branch
        //  Pumps can be placed anywhere, even between load-range based components, since they will not affect loop load

        // RETURN VALUE:
        // Returns a control validator flow structure, including a flag for successful or not, then if not successful
        //  the other values are filled in such as location on the loop where the error occurred and a message error description

        // FUNCTION PARAMETER DEFINITIONS:
        int const Parallel(1);
        int const Outlet(2);

        //~ Initialze
        bool EncounteredLRB = false;
        bool EncounteredNonLRBAfterLRB = false;
        int const NumParallelPaths = this->TotalBranches - 2;

        // We'll start by stepping through the first branch, which may be the only branch
        // If we find a load range based, then trip the flag and just keep moving
        // If we only have one branch and all is good, then RETURN early
        // If we have parallel branches, then start looping through each flow path to
        //  decide if it is a valid path.
        // If any one path is invalid then all is wrong
        int firstBranchIndex = 1;
        for (int CompIndex = 1; CompIndex <= this->Branch(firstBranchIndex).TotalComponents; ++CompIndex) {

            auto &this_component(this->Branch(firstBranchIndex).Comp(CompIndex));

            {
                auto const SELECT_CASE_var(this_component.CurOpSchemeType);

                if ((SELECT_CASE_var >= DataPlant::LoadRangeBasedMin) && (SELECT_CASE_var <= DataPlant::LoadRangeBasedMax)) { //~ load range based
                    if (EncounteredNonLRBAfterLRB) {
                        // We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
                        ShowSevereError("Plant Topology Problem: Load range based components are separated by other control type components.");
                        ShowContinueError("Load Range Based should be grouped together on each flow path.");
                        ShowFatalError("Plant topology issue causes program termination");
                    } else {
                        EncounteredLRB = true;
                    }

                } else if (SELECT_CASE_var == DataPlant::PumpOpSchemeType) { //~ pump
                    // For now this is just a placeholder, because I think pumps will be available anywhere,
                    //  and they won't affect the load distribution

                } else if (SELECT_CASE_var == DataPlant::NoControlOpSchemeType) { //~ Such as pipes
                    // For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

                } else if (SELECT_CASE_var ==
                           DataPlant::UnknownStatusOpSchemeType) { //~ Uninitialized, this should be a sufficient place to catch for this on branch 1
                    // throw fatal
                    ShowSevereError("ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: " + this_component.Name);
                    ShowFatalError("ValidateFlowControlPaths: developer notice, Inlet path validation loop");
                } else { //~ Other control type
                    if (EncounteredLRB) {
                        EncounteredNonLRBAfterLRB = true;
                    } else {
                        // For now don't do anything, but we'll see...
                    }
                }
            }
        }

        // Return early if we only needed to do the one branch
        if (NumParallelPaths <= 0) return;

        // Now, if we have multiple parallel branches, I think the easiest way is to go all the way from the inlet node
        //  of each parallel branch to the loop outlet node and check the flow path
        // This way we don't have to remember the conditions on each of the parallel branches when we would finally move
        //  to analyzing the outlet node when all done
        // This will reduce allocation on the heap because we will keep from storing that array
        // For each parallel path, we will need to check two branches: the parallel branch and the LoopSide outlet branch
        for (int PathCounter = 1; PathCounter <= NumParallelPaths; ++PathCounter) {
            for (int ParallelOrOutletIndex = Parallel; ParallelOrOutletIndex <= Outlet; ++ParallelOrOutletIndex) {
                int BranchIndex;
                if (ParallelOrOutletIndex == Parallel) {
                    // The branch index will be the current pathtype + 1 to add the inlet branch
                    BranchIndex = PathCounter + 1;
                } else { // ParallelOrOutletIndex == Outlet
                    // The branch index will be the LoopSide outlet node
                    BranchIndex = this->TotalBranches;
                }

                // Now that we have the branch index, let's do the control type check over all the components
                for (int CompIndex = 1; CompIndex <= this->Branch(BranchIndex).TotalComponents; ++CompIndex) {

                    auto &this_component(this->Branch(BranchIndex).Comp(CompIndex));

                    {
                        auto const SELECT_CASE_var(this_component.CurOpSchemeType);

                        if ((SELECT_CASE_var >= DataPlant::LoadRangeBasedMin) &&
                            (SELECT_CASE_var <= DataPlant::LoadRangeBasedMax)) { //~ load range based
                            if (EncounteredNonLRBAfterLRB) {
                                // We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
                                ShowSevereError(
                                    "Plant Topology Problem: Load range based components are separated by other control type components.");
                                ShowContinueError("Load Range Based should be grouped together on each flow path.");
                                ShowFatalError("Plant topology issue causes program termination");
                            } else {
                                EncounteredLRB = true;
                            }

                        } else if (SELECT_CASE_var == DataPlant::NoControlOpSchemeType) { //~ Such as pipes
                            // For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

                        } else if (SELECT_CASE_var == DataPlant::PumpOpSchemeType) { //~ pump
                            // For now this is just a placeholder, because I think pumps will be available anywhere,
                            //  and they won't affect the load distribution

                        } else if (SELECT_CASE_var == DataPlant::UnknownStatusOpSchemeType) { //~ Uninitialized, this should be sufficient place to catch for this on other branches
                            // throw fatal error
                            ShowSevereError("ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: " +
                                            this_component.Name);
                            ShowFatalError("ValidateFlowControlPaths: developer notice, problem in Parallel path validation loop");
                        } else { //~ Other control type
                            if (EncounteredLRB) {
                                EncounteredNonLRBAfterLRB = true;
                            } else {
                                // For now don't do anything, but we'll see...
                            }
                        }
                    }

                } //~ CompIndex

            } //~ Parallel and Outlet Branches

        } //~ Parallel Paths
    }

    void HalfLoopData::SimulateAllLoopSideBranches(Real64 const ThisLoopSideFlow, bool const FirstHVACIteration, bool &LoopShutDownFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will step through all branch groups (single branch .OR. inlet/parallels/outlet)
        //  and call the branch group simulation routine.  This routine also calls to update the splitter
        //  and mixer.

        // METHODOLOGY EMPLOYED:
        // The number of branch groups is either 1 or 3.  1 would be a single branch half-loop.  3 would
        //  be the minimum for an inlet/parallels/outlet set.  The number of branch groups can then be
        //  calculated as #BrGrps = 1 + 2*L; where L is zero for single half loop and one for parallel-type set.
        //  This calculation can be reduced to the logical/integer conversion as shown in the code.
        // The simulation then steps through each branch group.  If there are parallel branches, the splitter is
        //  updated on flowlock=0 to pass information through, then after the parallel branches the mixer is always
        //  updated.  The outlet branch "group" is then simulated.

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const InletBranchOrOneBranchHalfLoop(1);
        int const ParallelBranchSet(2);
        int const OutletBranch(3);

        int NumBranchGroups = 1;
        if (this->TotalBranches > 1) {
            NumBranchGroups = 3;
        }

        // reset branch starting component index back to zero before each pass
        for (int BranchCounter = 1; BranchCounter <= this->TotalBranches; ++BranchCounter) {
            this->Branch(BranchCounter).lastComponentSimulated = 0;
        }

        for (int BranchGroup = 1; BranchGroup <= NumBranchGroups; ++BranchGroup) {

            if ((BranchGroup > 1) && (this->TotalBranches == 1)) break;

            switch (BranchGroup) {
            case InletBranchOrOneBranchHalfLoop:
                this->SimulateLoopSideBranchGroup(1, 1, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag);
                break;
            case ParallelBranchSet:
                this->UpdatePlantSplitter();
                this->SimulateLoopSideBranchGroup(2, this->TotalBranches - 1, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag);
                this->UpdatePlantMixer();
                break;
            case OutletBranch:
                this->SimulateLoopSideBranchGroup(this->TotalBranches, this->TotalBranches, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag);
                break;
            }
        }
    }

    void HalfLoopData::SimulateLoopSideBranchGroup(
        int const FirstBranchNum, int const LastBranchNum, Real64 FlowRequest, bool const FirstHVACIteration, bool &LoopShutDownFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will manage the component simulation on a single set of parallel branches
        // This routine also reverts to a single branch simulation if there isn't a set of parallel branches

        // METHODOLOGY EMPLOYED:
        // Loop through all components, and simulate first the non-load range based on each branch.
        // When a load-range based (LRB) is encountered, the simulation moves to the next branch to do non-LRB components.
        // When all paths are exhausted the simulation begins simulating LRB components.  Before each comp, the load distribution
        //  engine is called to handle the load distribution for this current pass.  If load is successfully distributed, this is
        //  flagged, and not called again.  If load is not distributed (i.e. this component isn't ON right now), then the
        //  load distribution engine will be called again before the next component.
        // After all load distribution is done and those components are complete, the simulation moves back to do any
        //  remaining components that may be downstream.

        //~ Flags
        bool LoadDistributionWasPerformed;
        bool DummyInit = false;
        bool const DoNotGetCompSizFac(false);

        //~ General variables
        Real64 LoadToLoopSetPoint;
        PlantLocation PumpLocation;
        LoadToLoopSetPoint = 0.0;

        // We now know what plant simulation region is available to us, let's simulate this group
        bool EncounteredLRBObjDuringPass1(false);
        for (int BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter) {
            auto &branch(this->Branch(BranchCounter));

            //~ Always start from the last component we did the last time around + 1 and
            //~  try to make it all the way to the end of the loop
            int const StartingComponent = branch.lastComponentSimulated + 1;
            int const EndingComponent = branch.TotalComponents;
            for (int CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter) {

                auto &this_comp(branch.Comp(CompCounter));
                auto const CurOpSchemeType(this_comp.CurOpSchemeType);

                switch (CurOpSchemeType) {
                case DataPlant::WSEconOpSchemeType: //~ coils
                    this_comp.MyLoad = UpdatedDemandToLoopSetPoint;
                    branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    break;
                case DataPlant::PumpOpSchemeType: //~ pump
                    if (this->BranchPumpsExist) {
                        SimulateSinglePump(this_comp.location, branch.RequestedMassFlow);
                    } else {
                        SimulateSinglePump(this_comp.location, FlowRequest);
                    }
                    break;
                case DataPlant::CompSetPtBasedSchemeType:
                    PlantCondLoopOperation::ManagePlantLoadDistribution(this->myLoopNum,
                                                                        this->myLoopSideNum,
                                                                        BranchCounter,
                                                                        CompCounter,
                                                                        LoadToLoopSetPoint,
                                                                        LoadToLoopSetPointThatWasntMet,
                                                                        FirstHVACIteration,
                                                                        LoopShutDownFlag,
                                                                        LoadDistributionWasPerformed);
                    branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    break;
                case DataPlant::EMSOpSchemeType:
                    if (this->myLoopSideNum == DataPlant::SupplySide) {
                        int const curCompOpSchemePtr = this_comp.CurCompLevelOpNum;
                        int const OpSchemePtr = this_comp.OpScheme(curCompOpSchemePtr).OpSchemePtr;
                        DataPlant::PlantLoop(this->myLoopNum).OpScheme(OpSchemePtr).EMSIntVarLoopDemandRate = InitialDemandToLoopSetPoint;
                    }
                    PlantCondLoopOperation::ManagePlantLoadDistribution(this->myLoopNum,
                                                                        this->myLoopSideNum,
                                                                        BranchCounter,
                                                                        CompCounter,
                                                                        UpdatedDemandToLoopSetPoint,
                                                                        LoadToLoopSetPointThatWasntMet,
                                                                        FirstHVACIteration,
                                                                        LoopShutDownFlag,
                                                                        LoadDistributionWasPerformed);
                    branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    break;
                default:
                    if ((CurOpSchemeType >= DataPlant::LoadRangeBasedMin) && (CurOpSchemeType <= DataPlant::LoadRangeBasedMax)) { //~ load range based
                        EncounteredLRBObjDuringPass1 = true;
                        goto components_end; // don't do any more components on this branch
                    } else {                 // demand, , etc.
                        branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    }
                }

                // Update loop demand as needed for changes this component may have made
                this->UpdateAnyLoopDemandAlterations(BranchCounter, CompCounter);

                //~ If we didn't EXIT early, we must have simulated, so update array
                branch.lastComponentSimulated = CompCounter;

            } //~ CompCounter
        components_end:;

            if (this->FlowLock == DataPlant::FlowLocked) {
                PlantPressureSystem::SimPressureDropSystem(
                    this->myLoopNum, FirstHVACIteration, DataPlant::PressureCall_Calc, this->myLoopSideNum, BranchCounter);
            }

        } //~ BranchCounter

        // So now we have made one pass through all of the available components on these branches, skipping load based
        // If we didn't encounter any load based objects during the first pass, then we must be done!
        if (!EncounteredLRBObjDuringPass1) return;

        // If we have load based now, we should go ahead and distribute the load
        // If not then this branch group is done, since flow path validation was previously done
        LoadToLoopSetPoint = UpdatedDemandToLoopSetPoint;
        LoadDistributionWasPerformed = false;

        // The way the load distribution is set up, I think I should call this for every load range based component
        //  encountered until distribution is actually performed.  If we don't call for each component then we may
        //  call for a component that is not on the current equip list and then nothing would come on.
        bool EncounteredNonLBObjDuringPass2(false);
        for (int BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter) {
            auto &branch(this->Branch(BranchCounter));

            //~ Always start from the last component we did the last time around + 1 and
            //~  try to make it all the way to the end of the loop
            int const StartingComponent = branch.lastComponentSimulated + 1;
            int const EndingComponent = branch.TotalComponents;
            for (int CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter) {

                auto const CurOpSchemeType(branch.Comp(CompCounter).CurOpSchemeType);

                switch (CurOpSchemeType) {
                case DataPlant::NoControlOpSchemeType: //~ pipes, for example
                    branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    break;
                case DataPlant::DemandOpSchemeType:
                case DataPlant::CompSetPtBasedSchemeType:
                case DataPlant::FreeRejectionOpSchemeType: //~ other control types
                    EncounteredNonLBObjDuringPass2 = true;
                    goto components2_end;         // don't do anymore components on this branch
                case DataPlant::PumpOpSchemeType: //~ pump
                    PumpLocation.loopNum = this->myLoopNum;
                    PumpLocation.loopSideNum = this->myLoopSideNum;
                    PumpLocation.branchNum = BranchCounter;
                    PumpLocation.compNum = CompCounter;
                    if (this->BranchPumpsExist) {
                        SimulateSinglePump(PumpLocation, branch.RequestedMassFlow);
                    } else {
                        SimulateSinglePump(PumpLocation, FlowRequest);
                    }
                    break;
                default:
                    if ((CurOpSchemeType >= DataPlant::LoadRangeBasedMin) && (CurOpSchemeType <= DataPlant::LoadRangeBasedMax)) { //~ load range based
                        if (!LoadDistributionWasPerformed) { //~ Still need to distribute load among load range based components
                            PlantCondLoopOperation::ManagePlantLoadDistribution(this->myLoopNum,
                                                                                this->myLoopSideNum,
                                                                                BranchCounter,
                                                                                CompCounter,
                                                                                LoadToLoopSetPoint,
                                                                                LoadToLoopSetPointThatWasntMet,
                                                                                FirstHVACIteration,
                                                                                LoopShutDownFlag,
                                                                                LoadDistributionWasPerformed);
                        }
                        branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    }
                }

                //~ If we didn't EXIT early, we must have simulated, so update array
                branch.lastComponentSimulated = CompCounter;

            } //~ CompCounter
        components2_end:;

            //~ If we are locked, go ahead and simulate the pressure components on this branch
            if (this->FlowLock == DataPlant::FlowLocked) {
                PlantPressureSystem::SimPressureDropSystem(
                    this->myLoopNum, FirstHVACIteration, DataPlant::PressureCall_Calc, this->myLoopSideNum, BranchCounter);
            }

        } //~ BranchCounter

        // So now we have made the load range based pass through all the components on each branch
        // If we didn't see any other component types, then we are done, go away
        if (!EncounteredNonLBObjDuringPass2) return;

        // If we did encounter other objects than we just need to go back through and simulate them
        for (int BranchCounter = FirstBranchNum; BranchCounter <= LastBranchNum; ++BranchCounter) {
            auto &branch(this->Branch(BranchCounter));

            //~ Always start from the last component we did the last time around + 1 and
            //~  try to make it all the way to the end of the loop
            int const StartingComponent = branch.lastComponentSimulated + 1;
            int const EndingComponent = branch.TotalComponents;
            for (int CompCounter = StartingComponent; CompCounter <= EndingComponent; ++CompCounter) {

                auto const CurOpSchemeType(branch.Comp(CompCounter).CurOpSchemeType);

                switch (CurOpSchemeType) {
                case DataPlant::DemandOpSchemeType: //~ coils
                    branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    break;
                case DataPlant::PumpOpSchemeType: //~ pump
                    PumpLocation.loopNum = this->myLoopNum;
                    PumpLocation.loopSideNum = this->myLoopSideNum;
                    PumpLocation.branchNum = BranchCounter;
                    PumpLocation.compNum = CompCounter;
                    if (this->BranchPumpsExist) {
                        SimulateSinglePump(PumpLocation, branch.RequestedMassFlow);
                    } else {
                        SimulateSinglePump(PumpLocation, FlowRequest);
                    }
                    break;
                default:
                    if ((CurOpSchemeType >= DataPlant::LoadRangeBasedMin) && (CurOpSchemeType <= DataPlant::LoadRangeBasedMax)) { //~ load range based
                        ShowFatalError("Encountered Load Based Object after other components, invalid.");
                    } else { //~ Typical control equipment
                        branch.Comp(CompCounter).simulate(FirstHVACIteration, DummyInit, DoNotGetCompSizFac);
                    }
                }

                //~ If we didn't EXIT early, we must have simulated, so update array
                branch.lastComponentSimulated = CompCounter;

            } //~ CompCounter

            if (this->FlowLock == DataPlant::FlowLocked) {
                PlantPressureSystem::SimPressureDropSystem(
                    this->myLoopNum, FirstHVACIteration, DataPlant::PressureCall_Calc, this->myLoopSideNum, BranchCounter);
            }

        } //~ BranchCounter

        // I suppose I could do a check on the last component simulated to make sure we actually exhausted all branches
        // This would be the "THIRD" check on flow validation, but would be OK
    }

    void HalfLoopData::UpdateAnyLoopDemandAlterations(int const BranchNum, int const CompNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   August 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will analyze the given component and determine if any
        //  alterations need to be made to the current loop demand value.  If so,
        //  it will make the changes to the module level loop demand variables.

        // METHODOLOGY EMPLOYED:
        // Components will always supply a useful delta T, even if it happens to be zero
        // For flow rate, make decisions based on the component's current operating scheme type:
        //    Demand based: these components will have a flow request on their inlet node
        //    Pump: these components will not be included, as they no longer include heat at the pump
        //    component setpoint: these components will have a flow request

        //    on their outlet node corresponding to their calculated delta T
        //    load range based: these components do not 'alter' the load, they reject the load
        //    Therefore they are not included

        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataLoopNode::Node;
        using DataPlant::FlowLocked;
        using DataPlant::FlowUnlocked;
        using DataPlant::LoadRangeBasedMax;
        using DataPlant::LoadRangeBasedMin;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("PlantLoopSolver::UpdateAnyLoopDemandAlterations");

        // Init to zero, so that if we don't find anything, we exit early
        Real64 ComponentMassFlowRate(0.0);

        auto const &this_comp(this->Branch(BranchNum).Comp(CompNum));

        // Get information
        int const InletNode(this_comp.NodeNumIn);
        int const OutletNode(this_comp.NodeNumOut);

        if (this->FlowLock == FlowUnlocked) {

            // For unlocked flow, use the inlet request -- !DSU? for now
            {
                auto const SELECT_CASE_var(this_comp.CurOpSchemeType);
                if ((SELECT_CASE_var >= LoadRangeBasedMin) && (SELECT_CASE_var <= LoadRangeBasedMax)) {
                    // Don't do anything for load based components
                } else {
                    // pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
                    ComponentMassFlowRate = Node(InletNode).MassFlowRateRequest;
                    // DSU? make sure components like economizers use the mass flow request
                }
            }

        } else if (this->FlowLock == FlowLocked) {

            // For locked flow just use the mass flow rate
            {
                auto const SELECT_CASE_var(this_comp.CurOpSchemeType);
                if ((SELECT_CASE_var >= LoadRangeBasedMin) && (SELECT_CASE_var <= LoadRangeBasedMax)) {
                    // Don't do anything for load based components
                } else {
                    // pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
                    ComponentMassFlowRate = Node(OutletNode).MassFlowRate;
                }
            }

        } else { // flow pump query? problem?
        }

        // Leave early if there wasn't a mass flow rate or request
        if (ComponentMassFlowRate < MassFlowTolerance) return;

        // Get an average temperature for the property call
        Real64 const InletTemp(Node(InletNode).Temp);
        Real64 const OutletTemp(Node(OutletNode).Temp);
        Real64 const AverageTemp((InletTemp + OutletTemp) / 2.0);
        Real64 const ComponentCp(
            GetSpecificHeatGlycol(PlantLoop(this->myLoopNum).FluidName, AverageTemp, PlantLoop(this->myLoopNum).FluidIndex, RoutineName));

        // Calculate the load altered by this component
        Real64 const LoadAlteration(ComponentMassFlowRate * ComponentCp * (OutletTemp - InletTemp));

        // Now alter the module level variables
        this->CurrentAlterationsToDemand += LoadAlteration;
        this->UpdatedDemandToLoopSetPoint = this->InitialDemandToLoopSetPoint - this->CurrentAlterationsToDemand;
    }

    void HalfLoopData::SimulateSinglePump(PlantLocation const SpecificPumpLocation, Real64 &SpecificPumpFlowRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        auto &loop(DataPlant::PlantLoop(SpecificPumpLocation.loopNum));
        auto &loop_side(loop.LoopSide(SpecificPumpLocation.loopSideNum));
        auto &loop_side_branch(loop_side.Branch(SpecificPumpLocation.branchNum));
        auto &comp(loop_side_branch.Comp(SpecificPumpLocation.compNum));
        int const PumpIndex = comp.IndexInLoopSidePumps;
        auto &pump(loop_side.Pumps(PumpIndex));

        DataPlant::PlantLoop(SpecificPumpLocation.loopNum)
            .loopSolver.AdjustPumpFlowRequestByEMSControls(SpecificPumpLocation.loopNum,
                                                           SpecificPumpLocation.loopSideNum,
                                                           SpecificPumpLocation.branchNum,
                                                           SpecificPumpLocation.compNum,
                                                           SpecificPumpFlowRate);

        // Call SimPumps, routine takes a flow request, and returns some info about the status of the pump
        bool DummyThisPumpRunning;
        Pumps::SimPumps(pump.PumpName,
                        SpecificPumpLocation.loopNum,
                        SpecificPumpFlowRate,
                        DummyThisPumpRunning,
                        loop_side_branch.PumpIndex,
                        pump.PumpHeatToFluid);

        //~ Pull some state information from the pump outlet node
        pump.CurrentMinAvail = DataLoopNode::Node(pump.PumpOutletNode).MassFlowRateMinAvail;
        pump.CurrentMaxAvail = DataLoopNode::Node(pump.PumpOutletNode).MassFlowRateMaxAvail;

        //~ Update the LoopSide pump heat totality here
        if (loop_side.TotalPumps > 0) {
            loop_side.TotalPumpHeat = sum(loop_side.Pumps, &DataPlant::LoopSidePumpInformation::PumpHeatToFluid);
        }
    }

    void HalfLoopData::SimulateAllLoopSidePumps(Optional<PlantLocation const> SpecificPumpLocation,
                                                        Optional<Real64 const> SpecificPumpFlowRate) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int PumpIndexStart;
        int PumpIndexEnd;
        int PumpLoopNum;
        int PumpLoopSideNum;

        // If we have a specific loop/side/br/comp, then find the index and only do that one, otherwise do all pumps on the loop side
        if (present(SpecificPumpLocation)) {
            PumpLoopNum = SpecificPumpLocation().loopNum;
            PumpLoopSideNum = SpecificPumpLocation().loopSideNum;
            int const PumpBranchNum = SpecificPumpLocation().branchNum;
            int const PumpCompNum = SpecificPumpLocation().compNum;
            PumpIndexStart = DataPlant::PlantLoop(PumpLoopNum).LoopSide(PumpLoopSideNum).Branch(PumpBranchNum).Comp(
                PumpCompNum).IndexInLoopSidePumps;
            PumpIndexEnd = PumpIndexStart;
        } else {
            PumpLoopNum = this->myLoopNum;
            PumpLoopSideNum = this->myLoopSideNum;
            PumpIndexStart = 1;
            PumpIndexEnd = this->TotalPumps;
        }

        // If we have a flow rate to hit, then go for it, otherwise, just operate in request mode with zero flow
        Real64 FlowToRequest;
        if (present(SpecificPumpFlowRate)) {
            FlowToRequest = SpecificPumpFlowRate;
        } else {
            FlowToRequest = 0.0;
        }

        //~ Now loop through all the pumps and simulate them, keeping track of their status
        auto &loop_side(DataPlant::PlantLoop(PumpLoopNum).LoopSide(PumpLoopSideNum));
        auto &loop_side_branch(loop_side.Branch);
        for (int PumpCounter = PumpIndexStart; PumpCounter <= PumpIndexEnd; ++PumpCounter) {

            //~ Set some variables
            auto &pump(loop_side.Pumps(PumpCounter));
            int const PumpBranchNum = pump.BranchNum;
            int const PumpCompNum = pump.CompNum;
            int const PumpOutletNode = pump.PumpOutletNode;

            DataPlant::PlantLoop(this->myLoopNum).loopSolver.AdjustPumpFlowRequestByEMSControls(
                PumpLoopNum, PumpLoopSideNum, PumpBranchNum, PumpCompNum, FlowToRequest);

            // Call SimPumps, routine takes a flow request, and returns some info about the status of the pump
            bool DummyThisPumpRunning;
            Pumps::SimPumps(pump.PumpName, PumpLoopNum, FlowToRequest, DummyThisPumpRunning,
                            loop_side_branch(PumpBranchNum).PumpIndex, pump.PumpHeatToFluid);

            //~ Pull some state information from the pump outlet node
            Real64 const ThisPumpMinAvail = DataLoopNode::Node(PumpOutletNode).MassFlowRateMinAvail;
            Real64 const ThisPumpMaxAvail = DataLoopNode::Node(PumpOutletNode).MassFlowRateMaxAvail;

            //~ Now update the data structure
            pump.CurrentMinAvail = ThisPumpMinAvail;
            pump.CurrentMaxAvail = ThisPumpMaxAvail;
        }

        //~ Update the LoopSide pump heat totality here
        if (loop_side.TotalPumps > 0) {
            loop_side.TotalPumpHeat = sum(loop_side.Pumps, &DataPlant::LoopSidePumpInformation::PumpHeatToFluid);
        }
    }


Real64 HalfLoopData::DetermineLoopSideFlowRate(int ThisSideInletNode, Real64 ThisSideLoopFlowRequest)
    {
        Real64 ThisLoopSideFlow = ThisSideLoopFlowRequest;
        Real64 TotalPumpMinAvailFlow = 0.0;
        Real64 TotalPumpMaxAvailFlow = 0.0;
        if (allocated(this->Pumps)) {

            //~ Initialize pump values
            for (auto &e : this->Pumps) {
                e.CurrentMinAvail = 0.0;
                e.CurrentMaxAvail = 0.0;
            }
            this->FlowLock = DataPlant::FlowPumpQuery;

            //~ Simulate pumps
            this->SimulateAllLoopSidePumps();

            //~ Calculate totals
            for (auto const &e : this->Pumps) {
                TotalPumpMinAvailFlow += e.CurrentMinAvail;
                TotalPumpMaxAvailFlow += e.CurrentMaxAvail;
            }

            // Use the pump min/max avail to attempt to constrain the loop side flow
            ThisLoopSideFlow = PlantUtilities::BoundValueToWithinTwoValues(ThisLoopSideFlow, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow);
        }

        // Now we check flow restriction from the other side, both min and max avail.
        // Doing this last basically means it wins, so the pump should pull down to meet the flow restriction
        ThisLoopSideFlow = PlantUtilities::BoundValueToNodeMinMaxAvail(ThisLoopSideFlow, ThisSideInletNode);

        // Final preparation of loop inlet min/max avail if pumps exist
        if (allocated(this->Pumps)) {
            // At this point, the pump limits should have been obeyed unless a flow restriction was encountered from the other side
            // The pump may, however, have even tighter constraints than the other side
            // At this point, the inlet node doesn't know anything about those limits
            // Since we have already honored the other side flow restriction, try to honor the pump limits here
            PlantUtilities::TightenNodeMinMaxAvails(ThisSideInletNode, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow);
        }

        // Now reset the entering mass flow rate to the decided-upon flow rate
        DataLoopNode::Node(ThisSideInletNode).MassFlowRate = ThisLoopSideFlow;
        return ThisLoopSideFlow;
    }

    void HalfLoopData::UpdatePlantMixer()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brandon Anderson, Dan Fisher
        //       DATE WRITTEN   October 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // calculate the outlet conditions at the mixer
        // this is expected to only be called for loops with a mixer

        // Find mixer outlet node number
        int const MixerOutletNode = this->Mixer.NodeNumOut;

        // Find corresponding splitter inlet node number--correspondence, but currently
        //  hard code things to a single split/mix setting it to the mixer number
        int const SplitterInNode = this->Splitter.NodeNumIn;
        // Initialize Mixer outlet temp and mass flow rate
        Real64 MixerOutletTemp = 0.0;
        Real64 MixerOutletMassFlow = 0.0;
        Real64 MixerOutletMassFlowMaxAvail = 0.0;
        Real64 MixerOutletMassFlowMinAvail = 0.0;
        Real64 MixerOutletPress = 0.0;
        Real64 MixerOutletQuality = 0.0;

        // Calculate Mixer outlet mass flow rate
        for (int InletNodeNum = 1; InletNodeNum <= this->Mixer.TotalInletNodes; ++InletNodeNum) {
            int const MixerInletNode = this->Mixer.NodeNumIn(InletNodeNum);
            MixerOutletMassFlow += DataLoopNode::Node(MixerInletNode).MassFlowRate;
        }

        // Calculate Mixer outlet temperature
        for (int InletNodeNum = 1; InletNodeNum <= this->Mixer.TotalInletNodes; ++InletNodeNum) {
            int const MixerInletNode = this->Mixer.NodeNumIn(InletNodeNum);
            if (MixerOutletMassFlow > 0.0) {
                Real64 const MixerInletMassFlow = DataLoopNode::Node(MixerInletNode).MassFlowRate;
                Real64 const MassFrac = MixerInletMassFlow / MixerOutletMassFlow;
                // mass flow weighted temp and enthalpy for each mixer inlet
                MixerOutletTemp += MassFrac * DataLoopNode::Node(MixerInletNode).Temp;
                MixerOutletQuality += MassFrac * DataLoopNode::Node(MixerInletNode).Quality;
                MixerOutletMassFlowMaxAvail += DataLoopNode::Node(MixerInletNode).MassFlowRateMaxAvail;
                MixerOutletMassFlowMinAvail += DataLoopNode::Node(MixerInletNode).MassFlowRateMinAvail;
                MixerOutletPress = max(MixerOutletPress, DataLoopNode::Node(MixerInletNode).Press);
            } else { // MixerOutletMassFlow <=0, then perform the 'no flow' update.
                MixerOutletTemp = DataLoopNode::Node(SplitterInNode).Temp;
                MixerOutletQuality = DataLoopNode::Node(SplitterInNode).Quality;
                MixerOutletMassFlowMaxAvail = DataLoopNode::Node(SplitterInNode).MassFlowRateMaxAvail;
                MixerOutletMassFlowMinAvail = DataLoopNode::Node(SplitterInNode).MassFlowRateMinAvail;
                MixerOutletPress = DataLoopNode::Node(SplitterInNode).Press;
                break;
            }
        }

        DataLoopNode::Node(MixerOutletNode).MassFlowRate = MixerOutletMassFlow;
        DataLoopNode::Node(MixerOutletNode).Temp = MixerOutletTemp;
        if (DataPlant::PlantLoop(this->myLoopNum).HasPressureComponents) {
            // Don't update pressure, let pressure system handle this...
        } else {
            // Go ahead and update!
            DataLoopNode::Node(MixerOutletNode).Press = MixerOutletPress;
        }
        DataLoopNode::Node(MixerOutletNode).Quality = MixerOutletQuality;

        // set max/min avails on mixer outlet to be consistent with the following rules
        // 1.  limited by the max/min avails on splitter inlet
        // 2.  limited by the sum of max/min avails for each branch's mixer inlet node

        DataLoopNode::Node(MixerOutletNode).MassFlowRateMaxAvail =
            min(MixerOutletMassFlowMaxAvail, DataLoopNode::Node(SplitterInNode).MassFlowRateMaxAvail);
        DataLoopNode::Node(MixerOutletNode).MassFlowRateMinAvail =
            max(MixerOutletMassFlowMinAvail, DataLoopNode::Node(SplitterInNode).MassFlowRateMinAvail);
    }

    void HalfLoopData::UpdatePlantSplitter()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brandon Anderson, Dan Fisher
        //       DATE WRITTEN   October 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set the outlet conditions of the splitter

        // Update Temperatures across splitter
        if (this->SplitterExists) {

            // Set branch number at splitter inlet
            int const SplitterInletNode = this->Splitter.NodeNumIn;

            // Loop over outlet nodes
            for (int CurNode = 1; CurNode <= this->Splitter.TotalOutletNodes; ++CurNode) {
                int const SplitterOutletNode = this->Splitter.NodeNumOut(CurNode);

                // Inlet Temp equals exit Temp to all outlet branches
                DataLoopNode::Node(SplitterOutletNode).Temp = DataLoopNode::Node(SplitterInletNode).Temp;
                DataLoopNode::Node(SplitterOutletNode).TempMin = DataLoopNode::Node(SplitterInletNode).TempMin;
                DataLoopNode::Node(SplitterOutletNode).TempMax = DataLoopNode::Node(SplitterInletNode).TempMax;
                if (DataPlant::PlantLoop(this->myLoopNum).HasPressureComponents) {
                    // Don't update pressure, let pressure system handle this...
                } else {
                    // Go ahead and update!
                    DataLoopNode::Node(SplitterOutletNode).Press = DataLoopNode::Node(SplitterInletNode).Press;
                }
                DataLoopNode::Node(SplitterOutletNode).Quality = DataLoopNode::Node(SplitterInletNode).Quality;

                // DSU? These two blocks and the following one which I added need to be cleaned up
                // I think we will always pass maxavail down the splitter, min avail is the issue.
                // Changed to include hardware max in next line 7/26/2011
                DataLoopNode::Node(SplitterOutletNode).MassFlowRateMaxAvail =
                    min(DataLoopNode::Node(SplitterInletNode).MassFlowRateMaxAvail, DataLoopNode::Node(SplitterOutletNode).MassFlowRateMax);
                DataLoopNode::Node(SplitterOutletNode).MassFlowRateMinAvail = 0.0;

                // DSU? Not sure about passing min avail if it is nonzero.  I am testing a pump with nonzero
                // min flow rate, and it is causing problems because this routine passes zero down.  Perhaps if
                // it is a single parallel branch, we are safe to assume we need to just pass it down.
                // But need to test for multiple branches (or at least think about it), to see what we need to do...
                if (this->Splitter.TotalOutletNodes == 1) {
                    DataLoopNode::Node(SplitterOutletNode).MassFlowRateMinAvail = DataLoopNode::Node(SplitterInletNode).MassFlowRateMinAvail;
                }
            }
        }
    }

}
}
