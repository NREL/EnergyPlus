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
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::PlantUtilities {

// Module containing the routines dealing with the <module_name>

// MODULE INFORMATION:
//       AUTHOR         <author>
//       DATE WRITTEN   <date_written>
//       MODIFIED       na
//       RE-ENGINEERED  na

void InitComponentNodes(EnergyPlusData &state,
                        Real64 const MinCompMdot,
                        Real64 const MaxCompMdot,
                        int const InletNode,                    // component's inlet node index in node structure
                        int const OutletNode,                   // component's outlet node index in node structure
                        [[maybe_unused]] int const LoopNum,     // plant loop index for PlantLoop structure
                        [[maybe_unused]] int const LoopSideNum, // Loop side index for PlantLoop structure
                        [[maybe_unused]] int const BranchIndex, // branch index for PlantLoop
                        [[maybe_unused]] int const CompIndex    // component index for PlantLoop
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  Central routine for initializing plant nodes connected to components
    //  typically used for BeginEnvrnFlag

    // METHODOLOGY EMPLOYED:
    // set MassFlowRate variables on inlet node
    //  reset inlet node if more restrictive

    // Using/Aliasing
    using DataPlant::DemandOpSchemeType;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 tmpMinCompMdot; // local value
    Real64 tmpMaxCompMdot; // local value

    tmpMinCompMdot = MinCompMdot;
    tmpMaxCompMdot = MaxCompMdot;
    // trap bad values that can happen before all the setup is done
    if (tmpMinCompMdot < 0.0) tmpMinCompMdot = 0.0;
    if (tmpMaxCompMdot < 0.0) tmpMaxCompMdot = 0.0;

    // reset outlet node
    state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;

    state.dataLoopNodes->Node(InletNode).MassFlowRateMin = tmpMinCompMdot;
    state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = tmpMinCompMdot;
    state.dataLoopNodes->Node(InletNode).MassFlowRateMax = tmpMaxCompMdot;
    state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = tmpMaxCompMdot;
    // reset inlet node, but only change from inlet setting if set and more restrictive
    state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
    state.dataLoopNodes->Node(InletNode).MassFlowRateRequest = 0.0;
}

void SetComponentFlowRate(EnergyPlusData &state,
                          Real64 &CompFlow,      // [kg/s]
                          int const InletNode,   // component's inlet node index in node structure
                          int const OutletNode,  // component's outlet node index in node structure
                          int const LoopNum,     // plant loop index for PlantLoop structure
                          int const LoopSideNum, // Loop side index for PlantLoop structure
                          int const BranchIndex, // branch index for PlantLoop
                          int const CompIndex    // component index for PlantLoop
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   August 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // General purpose worker routine to set flows for a component model

    if (LoopNum == 0) { // protect from hard crash below
        if (InletNode > 0) {
            ShowSevereError(state,
                            "SetComponentFlowRate: trapped plant loop index = 0, check component with inlet node named=" +
                                state.dataLoopNodes->NodeID(InletNode));
        } else {
            ShowSevereError(state, "SetComponentFlowRate: trapped plant loop node id = 0");
        }
        return;
        // this crashes during ManageSizing, maybe it's just an init thing...
        // ShowFatalError(state, "Preceding loop index error causes program termination");
    }

    Real64 const MdotOldRequest = state.dataLoopNodes->Node(InletNode).MassFlowRateRequest;
    auto &loop_side(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum));
    auto &comp(loop_side.Branch(BranchIndex).Comp(CompIndex));

    if (comp.CurOpSchemeType == DataPlant::DemandOpSchemeType) {
        // store flow request on inlet node
        state.dataLoopNodes->Node(InletNode).MassFlowRateRequest = CompFlow;
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail =
            max(state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail, state.dataLoopNodes->Node(InletNode).MassFlowRateMin);
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail =
            min(state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail, state.dataLoopNodes->Node(InletNode).MassFlowRateMax);
        // virtual 2-way valve (was tried but it clamps down demand side component's flow options so they can't find proper solutions)
    } else {
        // lodge the original request for all types
        state.dataLoopNodes->Node(InletNode).MassFlowRateRequest = CompFlow;
    }

    // Update Min/Max Avail

    state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail =
        max(state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail, state.dataLoopNodes->Node(InletNode).MassFlowRateMin);
    if (state.dataLoopNodes->Node(InletNode).MassFlowRateMax >= 0.0) {
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail =
            min(state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail, state.dataLoopNodes->Node(InletNode).MassFlowRateMax);
    } else {
        if (!state.dataGlobal->SysSizingCalc && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            // throw error for developers, need to change a component model to set hardware limits on inlet
            if (!state.dataLoopNodes->Node(InletNode).plantNodeErrorMsgIssued) {
                ShowSevereError(state,
                                "SetComponentFlowRate: check component model implementation for component with inlet node named=" +
                                    state.dataLoopNodes->NodeID(InletNode));
                ShowContinueError(state, format("Inlet node MassFlowRatMax = {:.8R}", state.dataLoopNodes->Node(InletNode).MassFlowRateMax));
                state.dataLoopNodes->Node(InletNode).plantNodeErrorMsgIssued = true;
            }
        }
    }

    // Set loop flow rate
    if (loop_side.FlowLock == DataPlant::iFlowLock::Unlocked) {
        if (state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate == DataSizing::AutoSize) { // still haven't sized the plant loop
            state.dataLoopNodes->Node(OutletNode).MassFlowRate = CompFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
        } else { // bound the flow by Min/Max available and hardware limits
            if (comp.FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                // determine highest flow request for all the components on the branch
                Real64 SeriesBranchHighFlowRequest = 0.0;
                Real64 SeriesBranchHardwareMaxLim = state.dataLoopNodes->Node(InletNode).MassFlowRateMax;
                Real64 SeriesBranchHardwareMinLim = 0.0;
                Real64 SeriesBranchMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
                Real64 SeriesBranchMinAvail = 0.0;

                // inserting EMS On/Off Supervisory control here to series branch constraint and assuming EMS should shut off flow completely
                // action here means EMS will not impact the FlowLock == FlowLocked condition (which should still show EMS intent)
                bool EMSLoadOverride = false;

                for (int CompNum = 1; CompNum <= loop_side.Branch(BranchIndex).TotalComponents; ++CompNum) {
                    auto &thisComp(loop_side.Branch(BranchIndex).Comp(CompNum));
                    int const CompInletNodeNum = thisComp.NodeNumIn;
                    auto &thisInletNode(state.dataLoopNodes->Node(CompInletNodeNum));
                    SeriesBranchHighFlowRequest = max(thisInletNode.MassFlowRateRequest, SeriesBranchHighFlowRequest);
                    SeriesBranchHardwareMaxLim = min(thisInletNode.MassFlowRateMax, SeriesBranchHardwareMaxLim);
                    SeriesBranchHardwareMinLim = max(thisInletNode.MassFlowRateMin, SeriesBranchHardwareMinLim);
                    SeriesBranchMaxAvail = min(thisInletNode.MassFlowRateMaxAvail, SeriesBranchMaxAvail);
                    SeriesBranchMinAvail = max(thisInletNode.MassFlowRateMinAvail, SeriesBranchMinAvail);
                    // check to see if any component on branch uses EMS On/Off Supervisory control to shut down flow
                    if (thisComp.EMSLoadOverrideOn && thisComp.EMSLoadOverrideValue == 0.0) EMSLoadOverride = true;
                }

                if (EMSLoadOverride) { // actuate EMS controlled components to 0 if On/Off Supervisory control is active off
                    SeriesBranchHardwareMaxLim = 0.0;
                }

                // take higher of branch max flow request and this new flow request
                CompFlow = max(CompFlow, SeriesBranchHighFlowRequest);

                // apply constraints on component flow
                CompFlow = max(CompFlow, SeriesBranchHardwareMinLim);
                CompFlow = max(CompFlow, SeriesBranchMinAvail);
                CompFlow = min(CompFlow, SeriesBranchHardwareMaxLim);
                CompFlow = min(CompFlow, SeriesBranchMaxAvail);

                if (CompFlow < DataBranchAirLoopPlant::MassFlowTolerance) CompFlow = 0.0;
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = CompFlow;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                for (int CompNum = 1; CompNum <= loop_side.Branch(BranchIndex).TotalComponents; ++CompNum) {
                    auto &thisComp(loop_side.Branch(BranchIndex).Comp(CompNum));
                    int const CompInletNodeNum = thisComp.NodeNumIn;
                    int const CompOutletNodeNum = thisComp.NodeNumOut;
                    state.dataLoopNodes->Node(CompInletNodeNum).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                    state.dataLoopNodes->Node(CompOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                }

            } else { // not series active
                state.dataLoopNodes->Node(OutletNode).MassFlowRate = max(state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail, CompFlow);
                state.dataLoopNodes->Node(OutletNode).MassFlowRate =
                    max(state.dataLoopNodes->Node(InletNode).MassFlowRateMin, state.dataLoopNodes->Node(OutletNode).MassFlowRate);
                state.dataLoopNodes->Node(OutletNode).MassFlowRate =
                    min(state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail, state.dataLoopNodes->Node(OutletNode).MassFlowRate);
                state.dataLoopNodes->Node(OutletNode).MassFlowRate =
                    min(state.dataLoopNodes->Node(InletNode).MassFlowRateMax, state.dataLoopNodes->Node(OutletNode).MassFlowRate);

                // inserting EMS On/Off Supervisory control here to override min constraint assuming EMS should shut off flow completely
                // action here means EMS will not impact the FlowLock == FlowLocked condition (which should still show EMS intent)
                bool EMSLoadOverride = false;

                for (int CompNum = 1; CompNum <= loop_side.Branch(BranchIndex).TotalComponents; ++CompNum) {
                    // check to see if any component on branch uses EMS On/Off Supervisory control to shut down flow
                    auto &thisComp(loop_side.Branch(BranchIndex).Comp(CompNum));
                    if (thisComp.EMSLoadOverrideOn && thisComp.EMSLoadOverrideValue == 0.0) EMSLoadOverride = true;
                }

                if (EMSLoadOverride) { // actuate EMS controlled components to 0 if On/Off Supervisory control is active off
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
                }

                if (state.dataLoopNodes->Node(OutletNode).MassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance)
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
                CompFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
            }
        }
    } else if (loop_side.FlowLock == DataPlant::iFlowLock::Locked) {
        state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        CompFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
    } else {
        ShowFatalError(state, "SetComponentFlowRate: Flow lock out of range"); // DEBUG error...should never get here LCOV_EXCL_LINE
    }

    if (comp.CurOpSchemeType == DataPlant::DemandOpSchemeType) {
        if ((MdotOldRequest > 0.0) && (CompFlow > 0.0)) { // sure that not coming back from a no flow reset
            if (std::abs(MdotOldRequest - state.dataLoopNodes->Node(InletNode).MassFlowRateRequest) >
                DataBranchAirLoopPlant::MassFlowTolerance) { // demand comp changed its flow request
                loop_side.SimLoopSideNeeded = true;
            }
        }
    }
}

void SetActuatedBranchFlowRate(EnergyPlusData &state,
                               Real64 &CompFlow,
                               int const ActuatedNode,
                               int const LoopNum,
                               int const LoopSideNum,
                               int const BranchNum,
                               bool const ResetMode // flag to indicate if this is a real flow set, or a reset flow setting.
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Feb 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // general purpse worker routine to set plant node variables for node
    // and all nodes on the branch.  Used by HVAC water coil controller, that do not
    //  distinguish single component and have no inlet-outlet pair
    //  only a actuated noded of no clear position.  set flow on entire branch

    // METHODOLOGY EMPLOYED:
    // Set flow on node and branch while honoring constraints on actuated node

    auto &a_node(state.dataLoopNodes->Node(ActuatedNode));
    if (LoopNum == 0 || LoopSideNum == 0) {
        // early in simulation before plant loops are setup and found
        a_node.MassFlowRate = CompFlow;
        return;
    }

    auto &loop_side(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum));

    // store original flow
    Real64 const MdotOldRequest = a_node.MassFlowRateRequest;
    a_node.MassFlowRateRequest = CompFlow;
    if (LoopNum > 0 && LoopSideNum > 0 && (!ResetMode)) {
        if ((MdotOldRequest > 0.0) && (CompFlow > 0.0)) { // sure that not coming back from a no flow reset
            if ((std::abs(MdotOldRequest - a_node.MassFlowRateRequest) > DataBranchAirLoopPlant::MassFlowTolerance) &&
                (loop_side.FlowLock == DataPlant::iFlowLock::Unlocked)) {
                loop_side.SimLoopSideNeeded = true;
            }
        }
    }
    // Set loop flow rate

    if (LoopNum > 0 && LoopSideNum > 0) {
        auto const &branch(loop_side.Branch(BranchNum));
        if (loop_side.FlowLock == DataPlant::iFlowLock::Unlocked) {
            if (state.dataPlnt->PlantLoop(LoopNum).MaxVolFlowRate == DataSizing::AutoSize) { // still haven't sized the plant loop
                a_node.MassFlowRate = CompFlow;
            } else { // bound the flow by Min/Max available across entire branch

                a_node.MassFlowRate = max(a_node.MassFlowRateMinAvail, CompFlow);
                a_node.MassFlowRate = max(a_node.MassFlowRateMin, a_node.MassFlowRate);
                // add MassFlowRateMin hardware constraints

                // inserting EMS On/Off Supervisory control here to override min constraint assuming EMS should shut off flow completely
                // action here means EMS will not impact the FlowLock == FlowLocked condition (which should still show EMS intent)
                bool EMSLoadOverride = false;
                // check to see if any component on branch uses EMS On/Off Supervisory control to shut down flow
                for (int CompNum = 1, CompNum_end = branch.TotalComponents; CompNum <= CompNum_end; ++CompNum) {
                    auto const &comp(branch.Comp(CompNum));
                    if (comp.EMSLoadOverrideOn && comp.EMSLoadOverrideValue == 0.0) EMSLoadOverride = true;
                }
                if (EMSLoadOverride) { // actuate EMS controlled components to 0 if On/Off Supervisory control is active off
                    a_node.MassFlowRate = 0.0;
                    a_node.MassFlowRateRequest = 0.0;
                }

                a_node.MassFlowRate = min(a_node.MassFlowRateMaxAvail, a_node.MassFlowRate);
                a_node.MassFlowRate = min(a_node.MassFlowRateMax, a_node.MassFlowRate);
                if (a_node.MassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) a_node.MassFlowRate = 0.0;
                for (int CompNum = 1, CompNum_end = branch.TotalComponents; CompNum <= CompNum_end; ++CompNum) {
                    auto const &comp(branch.Comp(CompNum));
                    if (ActuatedNode == comp.NodeNumIn) {
                        //            ! found controller set to inlet of a component.  now set that component's outlet
                        int const NodeNum = comp.NodeNumOut;
                        state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail = max(a_node.MassFlowRateMinAvail, a_node.MassFlowRateMin);
                        state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail = min(a_node.MassFlowRateMaxAvail, a_node.MassFlowRateMax);
                        state.dataLoopNodes->Node(NodeNum).MassFlowRate = a_node.MassFlowRate;
                    }
                }
            }

        } else if (loop_side.FlowLock == DataPlant::iFlowLock::Locked) {

            CompFlow = a_node.MassFlowRate;
            // do not change requested flow rate either
            a_node.MassFlowRateRequest = MdotOldRequest;
            if ((CompFlow - a_node.MassFlowRateMaxAvail > DataBranchAirLoopPlant::MassFlowTolerance) ||
                (a_node.MassFlowRateMinAvail - CompFlow > DataBranchAirLoopPlant::MassFlowTolerance)) {
                ShowSevereError(state, "SetActuatedBranchFlowRate: Flow rate is out of range"); // DEBUG error...should never get here
                ShowContinueErrorTimeStamp(state, "");
                ShowContinueError(state, format("Component flow rate [kg/s] = {:.8R}", CompFlow));
                ShowContinueError(state, format("Node maximum flow rate available [kg/s] = {:.8R}", a_node.MassFlowRateMaxAvail));
                ShowContinueError(state, format("Node minimum flow rate available [kg/s] = {:.8R}", a_node.MassFlowRateMinAvail));
            }
        } else {
            ShowFatalError(state,
                           format("SetActuatedBranchFlowRate: Flowlock out of range, value={}",
                                  loop_side.FlowLock)); // DEBUG error...should never get here LCOV_EXCL_LINE
        }

        Real64 const a_node_MasFlowRate(a_node.MassFlowRate);
        Real64 const a_node_MasFlowRateRequest(a_node.MassFlowRateRequest);
        for (int CompNum = 1, CompNum_end = branch.TotalComponents; CompNum <= CompNum_end; ++CompNum) {
            auto const &comp(branch.Comp(CompNum));
            int NodeNum = comp.NodeNumIn;
            state.dataLoopNodes->Node(NodeNum).MassFlowRate = a_node_MasFlowRate;
            state.dataLoopNodes->Node(NodeNum).MassFlowRateRequest = a_node_MasFlowRateRequest;
            NodeNum = comp.NodeNumOut;
            state.dataLoopNodes->Node(NodeNum).MassFlowRate = a_node_MasFlowRate;
            state.dataLoopNodes->Node(NodeNum).MassFlowRateRequest = a_node_MasFlowRateRequest;
        }
    }
}

Real64 RegulateCondenserCompFlowReqOp(
    EnergyPlusData &state, int const LoopNum, int const LoopSideNum, int const BranchNum, int const CompNum, Real64 const TentativeFlowRequest)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   April 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function will do some intelligent flow request logic for condenser equipment.
    // Some condenser equipment (ground heat exchangers, etc.) may not have a meaningful load value
    //  since this is an environment heat transfer component.
    // The runflag is set, but may not be properly set, and the component may still request flow even
    //  when it doesn't need to.
    // This function will do a little more advanced logic than just checking runflag to determine whether
    //  to request any flow

    // METHODOLOGY EMPLOYED:
    // Query run flag and MyLoad
    // If run flag is OFF, then the component should actually be OFF, and tentative flow request will be zeroed
    // If the run flag is ON, then check the control type to determine if MyLoad is a meaningful value
    // If it is meaningful then determine whether to do flow request based on MyLoad
    // If not then we will have no choice but to leave the flow request alone (uncontrolled operation?)

    // Using/Aliasing
    using DataPlant::CompSetPtBasedSchemeType;
    using DataPlant::CoolingRBOpSchemeType;
    using DataPlant::HeatingRBOpSchemeType;

    // Return value
    Real64 FlowVal;

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const ZeroLoad(0.0001);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 CompCurLoad;
    bool CompRunFlag;
    int CompOpScheme;

    CompCurLoad = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).MyLoad;
    CompRunFlag = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).ON;
    CompOpScheme = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType;

    if (CompRunFlag) {

        {
            auto const SELECT_CASE_var(CompOpScheme);

            if ((SELECT_CASE_var == HeatingRBOpSchemeType) || (SELECT_CASE_var == CoolingRBOpSchemeType) ||
                (SELECT_CASE_var == CompSetPtBasedSchemeType)) { // These provide meaningful MyLoad values
                if (std::abs(CompCurLoad) > ZeroLoad) {
                    FlowVal = TentativeFlowRequest;
                } else { // no load
                    FlowVal = 0.0;
                }

            } else { // Types that don't provide meaningful MyLoad values
                FlowVal = TentativeFlowRequest;
            }
        }

    } else { // runflag OFF

        FlowVal = 0.0;
    }

    return FlowVal;
}

bool AnyPlantSplitterMixerLacksContinuity(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Similar to CheckPlantMixerSplitterConsistency, but used to decide if plant needs to iterate again
    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (int LoopSide = DataPlant::DemandSide; LoopSide <= DataPlant::SupplySide; ++LoopSide) {
            if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Splitter.Exists) {
                int const SplitterInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Splitter.NodeNumIn;
                // loop across branch outlet nodes and check mass continuity
                int const NumSplitterOutlets = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Splitter.TotalOutletNodes;
                Real64 SumOutletFlow = 0.0;
                for (int OutletNum = 1; OutletNum <= NumSplitterOutlets; ++OutletNum) {
                    int const BranchNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Splitter.BranchNumOut(OutletNum);
                    int const LastNodeOnBranch = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Branch(BranchNum).NodeNumOut;
                    SumOutletFlow += state.dataLoopNodes->Node(LastNodeOnBranch).MassFlowRate;
                }
                Real64 const AbsDifference = std::abs(state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate - SumOutletFlow);
                if (AbsDifference > DataPlant::CriteriaDelta_MassFlowRate) {
                    return true;
                }
            }
        }
    }
    return false;
}

void CheckPlantMixerSplitterConsistency(EnergyPlusData &state, int const LoopNum, int const LoopSideNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Oct 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Check for plant flow resolver errors

    // METHODOLOGY EMPLOYED:
    // compare flow rate of splitter inlet to flow rate of mixer outlet

    // Using/Aliasing
    using DataPlant::CriteriaDelta_MassFlowRate;
    using DataPlant::DemandSide;
    using DataPlant::SupplySide;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MixerOutletNode;
    int SplitterInletNode;
    Real64 AbsDifference;
    int NumSplitterOutlets;
    Real64 SumOutletFlow;
    int OutletNum;
    int BranchNum;
    int LastNodeOnBranch;

    if (!state.dataPlnt->PlantLoop(LoopNum).LoopHasConnectionComp) {
        if (!state.dataGlobal->DoingSizing && !state.dataGlobal->WarmupFlag &&
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Mixer.Exists && !FirstHVACIteration) {
            // Find mixer outlet node number
            MixerOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Mixer.NodeNumOut;
            // Find splitter inlet node number
            SplitterInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.NodeNumIn;

            AbsDifference =
                std::abs(state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate - state.dataLoopNodes->Node(MixerOutletNode).MassFlowRate);
            if (AbsDifference > DataBranchAirLoopPlant::MassFlowTolerance) {
                if (state.dataPlnt->PlantLoop(LoopNum).MFErrIndex1 == 0) {
                    ShowSevereMessage(state, "Plant flows do not resolve -- splitter inlet flow does not match mixer outlet flow ");
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "PlantLoop name= " + state.dataPlnt->PlantLoop(LoopNum).Name);
                    ShowContinueError(state, "Plant Connector:Mixer name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Mixer.Name);
                    ShowContinueError(
                        state, format("Mixer outlet mass flow rate= {:.6R} {{kg/s}}", state.dataLoopNodes->Node(MixerOutletNode).MassFlowRate));
                    ShowContinueError(state,
                                      "Plant Connector:Splitter name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.Name);
                    ShowContinueError(
                        state, format("Splitter inlet mass flow rate= {:.6R} {{kg/s}}", state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate));
                    ShowContinueError(state, format("Difference in two mass flow rates= {:.6R} {{kg/s}}", AbsDifference));
                }
                ShowRecurringSevereErrorAtEnd(state,
                                              "Plant Flows (Loop=" + state.dataPlnt->PlantLoop(LoopNum).Name +
                                                  ") splitter inlet flow not match mixer outlet flow",
                                              state.dataPlnt->PlantLoop(LoopNum).MFErrIndex1,
                                              AbsDifference,
                                              AbsDifference,
                                              _,
                                              "kg/s",
                                              "kg/s");
                if (AbsDifference > DataBranchAirLoopPlant::MassFlowTolerance * 10.0) {
                    ShowSevereError(state, "Plant flows do not resolve -- splitter inlet flow does not match mixer outlet flow ");
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "PlantLoop name= " + state.dataPlnt->PlantLoop(LoopNum).Name);
                    ShowContinueError(state, "Plant Connector:Mixer name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Mixer.Name);
                    ShowContinueError(
                        state, format("Mixer outlet mass flow rate= {:.6R} {{kg/s}}", state.dataLoopNodes->Node(MixerOutletNode).MassFlowRate));
                    ShowContinueError(state,
                                      "Plant Connector:Splitter name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.Name);
                    ShowContinueError(
                        state, format("Splitter inlet mass flow rate= {:.6R} {{kg/s}}", state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate));
                    ShowContinueError(state, format("Difference in two mass flow rates= {:.6R} {{kg/s}}", AbsDifference));
                    ShowFatalError(state, "CheckPlantMixerSplitterConsistency: Simulation terminated because of problems in plant flow resolver");
                }
            }

            // now check inside s/m to see if there are problems

            // loop across branch outlet nodes and check mass continuity
            NumSplitterOutlets = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.TotalOutletNodes;
            SumOutletFlow = 0.0;
            //  SumInletFlow = 0.0;
            for (OutletNum = 1; OutletNum <= NumSplitterOutlets; ++OutletNum) {
                BranchNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.BranchNumOut(OutletNum);
                LastNodeOnBranch = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).NodeNumOut;
                SumOutletFlow += state.dataLoopNodes->Node(LastNodeOnBranch).MassFlowRate;
                //  FirstNodeOnBranch= PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
                //  SumInletFlow = SumInletFlow + Node(FirstNodeOnBranch)%MassFlowRate
            }
            AbsDifference = std::abs(state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate - SumOutletFlow);
            if (AbsDifference > CriteriaDelta_MassFlowRate) {
                if (state.dataPlnt->PlantLoop(LoopNum).MFErrIndex2 == 0) {
                    ShowSevereMessage(state, "Plant flows do not resolve -- splitter inlet flow does not match branch outlet flows");
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "PlantLoop name= " + state.dataPlnt->PlantLoop(LoopNum).Name);
                    ShowContinueError(state, "Plant Connector:Mixer name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Mixer.Name);
                    ShowContinueError(state, format("Sum of Branch outlet mass flow rates= {:.6R} {{kg/s}}", SumOutletFlow));
                    ShowContinueError(state,
                                      "Plant Connector:Splitter name= " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).Splitter.Name);
                    ShowContinueError(
                        state, format("Splitter inlet mass flow rate= {:.6R} {{kg/s}}", state.dataLoopNodes->Node(SplitterInletNode).MassFlowRate));
                    ShowContinueError(state, format("Difference in two mass flow rates= {:.6R} {{kg/s}}", AbsDifference));
                }
                ShowRecurringSevereErrorAtEnd(state,
                                              "Plant Flows (Loop=" + state.dataPlnt->PlantLoop(LoopNum).Name +
                                                  ") splitter inlet flow does not match branch outlet flows",
                                              state.dataPlnt->PlantLoop(LoopNum).MFErrIndex2,
                                              AbsDifference,
                                              AbsDifference,
                                              _,
                                              "kg/s",
                                              "kg/s");
            }
        }
    }
}

void CheckForRunawayPlantTemps(EnergyPlusData &state, int const LoopNum, int const LoopSideNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Check for plant control errors revealed as run away fluid temps
    //  halt program so it won't siliently run in out of control state

    // METHODOLOGY EMPLOYED:
    //  compare plant temps to plant min and max and halt if things run away
    //  sensitivity can be adjusted with parameters, picked somewhat arbitrary

    // REFERENCES:
    // na

    // Using/Aliasing
    using DataPlant::DemandSide;
    using DataPlant::SupplySide;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const OverShootOffset(5.0);
    Real64 const UnderShootOffset(5.0);
    Real64 const FatalOverShootOffset(200.0);
    Real64 const FatalUnderShootOffset(100.0);
    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string hotcold;
    bool makefatalerror;
    std::string DemandSupply;
    int LSN;
    int BrN;
    int CpN;
    Real64 LoopCapacity;
    Real64 LoopDemandSideCapacity;
    Real64 LoopSupplySideCapacity;
    Real64 DispatchedCapacity;
    Real64 LoopDemandSideDispatchedCapacity;
    Real64 LoopSupplySideDispatchedCapacity;

    makefatalerror = false;
    if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp >
        (state.dataPlnt->PlantLoop(LoopNum).MaxTemp + OverShootOffset)) {

        // first stage, throw recurring warning that plant loop is getting out of control
        ShowRecurringWarningErrorAtEnd(state,
                                       "Plant loop exceeding upper temperature limit, PlantLoop=\"" + state.dataPlnt->PlantLoop(LoopNum).Name + "\"",
                                       state.dataPlnt->PlantLoop(LoopNum).MaxTempErrIndex,
                                       state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp);

        if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp >
            (state.dataPlnt->PlantLoop(LoopNum).MaxTemp + FatalOverShootOffset)) {
            hotcold = "hot";
            makefatalerror = true;
        }
    }

    if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp <
        (state.dataPlnt->PlantLoop(LoopNum).MinTemp - UnderShootOffset)) {

        // first stage, throw recurring warning that plant loop is getting out of control
        ShowRecurringWarningErrorAtEnd(state,
                                       "Plant loop falling below lower temperature limit, PlantLoop=\"" + state.dataPlnt->PlantLoop(LoopNum).Name +
                                           "\"",
                                       state.dataPlnt->PlantLoop(LoopNum).MinTempErrIndex,
                                       _,
                                       state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp);

        if (state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp <
            (state.dataPlnt->PlantLoop(LoopNum).MinTemp - FatalUnderShootOffset)) {
            hotcold = "cold";
            makefatalerror = true;
        }
    }

    if (makefatalerror) {
        ShowSevereError(state, "Plant temperatures are getting far too " + hotcold + ", check controls and relative loads and capacities");
        ShowContinueErrorTimeStamp(state, "");
        if (LoopSideNum == DemandSide) {
            DemandSupply = "Demand";
        } else if (LoopSideNum == SupplySide) {
            DemandSupply = "Supply";
        } else {
            DemandSupply = "Unknown";
        }
        ShowContinueError(state, "PlantLoop Name (" + DemandSupply + "Side)= " + state.dataPlnt->PlantLoop(LoopNum).Name);
        ShowContinueError(state,
                          format("PlantLoop Setpoint Temperature={:.1R} {{C}}",
                                 state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint));
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).InletNodeSetPt) {
            ShowContinueError(state, "PlantLoop Inlet Node (SupplySide) has a Setpoint.");
        } else {
            ShowContinueError(state, "PlantLoop Inlet Node (SupplySide) does not have a Setpoint.");
        }
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
            ShowContinueError(state, "PlantLoop Inlet Node (DemandSide) has a Setpoint.");
        } else {
            ShowContinueError(state, "PlantLoop Inlet Node (DemandSide) does not have a Setpoint.");
        }
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).OutletNodeSetPt) {
            ShowContinueError(state, "PlantLoop Outlet Node (SupplySide) has a Setpoint.");
        } else {
            ShowContinueError(state, "PlantLoop Outlet Node (SupplySide) does not have a Setpoint.");
        }
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).OutletNodeSetPt) {
            ShowContinueError(state, "PlantLoop Outlet Node (DemandSide) has a Setpoint.");
        } else {
            ShowContinueError(state, "PlantLoop Outlet Node (DemandSide) does not have a Setpoint.");
        }
        ShowContinueError(state,
                          format("PlantLoop Outlet Node ({}Side) \"{}\" has temperature={:.1R} {{C}}",
                                 DemandSupply,
                                 state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut),
                                 state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).Temp));
        ShowContinueError(state,
                          format("PlantLoop Inlet Node ({}Side) \"{}\" has temperature={:.1R} {{C}}",
                                 DemandSupply,
                                 state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumIn),
                                 state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumIn).Temp));
        ShowContinueError(state, format("PlantLoop Minimum Temperature={:.1R} {{C}}", state.dataPlnt->PlantLoop(LoopNum).MinTemp));
        ShowContinueError(state, format("PlantLoop Maximum Temperature={:.1R} {{C}}", state.dataPlnt->PlantLoop(LoopNum).MaxTemp));
        ShowContinueError(
            state,
            format("PlantLoop Flow Request (SupplySide)={:.1R} {{kg/s}}", state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).FlowRequest));
        ShowContinueError(
            state,
            format("PlantLoop Flow Request (DemandSide)={:.1R} {{kg/s}}", state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).FlowRequest));
        ShowContinueError(state,
                          format("PlantLoop Node ({}Side) \"{}\" has mass flow rate ={:.1R} {{kg/s}}",
                                 DemandSupply,
                                 state.dataLoopNodes->NodeID(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut),
                                 state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).NodeNumOut).MassFlowRate));
        ShowContinueError(
            state, format("PlantLoop PumpHeat (SupplySide)={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).TotalPumpHeat));
        ShowContinueError(
            state, format("PlantLoop PumpHeat (DemandSide)={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).TotalPumpHeat));
        ShowContinueError(state, format("PlantLoop Cooling Demand={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).CoolingDemand));
        ShowContinueError(state, format("PlantLoop Heating Demand={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).HeatingDemand));
        ShowContinueError(state, format("PlantLoop Demand not Dispatched={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).DemandNotDispatched));
        ShowContinueError(state, format("PlantLoop Unmet Demand={:.1R} {{W}}", state.dataPlnt->PlantLoop(LoopNum).UnmetDemand));

        LoopCapacity = 0.0;
        DispatchedCapacity = 0.0;
        for (LSN = DemandSide; LSN <= SupplySide; ++LSN) {
            for (BrN = 1; BrN <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).TotalBranches; ++BrN) {
                for (CpN = 1; CpN <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).TotalComponents; ++CpN) {
                    LoopCapacity += state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).Comp(CpN).MaxLoad;
                    DispatchedCapacity += std::abs(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).Comp(CpN).MyLoad);
                }
            }
            if (LSN == DemandSide) {
                LoopDemandSideCapacity = LoopCapacity;
                LoopDemandSideDispatchedCapacity = DispatchedCapacity;
            } else {
                LoopSupplySideCapacity = LoopCapacity - LoopDemandSideCapacity;
                LoopSupplySideDispatchedCapacity = DispatchedCapacity - LoopDemandSideDispatchedCapacity;
            }
        }
        ShowContinueError(state, format("PlantLoop Capacity={:.1R} {{W}}", LoopCapacity));
        ShowContinueError(state, format("PlantLoop Capacity (SupplySide)={:.1R} {{W}}", LoopSupplySideCapacity));
        ShowContinueError(state, format("PlantLoop Capacity (DemandSide)={:.1R} {{W}}", LoopDemandSideCapacity));
        ShowContinueError(state, "PlantLoop Operation Scheme=" + state.dataPlnt->PlantLoop(LoopNum).OperationScheme);
        ShowContinueError(state, format("PlantLoop Operation Dispatched Load = {:.1R} {{W}}", DispatchedCapacity));
        ShowContinueError(state, format("PlantLoop Operation Dispatched Load (SupplySide)= {:.1R} {{W}}", LoopSupplySideDispatchedCapacity));
        ShowContinueError(state, format("PlantLoop Operation Dispatched Load (DemandSide)= {:.1R} {{W}}", LoopDemandSideDispatchedCapacity));
        ShowContinueError(state, "Branches on the Loop.");
        ShowBranchesOnLoop(state, LoopNum);
        ShowContinueError(state, "*************************");
        ShowContinueError(state, "Possible things to look for to correct this problem are:");
        ShowContinueError(state, "  Capacity, Operation Scheme, Mass flow problems, Pump Heat building up over time.");
        ShowContinueError(state, "  Try a shorter runperiod to stop before it fatals and look at");
        ShowContinueError(state, "    lots of node time series data to see what is going wrong.");
        ShowContinueError(state, "  If this is happening during Warmup, you can use Output:Diagnostics,ReportDuringWarmup;");
        ShowContinueError(state, "  This is detected at the loop level, but the typical problems are in the components.");
        ShowFatalError(state, "CheckForRunawayPlantTemps: Simulation terminated because of run away plant temperatures, too " + hotcold);
    }
}

void SetAllFlowLocks(EnergyPlusData &state, DataPlant::iFlowLock const Value)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   November 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine will set both LoopSide flowlocks on all plant loops to the input value (0 or 1)
    // Initially this routine is used as a quick replacement for the FlowLock=0 and FlowLock=1 statements
    //  in order to provide the same behavior through phase I of the demand side rewrite
    // Eventually this routine may be employed again to quickly initialize all loops once phase III is complete
    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (int LoopSideNum = 1; LoopSideNum <= isize(state.dataPlnt->PlantLoop(LoopNum).LoopSide); ++LoopSideNum) {
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock = Value;
        }
    }
}

void ResetAllPlantInterConnectFlags(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine will reset all interconnected (air, zone, etc.) sim flags for both loopsides of all loops

    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (auto &e : state.dataPlnt->PlantLoop(LoopNum).LoopSide) {
            e.SimAirLoopsNeeded = false;
            e.SimZoneEquipNeeded = false;
            e.SimNonZoneEquipNeeded = false;
            e.SimElectLoadCentrNeeded = false;
        }
    }
}

void PullCompInterconnectTrigger(EnergyPlusData &state,
                                 int const LoopNum,                           // component's loop index
                                 int const LoopSide,                          // component's loop side number
                                 int const BranchNum,                         // Component's branch number
                                 int const CompNum,                           // Component's comp number
                                 int &UniqueCriteriaCheckIndex,               // An integer given to this particular check
                                 int const ConnectedLoopNum,                  // Component's interconnected loop number
                                 int const ConnectedLoopSide,                 // Component's interconnected loop side number
                                 DataPlant::iCriteriaType const CriteriaType, // The criteria check to use, see DataPlant: SimFlagCriteriaTypes
                                 Real64 const CriteriaValue                   // The value of the criteria check to evaluate
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides a generic means for components to trigger interconnected loop sides sim flags

    // METHODOLOGY EMPLOYED:
    // Determine convergence criteria based on *CriteriaType* variable.  This routine only turns
    //  the loop side sim flag ON, it doesn't turn it OFF.
    // The convergence value history was originally going to be put at the Branch()%Comp()%...
    //  level, but this would be quite difficult if we had multiple convergence checks for the
    //  same component, such as if a chiller was trying to turn on the condenser side and the
    //  heat recovery side.
    // It was determined to use a local array, which is only reallocated during the first stages
    //  of the simulation when components are first calling their sim flag requests.  After that
    //  the INOUT index variable will be used to avoid reallocation and string compares.
    // Error handling will be put in to ensure unique identifiers are used for debugging purposes.
    // A single component may have multiple check indeces, but a single index will only have one
    //  associated component.  Therefore whenever we come in with a non-zero index, we will just
    //  verify that the stored loop/side/branch/comp matches

    // Using/Aliasing
    using DataPlant::CriteriaDelta_HeatTransferRate;
    using DataPlant::CriteriaDelta_MassFlowRate;
    using DataPlant::CriteriaDelta_Temperature;

    CriteriaData CurCriteria; // for convenience

    if (UniqueCriteriaCheckIndex <= 0) { // If we don't yet have an index, we need to initialize

        // We need to start by allocating, or REallocating the array
        int const CurrentNumChecksStored(static_cast<int>(state.dataPlantUtilities->CriteriaChecks.size() + 1));
        state.dataPlantUtilities->CriteriaChecks.redimension(CurrentNumChecksStored);

        // Store the unique name and location
        state.dataPlantUtilities->CriteriaChecks(CurrentNumChecksStored).CallingCompLoopNum = LoopNum;
        state.dataPlantUtilities->CriteriaChecks(CurrentNumChecksStored).CallingCompLoopSideNum = LoopSide;
        state.dataPlantUtilities->CriteriaChecks(CurrentNumChecksStored).CallingCompBranchNum = BranchNum;
        state.dataPlantUtilities->CriteriaChecks(CurrentNumChecksStored).CallingCompCompNum = CompNum;

        // Since this was the first pass, it is safe to assume something has changed!
        // Therefore we'll set the sim flag to true
        state.dataPlnt->PlantLoop(ConnectedLoopNum).LoopSide(ConnectedLoopSide).SimLoopSideNeeded = true;

        // Make sure we return the proper value of index
        UniqueCriteriaCheckIndex = CurrentNumChecksStored;

    } else { // We already have an index

        // If we have an index, we need to do a brief error handling, then determine
        //  sim flag status based on the criteria type

        // First store the current check in a single variable instead of array for readability
        CurCriteria = state.dataPlantUtilities->CriteriaChecks(UniqueCriteriaCheckIndex);

        // Check to make sure we didn't reuse the index in multiple components
        if (CurCriteria.CallingCompLoopNum != LoopNum || CurCriteria.CallingCompLoopSideNum != LoopSide ||
            CurCriteria.CallingCompBranchNum != BranchNum || CurCriteria.CallingCompCompNum != CompNum) {
            // Diagnostic fatal: component does not properly utilize unique indexing
        }

        // Initialize, then check if we are out of range
        {
            auto const SELECT_CASE_var(CriteriaType);
            if (SELECT_CASE_var == DataPlant::iCriteriaType::MassFlowRate) {
                if (std::abs(CurCriteria.ThisCriteriaCheckValue - CriteriaValue) > CriteriaDelta_MassFlowRate) {
                    state.dataPlnt->PlantLoop(ConnectedLoopNum).LoopSide(ConnectedLoopSide).SimLoopSideNeeded = true;
                }

            } else if (SELECT_CASE_var == DataPlant::iCriteriaType::Temperature) {
                if (std::abs(CurCriteria.ThisCriteriaCheckValue - CriteriaValue) > CriteriaDelta_Temperature) {
                    state.dataPlnt->PlantLoop(ConnectedLoopNum).LoopSide(ConnectedLoopSide).SimLoopSideNeeded = true;
                }

            } else if (SELECT_CASE_var == DataPlant::iCriteriaType::HeatTransferRate) {
                if (std::abs(CurCriteria.ThisCriteriaCheckValue - CriteriaValue) > CriteriaDelta_HeatTransferRate) {
                    state.dataPlnt->PlantLoop(ConnectedLoopNum).LoopSide(ConnectedLoopSide).SimLoopSideNeeded = true;
                }

            } else {
                // Diagnostic fatal: improper criteria type
            }
        }

    } // if we have an index or not

    // Store the value for the next pass
    state.dataPlantUtilities->CriteriaChecks(UniqueCriteriaCheckIndex).ThisCriteriaCheckValue = CriteriaValue;
}

void UpdateChillerComponentCondenserSide(EnergyPlusData &state,
                                         int const LoopNum,                    // component's loop index
                                         int const LoopSide,                   // component's loop side number
                                         [[maybe_unused]] int const TypeOfNum, // Component's type index
                                         int const InletNodeNum,               // Component's inlet node pointer
                                         int const OutletNodeNum,              // Component's outlet node pointer
                                         Real64 const ModelCondenserHeatRate,  // model's heat rejection rate at condenser (W)
                                         Real64 const ModelInletTemp,          // model's inlet temperature (C)
                                         Real64 const ModelOutletTemp,         // model's outlet temperature (C)
                                         Real64 const ModelMassFlowRate,       // model's condenser water mass flow rate (kg/s)
                                         bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provides reusable update routine for water cooled chiller's condenser water
    // connection to plant loops

    // METHODOLOGY EMPLOYED:
    // check if anything changed or doesn't agree and set simulation flags.
    // update outlet conditions if needed or possible

    // Using/Aliasing
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("UpdateChillerComponentCondenserSide");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool DidAnythingChange(false); // set to true if conditions changed
    int OtherLoopNum;              // local loop pointer for remote connected loop
    int OtherLoopSide;             // local loop side pointer for remote connected loop
    int ConnectLoopNum;            // local do loop counter
    Real64 Cp;

    // check if any conditions have changed
    if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate != ModelMassFlowRate) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate != ModelMassFlowRate) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(InletNodeNum).Temp != ModelInletTemp) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(OutletNodeNum).Temp != ModelOutletTemp) DidAnythingChange = true;

    // could also check heat rate agains McDeltaT from node data

    if ((state.dataLoopNodes->Node(InletNodeNum).MassFlowRate == 0.0) && (ModelCondenserHeatRate > 0.0)) {

        // TODO also send a request that condenser loop be made available, interlock message infrastructure??

        DidAnythingChange = true;
    }

    if (DidAnythingChange || FirstHVACIteration) {
        // use current mass flow rate and inlet temp from Node and recalculate outlet temp
        if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            // update node outlet conditions
            Cp = GetSpecificHeatGlycol(
                state, state.dataPlnt->PlantLoop(LoopNum).FluidName, ModelInletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
            state.dataLoopNodes->Node(OutletNodeNum).Temp =
                state.dataLoopNodes->Node(InletNodeNum).Temp + ModelCondenserHeatRate / (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * Cp);
        }

        // set sim flag for this loop
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = true;

        // set sim flag on connected loops to true because this side changed
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected > 0) {
            for (ConnectLoopNum = 1; ConnectLoopNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected; ++ConnectLoopNum) {
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopDemandsOnRemote) {
                    OtherLoopNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopNum;
                    OtherLoopSide = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopSideNum;
                    state.dataPlnt->PlantLoop(OtherLoopNum).LoopSide(OtherLoopSide).SimLoopSideNeeded = true;
                }
            }
        }

    } else { // nothing changed so turn off sim flag
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = false;
    }
}

void UpdateComponentHeatRecoverySide(EnergyPlusData &state,
                                     int const LoopNum,                    // component's loop index
                                     int const LoopSide,                   // component's loop side number
                                     [[maybe_unused]] int const TypeOfNum, // Component's type index
                                     int const InletNodeNum,               // Component's inlet node pointer
                                     int const OutletNodeNum,              // Component's outlet node pointer
                                     Real64 const ModelRecoveryHeatRate,   // model's heat rejection rate at recovery (W)
                                     Real64 const ModelInletTemp,          // model's inlet temperature (C)
                                     Real64 const ModelOutletTemp,         // model's outlet temperature (C)
                                     Real64 const ModelMassFlowRate,       // model's condenser water mass flow rate (kg/s)
                                     bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provides reusable update routine for heat recovery type
    // connection to plant loops

    // METHODOLOGY EMPLOYED:
    // check if anything changed or doesn't agree and set simulation flags.
    // update outlet conditions if needed or possible

    // Using/Aliasing
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("UpdateComponentHeatRecoverySide");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool DidAnythingChange(false); // set to true if conditions changed
    int OtherLoopNum;              // local loop pointer for remote connected loop
    int OtherLoopSide;             // local loop side pointer for remote connected loop
    int ConnectLoopNum;            // local do loop counter
    Real64 Cp;                     // local fluid specific heat

    // check if any conditions have changed
    if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate != ModelMassFlowRate) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate != ModelMassFlowRate) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(InletNodeNum).Temp != ModelInletTemp) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(OutletNodeNum).Temp != ModelOutletTemp) DidAnythingChange = true;

    // could also check heat rate against McDeltaT from node data

    if ((state.dataLoopNodes->Node(InletNodeNum).MassFlowRate == 0.0) && (ModelRecoveryHeatRate > 0.0)) {
        // no flow but trying to move heat to this loop problem!

        DidAnythingChange = true;
    }

    if (DidAnythingChange || FirstHVACIteration) {
        // use current mass flow rate and inlet temp from Node and recalculate outlet temp
        if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            // update node outlet conditions
            Cp = GetSpecificHeatGlycol(
                state, state.dataPlnt->PlantLoop(LoopNum).FluidName, ModelInletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
            state.dataLoopNodes->Node(OutletNodeNum).Temp =
                state.dataLoopNodes->Node(InletNodeNum).Temp + ModelRecoveryHeatRate / (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * Cp);
        }

        // set sim flag for this loop
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = true;

        // set sim flag on connected loops to true because this side changed
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected > 0) {
            for (ConnectLoopNum = 1; ConnectLoopNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected; ++ConnectLoopNum) {
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopDemandsOnRemote) {
                    OtherLoopNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopNum;
                    OtherLoopSide = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopSideNum;
                    state.dataPlnt->PlantLoop(OtherLoopNum).LoopSide(OtherLoopSide).SimLoopSideNeeded = true;
                }
            }
        }

    } else { // nothing changed so turn off sim flag
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = false;
    }
}

void UpdateAbsorberChillerComponentGeneratorSide(EnergyPlusData &state,
                                                 int const LoopNum,                                                 // component's loop index
                                                 int const LoopSide,                                                // component's loop side number
                                                 [[maybe_unused]] int const TypeOfNum,                              // Component's type index
                                                 int const InletNodeNum,                                            // Component's inlet node pointer
                                                 [[maybe_unused]] int const OutletNodeNum,                          // Component's outlet node pointer
                                                 [[maybe_unused]] DataLoopNode::NodeFluidType const HeatSourceType, // Type of fluid in Generator loop
                                                 Real64 const ModelGeneratorHeatRate,                               // model's generator heat rate (W)
                                                 Real64 const ModelMassFlowRate, // model's generator mass flow rate (kg/s)
                                                 bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   February 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provides reusable update routine for absoption chiller's generator
    // connection to plant loops

    // METHODOLOGY EMPLOYED:
    // check if anything changed or doesn't agree and set simulation flags.
    // update outlet conditions if needed or possible

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool DidAnythingChange(false); // set to true if conditions changed
    int OtherLoopNum;              // local loop pointer for remote connected loop
    int OtherLoopSide;             // local loop side pointer for remote connected loop
    int ConnectLoopNum;            // local do loop counter

    // check if any conditions have changed
    if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate != ModelMassFlowRate) DidAnythingChange = true;

    if ((state.dataLoopNodes->Node(InletNodeNum).MassFlowRate == 0.0) && (ModelGeneratorHeatRate > 0.0)) {

        //  TODO also send a request that generator loop be made available, interlock message infrastructure??

        DidAnythingChange = true;
    }

    if (DidAnythingChange || FirstHVACIteration) {

        // set sim flag for this loop
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = true;

        // set sim flag on connected loops to true because this side changed
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected > 0) {
            for (ConnectLoopNum = 1; ConnectLoopNum <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).TotalConnected; ++ConnectLoopNum) {
                if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopDemandsOnRemote) {
                    OtherLoopNum = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopNum;
                    OtherLoopSide = state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).Connected(ConnectLoopNum).LoopSideNum;
                    state.dataPlnt->PlantLoop(OtherLoopNum).LoopSide(OtherLoopSide).SimLoopSideNeeded = true;
                }
            }
        }

    } else { // nothing changed so turn off sim flag
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = false;
    }
}

void InterConnectTwoPlantLoopSides(EnergyPlusData &state,
                                   int const Loop1Num,
                                   int const Loop1LoopSideNum,
                                   int const Loop2Num,
                                   int const Loop2LoopSideNum,
                                   int const PlantComponentTypeOfNum,
                                   bool const Loop1DemandsOnLoop2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   February 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Setup PlantLoop data structure pointers to direct interacting loops

    // Using/Aliasing
    using DataPlant::ConnectedLoopData;

    if (Loop1Num == 0 || Loop1LoopSideNum == 0 || Loop2Num == 0 || Loop2LoopSideNum == 0) {
        return; // Associated ScanPlantLoopsForObject couldn't find the component in the the plant loop structure...
    }           // This is a Fatal error condition

    bool const Loop2DemandsOnLoop1(!Loop1DemandsOnLoop2);

    int TotalConnected;

    auto &loop_side_1(state.dataPlnt->PlantLoop(Loop1Num).LoopSide(Loop1LoopSideNum));
    auto &connected_1(loop_side_1.Connected);
    if (allocated(connected_1)) {
        TotalConnected = ++loop_side_1.TotalConnected;
        connected_1.redimension(TotalConnected);
    } else {
        TotalConnected = loop_side_1.TotalConnected = 1;
        connected_1.allocate(1);
    }
    connected_1(TotalConnected).LoopNum = Loop2Num;
    connected_1(TotalConnected).LoopSideNum = Loop2LoopSideNum;
    connected_1(TotalConnected).ConnectorTypeOf_Num = PlantComponentTypeOfNum;
    connected_1(TotalConnected).LoopDemandsOnRemote = Loop1DemandsOnLoop2;

    auto &loop_side_2(state.dataPlnt->PlantLoop(Loop2Num).LoopSide(Loop2LoopSideNum));
    auto &connected_2(loop_side_2.Connected);
    if (allocated(connected_2)) {
        TotalConnected = ++loop_side_2.TotalConnected;
        connected_2.redimension(TotalConnected);
    } else {
        TotalConnected = loop_side_2.TotalConnected = 1;
        connected_2.allocate(1);
    }
    connected_2(TotalConnected).LoopNum = Loop1Num;
    connected_2(TotalConnected).LoopSideNum = Loop1LoopSideNum;
    connected_2(TotalConnected).ConnectorTypeOf_Num = PlantComponentTypeOfNum;
    connected_2(TotalConnected).LoopDemandsOnRemote = Loop2DemandsOnLoop1;
}

void ShiftPlantLoopSideCallingOrder(EnergyPlusData &state, int const OldIndex, int const NewIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   <April 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // re-arrange the calling order, move one loop side from an old index to a new one

    // Using/Aliasing
    using namespace DataPlant;

    // Object Data
    PlantCallingOrderInfoStruct RecordToMoveInPlantCallingOrderInfo;

    if (OldIndex == 0) {
        ShowSevereError(state, "ShiftPlantLoopSideCallingOrder: developer error notice of invalid index, Old Index=0");
    }
    if (NewIndex == 0) {
        ShowSevereError(state, "ShiftPlantLoopSideCallingOrder: developer error notice of invalid index, New Index=1");
    }
    if ((OldIndex == 0) || (NewIndex == 0)) {
        return;
    }

    // store copy of prior structure
    Array1D<PlantCallingOrderInfoStruct> TempPlantCallingOrderInfo(state.dataPlnt->PlantCallingOrderInfo);

    RecordToMoveInPlantCallingOrderInfo = state.dataPlnt->PlantCallingOrderInfo(OldIndex);

    if (OldIndex == NewIndex) {
        // do nothing, no shift needed.
    } else if ((OldIndex == 1) && (NewIndex > OldIndex) && (NewIndex < state.dataPlnt->TotNumHalfLoops)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 1, NI = 5)
        // example shifted:  2  3  4  5  1  6  7  8

        state.dataPlnt->PlantCallingOrderInfo({1, NewIndex - 1}) = TempPlantCallingOrderInfo({2, NewIndex});
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
        state.dataPlnt->PlantCallingOrderInfo({NewIndex + 1, state.dataPlnt->TotNumHalfLoops}) =
            TempPlantCallingOrderInfo({NewIndex + 1, state.dataPlnt->TotNumHalfLoops});

    } else if ((OldIndex == 1) && (NewIndex > OldIndex) && (NewIndex == state.dataPlnt->TotNumHalfLoops)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 1, NI = 8)
        // example shifted:  2  3  4  5  6  7  8  1

        state.dataPlnt->PlantCallingOrderInfo({1, NewIndex - 1}) = TempPlantCallingOrderInfo({2, NewIndex});
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
    } else if ((OldIndex > 1) && (NewIndex > OldIndex) && (NewIndex < state.dataPlnt->TotNumHalfLoops)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 6)
        // example shifted:  1  2  4  5  6  3  7  8
        state.dataPlnt->PlantCallingOrderInfo({1, OldIndex - 1}) = TempPlantCallingOrderInfo({1, OldIndex - 1});
        state.dataPlnt->PlantCallingOrderInfo({OldIndex, NewIndex - 1}) = TempPlantCallingOrderInfo({OldIndex + 1, NewIndex});
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
        state.dataPlnt->PlantCallingOrderInfo({NewIndex + 1, state.dataPlnt->TotNumHalfLoops}) =
            TempPlantCallingOrderInfo({NewIndex + 1, state.dataPlnt->TotNumHalfLoops});
    } else if ((OldIndex > 1) && (NewIndex > OldIndex) && (NewIndex == state.dataPlnt->TotNumHalfLoops)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 8)
        // example shifted:  1  2  4  5  6  7  8  3
        state.dataPlnt->PlantCallingOrderInfo({1, OldIndex - 1}) = TempPlantCallingOrderInfo({1, OldIndex - 1});
        state.dataPlnt->PlantCallingOrderInfo({OldIndex, NewIndex - 1}) = TempPlantCallingOrderInfo({OldIndex + 1, NewIndex});
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
    } else if ((OldIndex > 1) && (NewIndex < OldIndex) && (NewIndex == 1)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 1)
        // example shifted:  3  1  2  4  5  6  7  8
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
        state.dataPlnt->PlantCallingOrderInfo({NewIndex + 1, OldIndex}) = TempPlantCallingOrderInfo({1, OldIndex - 1});
        state.dataPlnt->PlantCallingOrderInfo({OldIndex + 1, state.dataPlnt->TotNumHalfLoops}) =
            TempPlantCallingOrderInfo({OldIndex + 1, state.dataPlnt->TotNumHalfLoops});

    } else if ((OldIndex > 1) && (NewIndex < OldIndex) && (NewIndex > 1)) {
        // example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 2)
        // example shifted:  1  3  2  4  5  6  7  8
        state.dataPlnt->PlantCallingOrderInfo({1, NewIndex - 1}) = TempPlantCallingOrderInfo({1, NewIndex - 1});
        state.dataPlnt->PlantCallingOrderInfo(NewIndex) = RecordToMoveInPlantCallingOrderInfo;
        state.dataPlnt->PlantCallingOrderInfo({NewIndex + 1, OldIndex}) = TempPlantCallingOrderInfo({NewIndex, NewIndex + (OldIndex - NewIndex) - 1});
        state.dataPlnt->PlantCallingOrderInfo({OldIndex + 1, state.dataPlnt->TotNumHalfLoops}) =
            TempPlantCallingOrderInfo({OldIndex + 1, state.dataPlnt->TotNumHalfLoops});

    } else {
        ShowSevereError(state,
                        "ShiftPlantLoopSideCallingOrder: developer error notice, caught unexpected logical case in "
                        "ShiftPlantLoopSideCallingOrder PlantUtilities");
    }
}

void RegisterPlantCompDesignFlow(EnergyPlusData &state,
                                 int const ComponentInletNodeNum, // the component's water inlet node number
                                 Real64 const DesPlantFlow        // the component's design fluid volume flow rate [m3/s]
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl(previously SaveCompDesWaterFlow in General.cc)
    //       DATE WRITTEN   January 2004
    //       MODIFIED
    //       RE-ENGINEERED  B. Griffith April 2011, allow to enter repeatedly

    // PURPOSE OF THIS SUBROUTINE:
    // Regester the design fluid flow rates of plant components for sizing purposes
    // in an array that can be accessed by the plant manager routines
    // allows sizing routines to iterate by safely processing repeated calls from the same component

    // METHODOLOGY EMPLOYED:
    // Derived from SaveCompDesWaterFlow but changed to allow re entry with the same node just update
    // the information at the same location in the structure
    // The design flow rate is stored in a dynamic structure array along with the plant component's inlet node number
    // (which is used by plant as a component identifier instead if name and type).

    // Using/Aliasing
    using namespace DataSizing;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumPlantComps;
    int PlantCompNum; // component do loop index
    bool Found;
    int thisCallNodeIndex;

    NumPlantComps = state.dataSize->SaveNumPlantComps;

    if (NumPlantComps == 0) { // first time in, fill and return
        NumPlantComps = 1;
        state.dataSize->CompDesWaterFlow.allocate(NumPlantComps);
        // save the new data
        state.dataSize->CompDesWaterFlow(NumPlantComps).SupNode = ComponentInletNodeNum;
        state.dataSize->CompDesWaterFlow(NumPlantComps).DesVolFlowRate = DesPlantFlow;
        state.dataSize->SaveNumPlantComps = NumPlantComps;
        return;
    }

    Found = false;
    // find node num index in structure if any
    for (PlantCompNum = 1; PlantCompNum <= NumPlantComps; ++PlantCompNum) {
        if (ComponentInletNodeNum == state.dataSize->CompDesWaterFlow(PlantCompNum).SupNode) {
            Found = true;
            thisCallNodeIndex = PlantCompNum;
        }
        if (Found) break;
    }

    if (!Found) {        // grow structure and add new node at the end
        ++NumPlantComps; // increment the number of components that use water as a source of heat or coolth
        state.dataSize->CompDesWaterFlow.emplace_back(ComponentInletNodeNum, DesPlantFlow); // Append the new element
        state.dataSize->SaveNumPlantComps = NumPlantComps;
    } else {
        state.dataSize->CompDesWaterFlow(thisCallNodeIndex).SupNode = ComponentInletNodeNum;
        state.dataSize->CompDesWaterFlow(thisCallNodeIndex).DesVolFlowRate = DesPlantFlow;
    }
}

void SafeCopyPlantNode(EnergyPlusData &state,
                       int const InletNodeNum,
                       int const OutletNodeNum,
                       Optional_int_const LoopNum,
                       [[maybe_unused]] Optional<Real64 const> OutletTemp // set on outlet node if present and water.
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B.  Griffith
    //       DATE WRITTEN   February, 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provide a safer alternative for Node(outlet) = Node(inlet)
    // Intended just for plant

    // METHODOLOGY EMPLOYED:
    // Copy over state variables but not setpoints
    // derived from adiabatic Pipes

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    state.dataLoopNodes->Node(OutletNodeNum).FluidType = state.dataLoopNodes->Node(InletNodeNum).FluidType;

    state.dataLoopNodes->Node(OutletNodeNum).Temp = state.dataLoopNodes->Node(InletNodeNum).Temp;
    state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(InletNodeNum).MassFlowRate;
    state.dataLoopNodes->Node(OutletNodeNum).Quality = state.dataLoopNodes->Node(InletNodeNum).Quality;
    state.dataLoopNodes->Node(OutletNodeNum).Enthalpy =
        state.dataLoopNodes->Node(InletNodeNum).Enthalpy; // should have routines that keep this current with temp?

    state.dataLoopNodes->Node(OutletNodeNum).TempMin = state.dataLoopNodes->Node(InletNodeNum).TempMin;
    state.dataLoopNodes->Node(OutletNodeNum).TempMax = state.dataLoopNodes->Node(InletNodeNum).TempMax;
    state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMinAvail =
        max(state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMin, state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMinAvail);
    state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail =
        min(state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMax, state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMaxAvail);

    state.dataLoopNodes->Node(OutletNodeNum).HumRat = state.dataLoopNodes->Node(InletNodeNum).HumRat; // air only?

    // Only pass pressure if we aren't doing a pressure simulation
    if (present(LoopNum)) {
        switch (state.dataPlnt->PlantLoop(LoopNum).PressureSimType) {
        case DataPlant::iPressSimType::NoPressure:
            state.dataLoopNodes->Node(OutletNodeNum).Press = state.dataLoopNodes->Node(InletNodeNum).Press;
        default:
            // Don't do anything
            break;
        }
    }
}

Real64 BoundValueToNodeMinMaxAvail(EnergyPlusData &state, Real64 const ValueToBound, int const NodeNumToBoundWith)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Provides a clean way to quickly bound a generic value to within any node's minavail and maxavail range

    // METHODOLOGY EMPLOYED:
    // Bound up to min avail, down to max avail

    // Return value
    Real64 BoundedValue;

    BoundedValue = ValueToBound;
    BoundedValue = max(BoundedValue, state.dataLoopNodes->Node(NodeNumToBoundWith).MassFlowRateMinAvail);
    BoundedValue = min(BoundedValue, state.dataLoopNodes->Node(NodeNumToBoundWith).MassFlowRateMaxAvail);

    return BoundedValue;
}

void TightenNodeMinMaxAvails(EnergyPlusData &state, int const NodeNum, Real64 const NewMinAvail, Real64 const NewMaxAvail)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   January, 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides a means of tightening up min/max avail on a node if possible

    // METHODOLOGY EMPLOYED:
    // Bring up node min avail to new min avail if it doesn't violate any other node conditions
    // Pull down node max avail to new max avail if it doesn't violate any other node conditions
    // Assumes that current min/max avails are already honoring hardware min/max values, so they aren't checked here

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OldMinAvail;
    Real64 OldMaxAvail;

    OldMinAvail = state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail;
    OldMaxAvail = state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail;

    // If the new min avail is higher than previous, and it isn't higher than the max avail, update MIN AVAIL
    if ((NewMinAvail > OldMinAvail) && (NewMinAvail <= OldMaxAvail)) state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail = NewMinAvail;

    // If the new max avail is lower than previous, and it isn't lower than the min avail, update MAX AVAIL
    if ((NewMaxAvail < OldMaxAvail) && (NewMaxAvail >= OldMinAvail)) state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail = NewMaxAvail;
}

Real64 BoundValueToWithinTwoValues(Real64 const ValueToBound, Real64 const LowerBound, Real64 const UpperBound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Provides a clean way to quickly bound a generic value to within any two other values

    // METHODOLOGY EMPLOYED:
    // Bound up to min and down to max

    // Return value
    Real64 BoundedValue;

    BoundedValue = ValueToBound;
    BoundedValue = max(BoundedValue, LowerBound);
    BoundedValue = min(BoundedValue, UpperBound);

    return BoundedValue;
}

bool IntegerIsWithinTwoValues(int const ValueToCheck, int const LowerBound, int const UpperBound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   September 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Provides a clean way to quickly check if an integer is within two values

    // METHODOLOGY EMPLOYED:
    // TRUE if ValueToCheck = [LowerBound, UpperBound]
    // in other words, it returns true if ValueToCheck=LowerBound, or if ValueToCheck=UpperBound

    // Return value
    return (ValueToCheck >= LowerBound) && (ValueToCheck <= UpperBound);
}

// In-Place Right Shift by 1 of Array Elements
void rshift1(Array1D<Real64> &a, Real64 const a_l)
{
    assert(a.size_bounded());
    for (int i = a.u(), e = a.l(); i > e; --i) {
        a(i) = a(i - 1);
    }
    a(a.l()) = a_l;
}

void LogPlantConvergencePoints(EnergyPlusData &state, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   Summer 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine stores the history of the plant convergence to check for stuck (max iteration) conditions

    // METHODOLOGY EMPLOYED:
    // Loop across all loops and loopsides
    //   On first hvac, reset the history arrays to begin anew
    //   Pick up the LoopSide inlet and outlet temp and flow rate
    //   Store this in the history array of each node using EOSHIFT

    for (int ThisLoopNum = 1; ThisLoopNum <= isize(state.dataPlnt->PlantLoop); ++ThisLoopNum) {
        auto &loop(state.dataPlnt->PlantLoop(ThisLoopNum));
        for (int ThisLoopSide = 1; ThisLoopSide <= isize(loop.LoopSide); ++ThisLoopSide) {
            auto &loop_side(loop.LoopSide(ThisLoopSide));

            if (FirstHVACIteration) {
                loop_side.InletNode.TemperatureHistory = 0.0;
                loop_side.InletNode.MassFlowRateHistory = 0.0;
                loop_side.OutletNode.TemperatureHistory = 0.0;
                loop_side.OutletNode.MassFlowRateHistory = 0.0;
            }

            int InletNodeNum = loop_side.NodeNumIn;
            Real64 InletNodeTemp = state.dataLoopNodes->Node(InletNodeNum).Temp;
            Real64 InletNodeMdot = state.dataLoopNodes->Node(InletNodeNum).MassFlowRate;

            int OutletNodeNum = loop_side.NodeNumOut;
            Real64 OutletNodeTemp = state.dataLoopNodes->Node(OutletNodeNum).Temp;
            Real64 OutletNodeMdot = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate;

            rshift1(loop_side.InletNode.TemperatureHistory, InletNodeTemp);
            rshift1(loop_side.InletNode.MassFlowRateHistory, InletNodeMdot);
            rshift1(loop_side.OutletNode.TemperatureHistory, OutletNodeTemp);
            rshift1(loop_side.OutletNode.MassFlowRateHistory, OutletNodeMdot);
        }
    }
}

void ScanPlantLoopsForObject(EnergyPlusData &state,
                             std::string_view CompName,
                             int const CompType,
                             int &LoopNum,
                             int &LoopSideNum,
                             int &BranchNum,
                             int &CompNum,
                             bool &errFlag,
                             Optional<Real64 const> LowLimitTemp,
                             Optional<Real64 const> HighLimitTemp,
                             Optional_int CountMatchPlantLoops,
                             Optional_int_const InletNodeNumber,
                             Optional_int_const SingleLoopSearch)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   November 2009
    //       MODIFIED       B. Griffith, changes to help with single component one multiple plant loops
    //       RE-ENGINEERED  na
    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine scans the plant loop structure trying to find the component by type then name.
    // If there are more than one match, it counts them up and returns count using an optional output arg
    // If the option input declaring the component inlet's node name, then the matching is more specific.
    // An optional input, lowlimittemp, can be passed in to be used in the PlantCondLoopOperation routines
    //  when distributing loads to components
    // METHODOLOGY EMPLOYED:
    // Standard EnergyPlus methodology.

    // Using/Aliasing
    using BranchInputManager::AuditBranches;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopCtr;
    int LoopSideCtr;
    int BranchCtr;
    int CompCtr;
    bool FoundComponent;
    int FoundCount;
    bool FoundCompName;
    int StartingLoopNum;
    int EndingLoopNum;

    FoundCount = 0;

    FoundComponent = false;
    FoundCompName = false;
    StartingLoopNum = 1;
    EndingLoopNum = state.dataPlnt->TotNumLoops;
    if (present(SingleLoopSearch)) {
        StartingLoopNum = SingleLoopSearch;
        EndingLoopNum = SingleLoopSearch;
    }

    for (LoopCtr = StartingLoopNum; LoopCtr <= EndingLoopNum; ++LoopCtr) {
        auto &this_loop(state.dataPlnt->PlantLoop(LoopCtr));
        for (LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr) {
            auto &this_loop_side(this_loop.LoopSide(LoopSideCtr));
            for (BranchCtr = 1; BranchCtr <= this_loop_side.TotalBranches; ++BranchCtr) {
                auto &this_branch(this_loop_side.Branch(BranchCtr));
                for (CompCtr = 1; CompCtr <= this_branch.TotalComponents; ++CompCtr) {
                    auto &this_component(this_branch.Comp(CompCtr));
                    if (this_component.TypeOf_Num == CompType) {
                        if (UtilityRoutines::SameString(CompName, this_component.Name)) {
                            FoundCompName = true;
                            if (present(InletNodeNumber)) {
                                if (InletNodeNumber > 0) {
                                    // check if inlet nodes agree
                                    if (InletNodeNumber == this_component.NodeNumIn) {
                                        FoundComponent = true;
                                        ++FoundCount;
                                        LoopNum = LoopCtr;
                                        LoopSideNum = LoopSideCtr;
                                        BranchNum = BranchCtr;
                                        CompNum = CompCtr;
                                    }
                                }
                            } else {
                                FoundComponent = true;
                                ++FoundCount;
                                LoopNum = LoopCtr;
                                LoopSideNum = LoopSideCtr;
                                BranchNum = BranchCtr;
                                CompNum = CompCtr;
                            }
                            if (present(LowLimitTemp)) {
                                this_component.MinOutletTemp = LowLimitTemp;
                            }
                            if (present(HighLimitTemp)) {
                                this_component.MaxOutletTemp = HighLimitTemp;
                            }
                        }
                    }
                }
            }
        }
    }

    if (!FoundComponent) {
        if (CompType >= 1 && CompType <= DataPlant::NumSimPlantEquipTypes) {
            if (!present(SingleLoopSearch)) {
                ShowSevereError(state,
                                "Plant Component " + DataPlant::ccSimPlantEquipTypes(CompType) + " called \"" + std::string{CompName} +
                                    "\" was not found on any plant loops.");
                AuditBranches(state, true, DataPlant::ccSimPlantEquipTypes(CompType), CompName);
            } else {
                ShowSevereError(state,
                                "Plant Component " + DataPlant::ccSimPlantEquipTypes(CompType) + " called \"" + std::string{CompName} +
                                    "\" was not found on plant loop=\"" + state.dataPlnt->PlantLoop(SingleLoopSearch).Name + "\".");
            }
            if (present(InletNodeNumber)) {
                if (FoundCompName) {
                    ShowContinueError(state, "Looking for matching inlet Node=\"" + state.dataLoopNodes->NodeID(InletNodeNumber) + "\".");
                }
            }
            if (present(SingleLoopSearch)) {
                ShowContinueError(state, "Look at Operation Scheme=\"" + state.dataPlnt->PlantLoop(SingleLoopSearch).OperationScheme + "\".");
                ShowContinueError(state, "Look at Branches and Components on the Loop.");
                ShowBranchesOnLoop(state, SingleLoopSearch);
            }
            errFlag = true;
        } else {
            ShowSevereError(state, format("ScanPlantLoopsForObject: Invalid CompType passed [{}], Name={}", CompType, CompName));
            ShowContinueError(state, format("Valid CompTypes are in the range [1 - {}].", DataPlant::NumSimPlantEquipTypes));
            ShowFatalError(state, "Previous error causes program termination");
        }
    }

    if (present(CountMatchPlantLoops)) {
        CountMatchPlantLoops = FoundCount;
    }
}

void ScanPlantLoopsForNodeNum(EnergyPlusData &state,
                              std::string_view const CallerName, // really used for error messages
                              int const NodeNum,                 // index in Node structure of node to be scanned
                              int &LoopNum,                      // return value for plant loop
                              int &LoopSideNum,                  // return value for plant loop side
                              int &BranchNum,
                              Optional_int CompNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Feb. 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Get routine to return plant loop index and plant loop side
    // based on node number.  for one time init routines only.

    // METHODOLOGY EMPLOYED:
    // Loop thru plant data structure and find matching node.

    int LoopCtr;
    int LoopSideCtr;
    int BranchCtr;
    int CompCtr;
    bool FoundNode;
    int inFoundCount;
    int outFoundCount;

    inFoundCount = 0;
    outFoundCount = 0;
    if (present(CompNum)) {
        CompNum = 0;
    }
    FoundNode = false;

    for (LoopCtr = 1; LoopCtr <= state.dataPlnt->TotNumLoops; ++LoopCtr) {
        auto &this_loop(state.dataPlnt->PlantLoop(LoopCtr));
        for (LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr) {
            auto &this_loop_side(this_loop.LoopSide(LoopSideCtr));
            for (BranchCtr = 1; BranchCtr <= this_loop_side.TotalBranches; ++BranchCtr) {
                auto &this_branch(this_loop_side.Branch(BranchCtr));
                for (CompCtr = 1; CompCtr <= this_branch.TotalComponents; ++CompCtr) {
                    auto &this_comp(this_branch.Comp(CompCtr));
                    if (NodeNum == this_comp.NodeNumIn) {
                        FoundNode = true;
                        ++inFoundCount;
                        LoopNum = LoopCtr;
                        LoopSideNum = LoopSideCtr;
                        BranchNum = BranchCtr;
                        if (present(CompNum)) {
                            CompNum = CompCtr;
                        }
                    }

                    if (NodeNum == this_comp.NodeNumOut) {
                        ++outFoundCount;
                        LoopNum = LoopCtr;
                        LoopSideNum = LoopSideCtr;
                        BranchNum = BranchCtr;
                    }
                }
            }
        }
    }

    if (!FoundNode) {
        ShowSevereError(state, "ScanPlantLoopsForNodeNum: Plant Node was not found as inlet node (for component) on any plant loops");
        ShowContinueError(state, "Node Name=\"" + state.dataLoopNodes->NodeID(NodeNum) + "\"");
        if (!state.dataGlobal->DoingSizing) {
            ShowContinueError(state, format("called by {}", CallerName));
        } else {
            ShowContinueError(state, format("during sizing: called by {}", CallerName));
        }
        if (outFoundCount > 0) ShowContinueError(state, format("Node was found as outlet node (for component) {} time(s).", outFoundCount));
        ShowContinueError(state, "Possible error in Branch inputs.  For more information, look for other error messages related to this node name.");
        // fatal?
    }
}

bool AnyPlantLoopSidesNeedSim(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   November 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na
    // PURPOSE OF THIS FUNCTION:
    // This subroutine scans the plant LoopSide simflags and returns if any of them are still true

    // Return value
    bool AnyPlantLoopSidesNeedSim;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int LoopCtr;
    int LoopSideCtr;

    // Assume that there aren't any
    AnyPlantLoopSidesNeedSim = false;

    // Then check if there are any
    for (LoopCtr = 1; LoopCtr <= state.dataPlnt->TotNumLoops; ++LoopCtr) {
        for (LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr) {
            if (state.dataPlnt->PlantLoop(LoopCtr).LoopSide(LoopSideCtr).SimLoopSideNeeded) {
                AnyPlantLoopSidesNeedSim = true;
                return AnyPlantLoopSidesNeedSim;
            }
        }
    }

    return AnyPlantLoopSidesNeedSim;
}

void SetAllPlantSimFlagsToValue(EnergyPlusData &state, bool const Value)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Edwin Lee
    //       DATE WRITTEN   November 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  B. Griffith Feb 2009
    // PURPOSE OF THIS SUBROUTINE:
    // Quickly sets all sim flags of a certain type (loop type/side) to a value

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopCtr;

    // Loop over all loops
    for (LoopCtr = 1; LoopCtr <= state.dataPlnt->TotNumLoops; ++LoopCtr) {
        auto &this_loop(state.dataPlnt->PlantLoop(LoopCtr));
        this_loop.LoopSide(DataPlant::DemandSide).SimLoopSideNeeded = Value;
        this_loop.LoopSide(DataPlant::SupplySide).SimLoopSideNeeded = Value;
    }
}

void ShowBranchesOnLoop(EnergyPlusData &state, int const LoopNum) // Loop number of loop
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine will display (with continue error messages) the branch/component
    // structure of the given loop.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string DemandSupply;
    int LSN; // LoopSide counter
    int BrN; // Branch counter
    int CpN; // Component (on branch) counter

    for (LSN = DataPlant::DemandSide; LSN <= DataPlant::SupplySide; ++LSN) {
        if (LSN == DataPlant::DemandSide) {
            DemandSupply = "Demand";
        } else if (LSN == DataPlant::SupplySide) {
            DemandSupply = "Supply";
        } else {
            DemandSupply = "Unknown";
        }
        ShowContinueError(state, DemandSupply + " Branches:");
        for (BrN = 1; BrN <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).TotalBranches; ++BrN) {
            ShowContinueError(state, "  " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).Name);
            ShowContinueError(state, "    Components on Branch:");
            for (CpN = 1; CpN <= state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).TotalComponents; ++CpN) {
                ShowContinueError(state,
                                  "      " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).Comp(CpN).TypeOf + ':' +
                                      state.dataPlnt->PlantLoop(LoopNum).LoopSide(LSN).Branch(BrN).Comp(CpN).Name);
            }
        }
    }
}

int MyPlantSizingIndex(EnergyPlusData &state,
                       std::string const &CompType,           // component description
                       std::string_view CompName,             // user name of component
                       int const NodeNumIn,                   // component water inlet node
                       [[maybe_unused]] int const NodeNumOut, // component water outlet node
                       bool &ErrorsFound,                     // set to true if there's an error, unchanged otherwise
                       Optional_bool_const SupressErrors      // used for WSHP's where condenser loop may not be on a plant loop
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   July 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Identify the correct Plant Sizing object for demand-side components such as heating and
    // cooling coils.

    // METHODOLOGY EMPLOYED:
    // This function searches all plant loops for a component whose input and
    // output nodes match the desired input & output nodes. This plant loop index is then used
    // to search the Plant Sizing array for the matching Plant Sizing object.

    // Using/Aliasing
    using DataSizing::PlantSizingData;

    // Return value
    int MyPltSizNum; // returned plant sizing index

    int MyPltLoopNum;
    int PlantLoopNum;
    int DummyLoopSideNum;
    int DummyBranchNum;
    bool PrintErrorFlag;

    MyPltLoopNum = 0;
    MyPltSizNum = 0;
    if (present(SupressErrors)) {
        PrintErrorFlag = SupressErrors;
    } else {
        PrintErrorFlag = true;
    }

    ScanPlantLoopsForNodeNum(state, "MyPlantSizingIndex", NodeNumIn, PlantLoopNum, DummyLoopSideNum, DummyBranchNum);

    if (PlantLoopNum > 0) {
        MyPltLoopNum = PlantLoopNum;
    } else {
        MyPltLoopNum = 0;
    }

    if (MyPltLoopNum > 0) {
        if (state.dataSize->NumPltSizInput > 0) {
            MyPltSizNum = UtilityRoutines::FindItemInList(
                state.dataPlnt->PlantLoop(MyPltLoopNum).Name, state.dataSize->PlantSizData, &PlantSizingData::PlantLoopName);
        }
        if (MyPltSizNum == 0) {
            if (PrintErrorFlag) {
                ShowSevereError(state,
                                "MyPlantSizingIndex: Could not find " + state.dataPlnt->PlantLoop(MyPltLoopNum).Name + " in Sizing:Plant objects.");
                ShowContinueError(state, "...reference Component Type=\"" + CompType + "\", Name=\"" + std::string{CompName} + "\".");
            }
            ErrorsFound = true;
        }
    } else {
        if (PrintErrorFlag) {
            ShowWarningError(state, "MyPlantSizingIndex: Could not find " + CompType + " with name " + std::string{CompName} + " on any plant loop");
        }
        ErrorsFound = true;
    }

    return MyPltSizNum;
}

bool verifyTwoNodeNumsOnSamePlantLoop(EnergyPlusData &state, int const nodeIndexA, int const nodeIndexB)
{
    // this function simply searches across plant loops looking for node numbers
    // it returns true if the two nodes are found to be on the same loop
    // it returns false otherwise
    // because this is a nested loop, there's no reason it should be called except in one-time fashion
    int matchedIndexA = 0;
    int matchedIndexB = 0;
    for (int loopNum = 1; loopNum <= state.dataPlnt->TotNumLoops; loopNum++) {
        for (auto &loopSide : state.dataPlnt->PlantLoop(loopNum).LoopSide) {
            for (auto &branch : loopSide.Branch) {
                for (auto &comp : branch.Comp) {
                    if (comp.NodeNumIn == nodeIndexA || comp.NodeNumOut == nodeIndexA) {
                        matchedIndexA = loopNum;
                    }
                    if (comp.NodeNumIn == nodeIndexB || comp.NodeNumOut == nodeIndexB) {
                        matchedIndexB = loopNum;
                    }
                }
            }
        }
    }
    return (matchedIndexA == matchedIndexB) && (matchedIndexA != 0); // only return true if both are equal and non-zero
}

} // namespace EnergyPlus::PlantUtilities
