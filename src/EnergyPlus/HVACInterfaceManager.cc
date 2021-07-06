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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::HVACInterfaceManager {

// MODULE INFORMATION:
//       AUTHOR         Rick Strand
//       DATE WRITTEN   October 1998
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module contains one or more routines for checking the convergence
// of the various HVAC loops and passing information across interface
// boundaries.

// METHODOLOGY EMPLOYED:
// The upper level HVAC managers call the routine(s) contained in this
// module as a last step.  The node information is passed across the
// interface boundary and the logical flag is set if the nodes across
// from each other are not within tolerance.

void UpdateHVACInterface(EnergyPlusData &state,
                         int const AirLoopNum, // airloop number for which air loop this is
                         DataConvergParams::iCalledFrom const CalledFrom,
                         int const OutletNode,    // Node number for the outlet of the side of the loop just simulated
                         int const InletNode,     // Node number for the inlet of the side that needs the outlet node data
                         bool &OutOfToleranceFlag // True when the other side of the loop need to be (re)simulated
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   October 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages any generic HVAC loop interface.

    // METHODOLOGY EMPLOYED:
    // This is a simple "forward" interface where all of the properties
    // from the outlet of one side of the loop get transferred directly
    // to the inlet node of the corresponding other side of the loop.

    auto &TmpRealARR = state.dataHVACInterfaceMgr->TmpRealARR;
    Real64 DeltaEnergy;

    if ((CalledFrom == DataConvergParams::iCalledFrom::AirSystemDemandSide) && (OutletNode == 0)) {
        // Air loop has no return path - only check mass flow and then set return inlet node mass flow to sum of demand side inlet nodes
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(1) = false;

        Real64 totDemandSideMassFlow = 0.0;
        Real64 totDemandSideMinAvail = 0.0;
        Real64 totDemandSideMaxAvail = 0.0;
        for (int demIn = 1; demIn <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++demIn) {
            int demInNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(demIn);
            totDemandSideMassFlow += state.dataLoopNodes->Node(demInNode).MassFlowRate;
            totDemandSideMinAvail += state.dataLoopNodes->Node(demInNode).MassFlowRateMinAvail;
            totDemandSideMaxAvail += state.dataLoopNodes->Node(demInNode).MassFlowRateMaxAvail;
        }
        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue(1) =
            std::abs(totDemandSideMassFlow - state.dataLoopNodes->Node(InletNode).MassFlowRate);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue(1) > DataConvergParams::HVACFlowRateToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        state.dataLoopNodes->Node(InletNode).MassFlowRate = totDemandSideMassFlow;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = totDemandSideMinAvail;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = totDemandSideMaxAvail;
        return;
    }

    // Calculate the approximate energy difference across interface for comparison
    DeltaEnergy =
        DataConvergParams::HVACCpApprox * ((state.dataLoopNodes->Node(OutletNode).MassFlowRate * state.dataLoopNodes->Node(OutletNode).Temp) -
                                           (state.dataLoopNodes->Node(InletNode).MassFlowRate * state.dataLoopNodes->Node(InletNode).Temp));

    if ((CalledFrom == DataConvergParams::iCalledFrom::AirSystemDemandSide) && (OutletNode > 0)) {

        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(1) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(1) = false;

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).MassFlowRate - state.dataLoopNodes->Node(InletNode).MassFlowRate);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowDemandToSupplyTolValue(1) > DataConvergParams::HVACFlowRateToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumDemandToSupplyTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).HumRat - state.dataLoopNodes->Node(InletNode).HumRat);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumDemandToSupplyTolValue(1) > DataConvergParams::HVACHumRatToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempDemandToSupplyTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Temp - state.dataLoopNodes->Node(InletNode).Temp);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempDemandToSupplyTolValue(1) > DataConvergParams::HVACTemperatureToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyDemandToSupplyTolValue(1) = std::abs(DeltaEnergy);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (std::abs(DeltaEnergy) > DataConvergParams::HVACEnergyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyDemandToSupplyTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyDemandToSupplyTolValue(1) > DataConvergParams::HVACEnthalpyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureDemandToSupplyTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureDemandToSupplyTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Press - state.dataLoopNodes->Node(InletNode).Press);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureDemandToSupplyTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureDemandToSupplyTolValue(1) > DataConvergParams::HVACPressToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(1) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

    } else if (CalledFrom == DataConvergParams::iCalledFrom::AirSystemSupplySideDeck1) {

        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(2) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(2) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(2) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(2) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(2) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(2) = false;

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck1ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).MassFlowRate - state.dataLoopNodes->Node(InletNode).MassFlowRate);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck1ToDemandTolValue(1) > DataConvergParams::HVACFlowRateToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck1ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).HumRat - state.dataLoopNodes->Node(InletNode).HumRat);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck1ToDemandTolValue(1) > DataConvergParams::HVACHumRatToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck1ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Temp - state.dataLoopNodes->Node(InletNode).Temp);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck1ToDemandTolValue(1) >
            DataConvergParams::HVACTemperatureToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck1ToDemandTolValue(1) = DeltaEnergy;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (std::abs(DeltaEnergy) > DataConvergParams::HVACEnergyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck1ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum)
            .HVACEnthalpySupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck1ToDemandTolValue(1) >
            DataConvergParams::HVACEnthalpyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureSupplyDeck1ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureSupplyDeck1ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Press - state.dataLoopNodes->Node(InletNode).Press);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum)
            .HVACPressureSupplyDeck1ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureSupplyDeck1ToDemandTolValue(1) > DataConvergParams::HVACPressToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(2) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

    } else if (CalledFrom == DataConvergParams::iCalledFrom::AirSystemSupplySideDeck2) {

        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(3) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(3) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(3) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(3) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(3) = false;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(3) = false;

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck2ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).MassFlowRate - state.dataLoopNodes->Node(InletNode).MassFlowRate);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACFlowSupplyDeck2ToDemandTolValue(1) > DataConvergParams::HVACFlowRateToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACMassFlowNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck2ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).HumRat - state.dataLoopNodes->Node(InletNode).HumRat);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumSupplyDeck2ToDemandTolValue(1) > DataConvergParams::HVACHumRatToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACHumRatNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck2ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Temp - state.dataLoopNodes->Node(InletNode).Temp);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempSupplyDeck2ToDemandTolValue(1) >
            DataConvergParams::HVACTemperatureToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACTempNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck2ToDemandTolValue(1) = DeltaEnergy;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergySupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (std::abs(DeltaEnergy) > DataConvergParams::HVACEnergyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnergyNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck2ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum)
            .HVACEnthalpySupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpySupplyDeck2ToDemandTolValue(1) >
            DataConvergParams::HVACEnthalpyToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACEnthalpyNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }

        TmpRealARR = state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressueSupplyDeck2ToDemandTolValue;
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressueSupplyDeck2ToDemandTolValue(1) =
            std::abs(state.dataLoopNodes->Node(OutletNode).Press - state.dataLoopNodes->Node(InletNode).Press);
        state.dataConvergeParams->AirLoopConvergence(AirLoopNum)
            .HVACPressueSupplyDeck2ToDemandTolValue({2, DataConvergParams::ConvergLogStackDepth}) =
            TmpRealARR({1, DataConvergParams::ConvergLogStackDepth - 1});
        if (state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressueSupplyDeck2ToDemandTolValue(1) > DataConvergParams::HVACPressToler) {
            state.dataConvergeParams->AirLoopConvergence(AirLoopNum).HVACPressureNotConverged(3) = true;
            OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
        }
    }

    // Always update the new inlet conditions
    state.dataLoopNodes->Node(InletNode).Temp = state.dataLoopNodes->Node(OutletNode).Temp;
    state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
    state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail;
    state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail;
    state.dataLoopNodes->Node(InletNode).Quality = state.dataLoopNodes->Node(OutletNode).Quality;
    state.dataLoopNodes->Node(InletNode).Press = state.dataLoopNodes->Node(OutletNode).Press;
    state.dataLoopNodes->Node(InletNode).Enthalpy = state.dataLoopNodes->Node(OutletNode).Enthalpy;
    state.dataLoopNodes->Node(InletNode).HumRat = state.dataLoopNodes->Node(OutletNode).HumRat;

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataLoopNodes->Node(InletNode).CO2 = state.dataLoopNodes->Node(OutletNode).CO2;
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataLoopNodes->Node(InletNode).GenContam = state.dataLoopNodes->Node(OutletNode).GenContam;
    }
}

//***************

// In-Place Right Shift by 1 of Array Elements
void rshift1(Array1D<Real64> &a)
{
    assert(a.size_bounded());
    for (int i = a.u(), e = a.l(); i > e; --i) {
        a(i) = a(i - 1);
    }
}

void UpdatePlantLoopInterface(EnergyPlusData &state,
                              int const LoopNum,                // The 'inlet/outlet node' loop number
                              int const ThisLoopSideNum,        // The 'outlet node' LoopSide number
                              int const ThisLoopSideOutletNode, // Node number for the inlet of the side that needs the outlet node data
                              int const OtherLoopSideInletNode, // Node number for the outlet of the side of the loop just simulated
                              bool &OutOfToleranceFlag,         // True when the other side of the loop need to be (re)simulated
                              DataPlant::iCommonPipeType const CommonPipeType)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   October 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  Brent Griffith, Sept. 2010
    //       RE-ENGINEERED  Dan Fisher,     Sept. 2010

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages any generic HVAC loop interface.

    // METHODOLOGY EMPLOYED:
    // This is a simple "forward" interface where all of the properties
    // from the outlet of one side of the loop get transfered
    // to the inlet node of the corresponding other side of the loop.
    // Temperatures are 'lagged' by loop capacitance (i.e. a 'tank')
    // between the outlet and inlet nodes.
    // the update from the demand side to the supply side always triggers
    // resimulation of the supply side if any state variable (or energy) is
    // out of tolerance.  Remsimulation of the demand side is only triggered if
    // flow or energy are out of tolerance.  This in effect checks flow and
    // ~.25C temperature difference.

    // Using/Aliasing
    using DataPlant::DemandSide;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("UpdatePlantLoopInterface");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OldTankOutletTemp;
    Real64 OldOtherLoopSideInletMdot;
    Real64 TankOutletTemp;
    Real64 Cp;
    Real64 MixedOutletTemp;
    int ThisLoopSideInletNode;

    auto &convergence(state.dataConvergeParams->PlantConvergence(LoopNum));

    // reset out of tolerance flags
    convergence.PlantMassFlowNotConverged = false;
    convergence.PlantTempNotConverged = false;

    // set the LoopSide inlet node
    ThisLoopSideInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSideNum).NodeNumIn;

    // save the inlet node temp for DeltaEnergy check
    OldOtherLoopSideInletMdot = state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate;
    OldTankOutletTemp = state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp;

    // calculate the specific heat
    Cp = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(LoopNum).FluidName, OldTankOutletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);

    // update the enthalpy
    state.dataLoopNodes->Node(OtherLoopSideInletNode).Enthalpy = Cp * state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp;

    // update the temperatures and flow rates
    auto &flow_demand_to_supply_tol(convergence.PlantFlowDemandToSupplyTolValue);
    auto &flow_supply_to_demand_tol(convergence.PlantFlowSupplyToDemandTolValue);
    if (CommonPipeType == DataPlant::iCommonPipeType::Single || CommonPipeType == DataPlant::iCommonPipeType::TwoWay) {
        // update the temperature
        UpdateCommonPipe(state, LoopNum, ThisLoopSideNum, CommonPipeType, MixedOutletTemp);
        state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp = MixedOutletTemp;
        TankOutletTemp = MixedOutletTemp;
        if (ThisLoopSideNum == DataPlant::DemandSide) {
            rshift1(flow_demand_to_supply_tol);
            flow_demand_to_supply_tol(1) = std::abs(OldOtherLoopSideInletMdot - state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate);
            if (flow_demand_to_supply_tol(1) > DataConvergParams::PlantFlowRateToler) {
                convergence.PlantMassFlowNotConverged = true;
            }
        } else {
            rshift1(flow_supply_to_demand_tol);
            flow_supply_to_demand_tol(1) = std::abs(OldOtherLoopSideInletMdot - state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate);
            if (flow_supply_to_demand_tol(1) > DataConvergParams::PlantFlowRateToler) {
                convergence.PlantMassFlowNotConverged = true;
            }
        }
        // Set the flow rate.  Continuity requires that the flow rates at the half loop inlet and outlet match
        state.dataLoopNodes->Node(ThisLoopSideInletNode).MassFlowRate = state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRate;
        // Update this LoopSide inlet node Min/MaxAvail to this LoopSide outlet node Min/MaxAvail
        state.dataLoopNodes->Node(ThisLoopSideInletNode).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(ThisLoopSideInletNode).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRateMaxAvail;

    } else { // no common pipe
        UpdateHalfLoopInletTemp(state, LoopNum, ThisLoopSideNum, TankOutletTemp);
        // update the temperature
        state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp = TankOutletTemp;
        // Set the flow tolerance array
        if (ThisLoopSideNum == DataPlant::DemandSide) {
            rshift1(flow_demand_to_supply_tol);
            flow_demand_to_supply_tol(1) = std::abs(state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRate -
                                                    state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate);
            if (flow_demand_to_supply_tol(1) > DataConvergParams::PlantFlowRateToler) {
                convergence.PlantMassFlowNotConverged = true;
            }
        } else {
            rshift1(flow_supply_to_demand_tol);
            flow_supply_to_demand_tol(1) = std::abs(state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRate -
                                                    state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate);
            if (flow_supply_to_demand_tol(1) > DataConvergParams::PlantFlowRateToler) {
                convergence.PlantMassFlowNotConverged = true;
            }
        }
        //    PlantFlowTolValue(PlantQuePtr)  = ABS(Node(ThisLoopSideOutletNode)%MassFlowRate-Node(OtherLoopSideInletNode)%MassFlowRate)
        // Set the flow rate
        state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRate = state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRate;
        // update the MIN/MAX available flow rates
        state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRateMinAvail =
            state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(OtherLoopSideInletNode).MassFlowRateMaxAvail =
            state.dataLoopNodes->Node(ThisLoopSideOutletNode).MassFlowRateMaxAvail;
        // update Quality.  Note: This update assumes that STEAM cannot be used with common pipes.
        state.dataLoopNodes->Node(OtherLoopSideInletNode).Quality = state.dataLoopNodes->Node(ThisLoopSideOutletNode).Quality;
        // pressure update  Note: This update assumes that PRESSURE SIMULATION cannot be used with common pipes.
        if (state.dataPlnt->PlantLoop(LoopNum).HasPressureComponents) {
            // Don't update pressure, let the pressure simulation handle pressures
        } else {
            // Do update pressure!
            state.dataLoopNodes->Node(OtherLoopSideInletNode).Press = state.dataLoopNodes->Node(ThisLoopSideOutletNode).Press;
        }
    }

    // temperature
    if (ThisLoopSideNum == DataPlant::DemandSide) {
        auto &temp_demand_to_supply_tol(convergence.PlantTempDemandToSupplyTolValue);
        rshift1(temp_demand_to_supply_tol);
        temp_demand_to_supply_tol(1) = std::abs(OldTankOutletTemp - state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp);
        if (temp_demand_to_supply_tol(1) > DataConvergParams::PlantTemperatureToler) {
            convergence.PlantTempNotConverged = true;
        }
    } else {
        auto &temp_supply_to_demand_tol(convergence.PlantTempSupplyToDemandTolValue);
        rshift1(temp_supply_to_demand_tol);
        temp_supply_to_demand_tol(1) = std::abs(OldTankOutletTemp - state.dataLoopNodes->Node(OtherLoopSideInletNode).Temp);
        if (temp_supply_to_demand_tol(1) > DataConvergParams::PlantTemperatureToler) {
            convergence.PlantTempNotConverged = true;
        }
    }

    // Set out of tolerance flags
    if (ThisLoopSideNum == DataPlant::DemandSide) {
        if (convergence.PlantMassFlowNotConverged || convergence.PlantTempNotConverged) {
            OutOfToleranceFlag = true;
        }
    } else {
        if (convergence.PlantMassFlowNotConverged) {
            OutOfToleranceFlag = true;
        }
    }
}

//***************

void UpdateHalfLoopInletTemp(EnergyPlusData &state, int const LoopNum, int const TankInletLoopSide, Real64 &TankOutletTemp)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   September 2001
    //       MODIFIED       Simon Rees, July 2007
    //                      Brent Griffith, Feb. 2010, add LoopNum arg
    //       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
    //                                           add pump heat from other loop
    //                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the new loop side inlet temperature
    // based on the previous temperature of the mixed tank, mass flow rate and the new
    // outlet temperature on the supply side.  The temperature does not
    // pass directly across because the loop has some capacitance. It is
    // called separately but used for both supply-to-demand, and demand-to-supply

    // METHODOLOGY EMPLOYED:
    // This uses a analytical solution for changes in the
    // fluid loop temperature.  The user defines some volume of fluid
    // for the loop which gets converted to a fixed amount of mass.
    // The loop side inlet node is modeled as the outlet of a fully mixed
    // tank. Note that this routine is called repeatedly to re calculate
    // loop capacitance based on current plant conditions

    // REFERENCES:
    // na

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const FracTotLoopMass(0.5); // Fraction of total loop mass assigned to the half loop
    static std::string const RoutineName("UpdateHalfLoopInletTemp");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int TankOutletLoopSide;    // inlet loopsidenumber
    int TankInletNode;         // inlet loop side outlet node
    int TankOutletNode;        // inlet loop side outlet node
    Real64 TankInletTemp;      // temporary variable
    Real64 LastTankOutletTemp; // temporary variable
    Real64 Cp;                 // specific heat
    Real64 TimeElapsed;        // temporary value based on current clock time during simulation, fractional hours

    Real64 TimeStepSeconds;
    Real64 MassFlowRate;
    Real64 PumpHeat;
    Real64 ThisTankMass;
    Real64 TankFinalTemp;
    Real64 TankAverageTemp;

    // find tank inlet and outlet nodes
    TankOutletLoopSide = 3 - TankInletLoopSide;
    TankInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankInletLoopSide).NodeNumOut;
    TankOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).NodeNumIn;

    TankInletTemp = state.dataLoopNodes->Node(TankInletNode).Temp;

    // This needs to be based on time to deal with system downstepping and repeated timesteps
    TimeElapsed = (state.dataGlobal->HourOfDay - 1) + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + SysTimeElapsed;
    if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TimeElapsed != TimeElapsed) {
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LastTempInterfaceTankOutlet =
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TempInterfaceTankOutlet;
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TimeElapsed = TimeElapsed;
    }

    LastTankOutletTemp = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LastTempInterfaceTankOutlet;

    // calculate the specific heat for the capacitance calculation
    Cp = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(LoopNum).FluidName, LastTankOutletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);
    // set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

    // calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
    //--half of loop mass.  The other half is accounted for at the other half loop interface
    //--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
    //   Pump heat for a dual setpoint loop is added to each loop side inlet
    //  The previous tank temperature value is used to prevent accumulation of pump heat during iterations while recalculating
    // tank conditions each call.
    // Analytical solution for ODE, formulated for both final tank temp and average tank temp.

    TimeStepSeconds = TimeStepSys * DataGlobalConstants::SecInHour;
    MassFlowRate = state.dataLoopNodes->Node(TankInletNode).MassFlowRate;
    PumpHeat = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TotalPumpHeat;
    ThisTankMass = FracTotLoopMass * state.dataPlnt->PlantLoop(LoopNum).Mass;

    if (ThisTankMass <= 0.0) { // no mass, no plant loop volume
        if (MassFlowRate > 0.0) {
            TankFinalTemp = TankInletTemp + PumpHeat / (MassFlowRate * Cp);
            TankAverageTemp = (TankFinalTemp + LastTankOutletTemp) / 2.0;
        } else {
            TankFinalTemp = LastTankOutletTemp;
            TankAverageTemp = LastTankOutletTemp;
        }

    } else { // tank has mass
        if (MassFlowRate > 0.0) {
            Real64 const mdotCp = MassFlowRate * Cp;
            Real64 const mdotCpTempIn = mdotCp * TankInletTemp;
            Real64 const tankMassCp = ThisTankMass * Cp;
            Real64 const ExponentTerm = mdotCp / tankMassCp * TimeStepSeconds;
            if (ExponentTerm >= 700.0) {
                TankFinalTemp = (mdotCp * TankInletTemp + PumpHeat) / mdotCp;

                TankAverageTemp = (tankMassCp / mdotCp * (LastTankOutletTemp - (mdotCpTempIn + PumpHeat) / mdotCp) / TimeStepSeconds +
                                   (mdotCpTempIn + PumpHeat) / mdotCp);
            } else {
                TankFinalTemp = (LastTankOutletTemp - (mdotCpTempIn + PumpHeat) / mdotCp) * std::exp(-ExponentTerm) +
                                (mdotCpTempIn + PumpHeat) / (MassFlowRate * Cp);

                TankAverageTemp = (tankMassCp / mdotCp * (LastTankOutletTemp - (mdotCpTempIn + PumpHeat) / mdotCp) * (1.0 - std::exp(-ExponentTerm)) /
                                       TimeStepSeconds +
                                   (mdotCpTempIn + PumpHeat) / mdotCp);
            }
        } else {
            TankFinalTemp = PumpHeat / (ThisTankMass * Cp) * TimeStepSeconds + LastTankOutletTemp;
            TankAverageTemp = (TankFinalTemp + LastTankOutletTemp) / 2.0;
        }
    }

    // update last tank outlet temperature
    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TempInterfaceTankOutlet = TankFinalTemp;

    // update heat transport and heat storage rates
    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LoopSideInlet_MdotCpDeltaT =
        (TankInletTemp - TankAverageTemp) * Cp * MassFlowRate;
    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LoopSideInlet_McpDTdt =
        (ThisTankMass * Cp * (TankFinalTemp - LastTankOutletTemp)) / TimeStepSeconds;

    // update report variable
    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LoopSideInlet_TankTemp = TankAverageTemp;

    TankOutletTemp = TankAverageTemp;
}

void UpdateCommonPipe(
    EnergyPlusData &state, int const LoopNum, int const TankInletLoopSide, DataPlant::iCommonPipeType const CommonPipeType, Real64 &MixedOutletTemp)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   September 2001
    //       MODIFIED       Simon Rees, July 2007
    //                      Brent Griffith, Feb. 2010, add LoopNum arg
    //       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
    //                                           add pump heat from other loop
    //                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the new loop side inlet temperature
    // based on the previous temperature of the mixed tank, mass flow rate and the new
    // outlet temperature on the supply side.  The temperature does not
    // pass directly across because the loop has some capacitance. It is
    // called separately but used for both supply-to-demand, and demand-to-supply

    // METHODOLOGY EMPLOYED:
    // This uses a analytical solution for changes in the
    // fluid loop temperature.  The user defines some volume of fluid
    // for the loop which gets converted to a fixed amount of mass.
    // The loop side inlet node is modeled as the outlet of a fully mixed
    // tank. Note that this routine is called repeatedly to re calculate
    // loop capacitance based on current plant conditions

    // REFERENCES:
    // na

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using DataPlant::DemandSide;
    using FluidProperties::GetSpecificHeatGlycol;

    // Locals
    // SUBROUTINE ARGUMENTS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("UpdateCommonPipe");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int TankOutletLoopSide;    // inlet loopsidenumber
    int TankInletNode;         // inlet loop side outlet node
    int TankOutletNode;        // inlet loop side outlet node
    Real64 TankInletTemp;      // temporary variable
    Real64 LastTankOutletTemp; // temporary variable
    Real64 Cp;                 // specific heat
    Real64 TimeElapsed;        // temporary value based on current clock time during simulation, fractional hours

    Real64 FracTotLoopMass; // Fraction of total loop mass assigned to the half loop
    Real64 TimeStepSeconds;
    Real64 MassFlowRate;
    Real64 PumpHeat;
    Real64 ThisTankMass;
    Real64 TankFinalTemp;
    Real64 TankAverageTemp;

    // find tank inlet and outlet nodes
    TankOutletLoopSide = 3 - TankInletLoopSide;
    TankInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankInletLoopSide).NodeNumOut;
    TankOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).NodeNumIn;

    TankInletTemp = state.dataLoopNodes->Node(TankInletNode).Temp;

    if (TankInletLoopSide == DataPlant::DemandSide) {
        // for common pipe loops, assume 75% of plant loop volume is on the demand side
        FracTotLoopMass = 0.25;
    } else {
        FracTotLoopMass = 0.75;
    }

    // This needs to be based on time to deal with system downstepping and repeated timesteps
    TimeElapsed = (state.dataGlobal->HourOfDay - 1) + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + SysTimeElapsed;
    if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TimeElapsed != TimeElapsed) {
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LastTempInterfaceTankOutlet =
            state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TempInterfaceTankOutlet;
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TimeElapsed = TimeElapsed;
    }

    LastTankOutletTemp = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LastTempInterfaceTankOutlet;

    // calculate the specific heat for the capacitance calculation
    Cp = GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(LoopNum).FluidName, LastTankOutletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);

    // set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

    // calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
    //--half of loop mass.  The other half is accounted for at the other half loop interface
    //--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
    // Pump heat for a dual setpoint loop is added to each loop side inlet
    // The previous inlet side temp,'ThisLoopSideTankOutletTemp' is used to prevent accumulation of pump heat during iterations.
    // The placement of the 'tank' for common pipes is *after* the outlet node and *before* the flow split or flow mixing.
    // This requires no logical check in the code since for purposes of temperature calculations, it is identical to the
    // no common pipe case.
    // calculation is separated because for common pipe, a different split for mass fraction is applied
    // The pump heat source is swapped around here compared to no common pipe (so pump heat sort stays on its own side).
    TimeStepSeconds = TimeStepSys * DataGlobalConstants::SecInHour;
    MassFlowRate = state.dataLoopNodes->Node(TankInletNode).MassFlowRate;
    PumpHeat = state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankInletLoopSide).TotalPumpHeat;
    ThisTankMass = FracTotLoopMass * state.dataPlnt->PlantLoop(LoopNum).Mass;

    if (ThisTankMass <= 0.0) { // no mass, no plant loop volume
        if (MassFlowRate > 0.0) {
            TankFinalTemp = TankInletTemp + PumpHeat / (MassFlowRate * Cp);
            TankAverageTemp = (TankFinalTemp + LastTankOutletTemp) / 2.0;
        } else {
            TankFinalTemp = LastTankOutletTemp;
            TankAverageTemp = LastTankOutletTemp;
        }

    } else { // tank has mass
        if (MassFlowRate > 0.0) {
            TankFinalTemp = (LastTankOutletTemp - (MassFlowRate * Cp * TankInletTemp + PumpHeat) / (MassFlowRate * Cp)) *
                                std::exp(-(MassFlowRate * Cp) / (ThisTankMass * Cp) * TimeStepSeconds) +
                            (MassFlowRate * Cp * TankInletTemp + PumpHeat) / (MassFlowRate * Cp);
            TankAverageTemp = ((ThisTankMass * Cp) / (MassFlowRate * Cp) *
                                   (LastTankOutletTemp - (MassFlowRate * Cp * TankInletTemp + PumpHeat) / (MassFlowRate * Cp)) *
                                   (1.0 - std::exp(-(MassFlowRate * Cp) / (ThisTankMass * Cp) * TimeStepSeconds)) / TimeStepSeconds +
                               (MassFlowRate * Cp * TankInletTemp + PumpHeat) / (MassFlowRate * Cp));
        } else {

            TankFinalTemp = PumpHeat / (ThisTankMass * Cp) * TimeStepSeconds + LastTankOutletTemp;
            TankAverageTemp = (TankFinalTemp + LastTankOutletTemp) / 2.0;
        }
    }
    // Common Pipe Simulation
    if (CommonPipeType == DataPlant::iCommonPipeType::Single) {
        ManageSingleCommonPipe(state, LoopNum, TankOutletLoopSide, TankAverageTemp, MixedOutletTemp);
        // 2-way (controlled) common pipe simulation
    } else if (CommonPipeType == DataPlant::iCommonPipeType::TwoWay) {

        ManageTwoWayCommonPipe(state, LoopNum, TankOutletLoopSide, TankAverageTemp);
        MixedOutletTemp = state.dataLoopNodes->Node(TankOutletNode).Temp;
    }

    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).TempInterfaceTankOutlet = TankFinalTemp;

    state.dataPlnt->PlantLoop(LoopNum).LoopSide(TankOutletLoopSide).LoopSideInlet_TankTemp = TankAverageTemp;
}

void ManageSingleCommonPipe(EnergyPlusData &state,
                            int const LoopNum,           // plant loop number
                            int const LoopSide,          // plant loop side number
                            Real64 const TankOutletTemp, // inlet temperature to the common pipe passed in from the capacitance calculation
                            Real64 &MixedOutletTemp      // inlet temperature to the common pipe passed in from the capacitance calculation
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   November 2006
    //       MODIFIED       B. Griffith, Jan 2010 clean up setup to allow mixing common pipe modes
    //                      B. Griffith, Mar 2010 add LoopNum arg and simplify
    //       RE-ENGINEERED  D. Fisher, Sept. 2010
    //                      B. Griffith, Oct 2011, major rewrite for plant upgrade

    // PURPOSE OF THIS SUBROUTINE:
    // To determine the conditions in common pipe viz., the flow flow temperature and direction of flow.

    // METHODOLOGY EMPLOYED:
    // Determine the flow on both sides of the common pipe. Decide if flow is coming into common pipe
    // or going out of common pipe. After that determine which interface calls the subroutine, i.e. if
    // called from "Demand to Supply" interface or "Supply to Demand" interface. Update the node temperatures
    // accordingly.

    // Using/Aliasing
    using namespace DataPlant;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS
    Real64 MdotPri(0.0);      // flow rate on primary side kg/s
    Real64 MdotSec(0.0);      // flow rate on secondary side kg/s
    Real64 MdotPriRCLeg(0.0); // flow rate of primary recirculation thru common pipe kg/s
    Real64 MdotSecRCLeg(0.0); // flow rate of secondary recirculation thru common pipe kg/s
    Real64 TempSecInlet(0.0); // temperature at secondary inlet deg C
    Real64 TempPriInlet(0.0); // temperature at primary inlet deg C
    Real64 TempPriOutTankOut(0.0);
    Real64 TempSecOutTankOut(0.0);
    int NodeNumPriOut(0);
    int NodeNumSecOut(0);
    int NodeNumPriIn(0);
    int NodeNumSecIn(0);
    int CPFlowDir; // flow direction in single common pipe
    Real64 CommonPipeTemp;

    auto &PlantCommonPipe(state.dataHVACInterfaceMgr->PlantCommonPipe);
    auto &MyEnvrnFlag(state.dataHVACInterfaceMgr->MyEnvrnFlag_SingleCommonPipe);

    // One time call to set up report variables and set common pipe 'type' flag
    if (state.dataHVACInterfaceMgr->OneTimeData_SingleCommonPipe) {
        if (!state.dataHVACInterfaceMgr->CommonPipeSetupFinished) SetupCommonPipes(state);
        MyEnvrnFlag.dimension(state.dataPlnt->TotNumLoops, true);
        state.dataHVACInterfaceMgr->OneTimeData_SingleCommonPipe = false;
    }

    // fill local node indexes
    NodeNumPriIn = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn;
    NodeNumPriOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumOut;
    NodeNumSecIn = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn;
    NodeNumSecOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumOut;

    if (MyEnvrnFlag(LoopNum) && state.dataGlobal->BeginEnvrnFlag) {
        PlantCommonPipe(LoopNum).Flow = 0.0;
        PlantCommonPipe(LoopNum).Temp = 0.0;
        PlantCommonPipe(LoopNum).FlowDir = NoRecircFlow;
        MyEnvrnFlag(LoopNum) = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        MyEnvrnFlag(LoopNum) = true;
    }

    // every time inits
    MdotSec = state.dataLoopNodes->Node(NodeNumSecOut).MassFlowRate;
    MdotPri = state.dataLoopNodes->Node(NodeNumPriOut).MassFlowRate;

    if (LoopSide == SupplySide) {
        TempSecOutTankOut = TankOutletTemp;
        TempPriOutTankOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TankTemp;
    } else {
        TempPriOutTankOut = TankOutletTemp;
        TempSecOutTankOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_TankTemp;
    }

    // first do mass balances and find common pipe flow rate and direction
    if (MdotPri > MdotSec) {
        MdotPriRCLeg = MdotPri - MdotSec;
        if (MdotPriRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) {
            MdotPriRCLeg = 0.0;
            CPFlowDir = NoRecircFlow;
        } else {
            CPFlowDir = PrimaryRecirc;
        }
        MdotSecRCLeg = 0.0;
        CommonPipeTemp = TempPriOutTankOut;
    } else if (MdotPri < MdotSec) {
        MdotSecRCLeg = MdotSec - MdotPri;
        if (MdotSecRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) {
            MdotSecRCLeg = 0.0;
            CPFlowDir = NoRecircFlow;
        } else {
            CPFlowDir = SecondaryRecirc;
        }
        MdotPriRCLeg = 0.0;
        CommonPipeTemp = TempSecOutTankOut;
    } else { // equal
        MdotPriRCLeg = 0.0;
        MdotSecRCLeg = 0.0;
        CPFlowDir = NoRecircFlow;
        CommonPipeTemp = (TempPriOutTankOut + TempSecOutTankOut) / 2.0;
    }

    // now calculate inlet temps

    if (MdotSec > 0.0) {
        TempSecInlet = (MdotPri * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut - MdotPriRCLeg * TempPriOutTankOut) / (MdotSec);
    } else {
        TempSecInlet = TempPriOutTankOut;
    }
    if (MdotPri > 0.0) {
        TempPriInlet = (MdotSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut - MdotSecRCLeg * TempSecOutTankOut) / (MdotPri);
    } else {
        TempPriInlet = TempSecOutTankOut;
    }

    // Update the Common Pipe Data structure for reporting purposes.
    PlantCommonPipe(LoopNum).Flow = max(MdotPriRCLeg, MdotSecRCLeg);
    PlantCommonPipe(LoopNum).Temp = CommonPipeTemp;
    PlantCommonPipe(LoopNum).FlowDir = CPFlowDir;
    state.dataLoopNodes->Node(NodeNumSecIn).Temp = TempSecInlet;
    state.dataLoopNodes->Node(NodeNumPriIn).Temp = TempPriInlet;

    if (LoopSide == SupplySide) {
        MixedOutletTemp = TempPriInlet;
    } else {
        MixedOutletTemp = TempSecInlet;
    }
}

void ManageTwoWayCommonPipe(EnergyPlusData &state, int const LoopNum, int const LoopSide, Real64 const TankOutletTemp)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   June 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  B. Griffith, Oct 2011.  rewrite

    // PURPOSE OF THIS SUBROUTINE:
    // manage two-way common pipe modeling at half-loop interface

    // METHODOLOGY EMPLOYED:
    // calculate mixed temperatures and various flow rates
    // sequential substitution of system of equations

    // REFERENCES:
    // reimplementation of CheckTwoWayCommonPipeConditions by Sankaranarayanan K P Jan 2007

    // Using/Aliasing
    using DataPlant::DeltaTempTol;
    using DataPlant::DemandSide;
    using DataPlant::SupplySide;
    using PlantUtilities::SetActuatedBranchFlowRate;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const DemandLedPrimaryInletUpdate(101);
    int const DemandLedSecondaryInletUpdate(102);
    int const SupplyLedPrimaryInletUpdate(103);
    int const SupplyLedSecondaryInletUpdate(104);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CurCallingCase;       // local temporary
    Real64 MdotPri(0.0);      // flow rate on primary side kg/s
    Real64 MdotSec(0.0);      // flow rate on secondary side kg/s
    Real64 MdotPriToSec(0.0); // flow rate between primary and secondary side kg/s
    Real64 MdotPriRCLeg(0.0); // flow rate on primary recirculation common pipe kg/s
    Real64 MdotSecRCLeg(0.0); // flow rate on secondary recirculation common pipe kg/s
    Real64 TempSecInlet(0.0); // temperature at secondary inlet deg C
    Real64 TempPriInlet(0.0); // temperature at primary inlet deg C
    Real64 TempPriOutTankOut(0.0);
    Real64 TempSecOutTankOut(0.0);
    Real64 TempCPPrimaryCntrlSetPoint(0.0);
    Real64 TempCPSecondaryCntrlSetPoint(0.0);
    int NodeNumPriOut(0);
    int NodeNumSecOut(0);
    int NodeNumPriIn(0);
    int NodeNumSecIn(0);
    constexpr int MaxIterLimitCaseA(8);
    constexpr int MaxIterLimitCaseB(4);

    int loop; // iteration loop counter

    auto &PlantCommonPipe(state.dataHVACInterfaceMgr->PlantCommonPipe);
    auto &MyEnvrnFlag(state.dataHVACInterfaceMgr->MyEnvrnFlag_TwoWayCommonPipe);

    // one time setups
    if (state.dataHVACInterfaceMgr->OneTimeData_TwoWayCommonPipe) {
        if (!state.dataHVACInterfaceMgr->CommonPipeSetupFinished) SetupCommonPipes(state);
        MyEnvrnFlag.dimension(state.dataPlnt->TotNumLoops, true);
        state.dataHVACInterfaceMgr->OneTimeData_TwoWayCommonPipe = false;
    }

    // fill local node indexes
    NodeNumPriIn = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumIn;
    NodeNumPriOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).NodeNumOut;
    NodeNumSecIn = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumIn;
    NodeNumSecOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).NodeNumOut;

    // begin environment inits
    if (MyEnvrnFlag(LoopNum) && state.dataGlobal->BeginEnvrnFlag) {
        PlantCommonPipe(LoopNum).PriToSecFlow = 0.0;
        PlantCommonPipe(LoopNum).SecToPriFlow = 0.0;
        PlantCommonPipe(LoopNum).PriCPLegFlow = 0.0;
        PlantCommonPipe(LoopNum).SecCPLegFlow = 0.0;
        MyEnvrnFlag(LoopNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        MyEnvrnFlag(LoopNum) = true;
    }

    // every time inits
    MdotSec = state.dataLoopNodes->Node(NodeNumSecOut).MassFlowRate; // assume known and fixed by demand side operation
    TempCPPrimaryCntrlSetPoint = state.dataLoopNodes->Node(NodeNumPriIn).TempSetPoint;
    TempCPSecondaryCntrlSetPoint = state.dataLoopNodes->Node(NodeNumSecIn).TempSetPoint;

    // 6 unknowns follow, fill with current values
    MdotPriToSec = PlantCommonPipe(LoopNum).PriToSecFlow;
    MdotPriRCLeg = PlantCommonPipe(LoopNum).PriCPLegFlow;
    MdotSecRCLeg = PlantCommonPipe(LoopNum).SecCPLegFlow;
    TempSecInlet = state.dataLoopNodes->Node(NodeNumSecIn).Temp;
    TempPriInlet = state.dataLoopNodes->Node(NodeNumPriIn).Temp;
    MdotPri = state.dataLoopNodes->Node(NodeNumPriOut).MassFlowRate; // may or may not be an unknown, If variable speed primary side, then unknown

    if (LoopSide == SupplySide) {
        TempSecOutTankOut = TankOutletTemp;
        TempPriOutTankOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).LoopSideInlet_TankTemp;
    } else {
        TempPriOutTankOut = TankOutletTemp;
        TempSecOutTankOut = state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).LoopSideInlet_TankTemp;
    }

    // determine current case
    // which side is being updated
    // commonpipe control point is the inlet of one of the half loops
    CurCallingCase = 0;
    if (LoopSide == SupplySide) { // update primary inlet
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).InletNodeSetPt &&
            !state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
            CurCallingCase = SupplyLedPrimaryInletUpdate;

        } else if (!state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).InletNodeSetPt &&
                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
            CurCallingCase = DemandLedPrimaryInletUpdate;
        }
    } else { // update secondary inlet
        if (state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).InletNodeSetPt &&
            !state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
            CurCallingCase = SupplyLedSecondaryInletUpdate;

        } else if (!state.dataPlnt->PlantLoop(LoopNum).LoopSide(SupplySide).InletNodeSetPt &&
                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(DemandSide).InletNodeSetPt) {
            CurCallingCase = DemandLedSecondaryInletUpdate;
        }
    }

    {
        auto const SELECT_CASE_var(CurCallingCase);

        if ((SELECT_CASE_var == SupplyLedPrimaryInletUpdate) || (SELECT_CASE_var == SupplyLedSecondaryInletUpdate)) {
            // CASE A, Primary/Supply Led
            // six equations and six unknowns (although one has a setpoint)
            for (loop = 1; loop <= MaxIterLimitCaseA; ++loop) {

                // eq 1
                if (std::abs(TempSecOutTankOut - TempCPPrimaryCntrlSetPoint) > DeltaTempTol) {
                    MdotPriToSec = MdotPriRCLeg * (TempCPPrimaryCntrlSetPoint - TempPriOutTankOut) / (TempSecOutTankOut - TempCPPrimaryCntrlSetPoint);
                    if (MdotPriToSec < DataBranchAirLoopPlant::MassFlowTolerance) MdotPriToSec = 0.0;
                    if (MdotPriToSec > MdotSec) MdotPriToSec = MdotSec;
                } else {
                    MdotPriToSec = MdotSec; //  what to do (?)
                }
                // eq. 5
                MdotPriRCLeg = MdotPri - MdotPriToSec;
                if (MdotPriRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) MdotPriRCLeg = 0.0;

                // eq. 4
                MdotSecRCLeg = MdotSec - MdotPriToSec;
                if (MdotSecRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) MdotSecRCLeg = 0.0;

                // eq  6
                if ((MdotPriToSec + MdotSecRCLeg) > DataBranchAirLoopPlant::MassFlowTolerance) {
                    TempSecInlet = (MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut) / (MdotPriToSec + MdotSecRCLeg);
                } else {
                    TempSecInlet = TempPriOutTankOut;
                }

                // eq. 3
                if ((PlantCommonPipe(LoopNum).SupplySideInletPumpType == FlowType::Variable) && (CurCallingCase == SupplyLedPrimaryInletUpdate)) {
                    // MdotPri is a variable to be calculated and flow request needs to be made
                    if (std::abs(TempCPPrimaryCntrlSetPoint) > DeltaTempTol) {

                        MdotPri = (MdotPriRCLeg * TempPriOutTankOut + MdotPriToSec * TempSecOutTankOut) / (TempCPPrimaryCntrlSetPoint);

                        if (MdotPri < DataBranchAirLoopPlant::MassFlowTolerance) MdotPri = 0.0;
                    } else {
                        MdotPri = MdotSec;
                    }
                    SetActuatedBranchFlowRate(state, MdotPri, NodeNumPriIn, LoopNum, SupplySide, 1, false);
                }

                // eq. 2
                if ((MdotPriToSec + MdotPriRCLeg) > DataBranchAirLoopPlant::MassFlowTolerance) {
                    TempPriInlet = (MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut) / (MdotPriToSec + MdotPriRCLeg);
                } else {
                    TempPriInlet = TempSecOutTankOut;
                }
            }
        } else if ((SELECT_CASE_var == DemandLedPrimaryInletUpdate) || (SELECT_CASE_var == DemandLedSecondaryInletUpdate)) {
            // case B. Secondary/demand led

            // six equations and six unknowns (although one has a setpoint)
            for (loop = 1; loop <= MaxIterLimitCaseB; ++loop) {
                // eq 1,
                if (std::abs(TempPriOutTankOut - TempSecOutTankOut) > DeltaTempTol) {
                    MdotPriToSec = MdotSec * (TempCPSecondaryCntrlSetPoint - TempSecOutTankOut) / (TempPriOutTankOut - TempSecOutTankOut);
                    if (MdotPriToSec < DataBranchAirLoopPlant::MassFlowTolerance) MdotPriToSec = 0.0;
                    if (MdotPriToSec > MdotSec) MdotPriToSec = MdotSec;
                } else {
                    MdotPriToSec = MdotSec;
                }

                // eq. 2,
                if ((MdotPriToSec + MdotPriRCLeg) > DataBranchAirLoopPlant::MassFlowTolerance) {
                    TempPriInlet = (MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut) / (MdotPriToSec + MdotPriRCLeg);
                } else {
                    TempPriInlet = TempSecOutTankOut;
                }

                // eq. 3
                if ((PlantCommonPipe(LoopNum).SupplySideInletPumpType == FlowType::Variable) && (CurCallingCase == DemandLedPrimaryInletUpdate)) {
                    // MdotPri is a variable to be calculated and flow request made
                    if (std::abs(TempPriOutTankOut - TempPriInlet) > DeltaTempTol) {
                        MdotPri = MdotSec * (TempCPSecondaryCntrlSetPoint - TempSecOutTankOut) / (TempPriOutTankOut - TempPriInlet);
                        if (MdotPri < DataBranchAirLoopPlant::MassFlowTolerance) MdotPri = 0.0;
                    } else {
                        MdotPri = MdotSec;
                    }
                    SetActuatedBranchFlowRate(state, MdotPri, NodeNumPriIn, LoopNum, SupplySide, 1, false);
                }

                // eq. 4
                MdotSecRCLeg = MdotSec - MdotPriToSec;
                if (MdotSecRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) MdotSecRCLeg = 0.0;

                // eq. 5
                MdotPriRCLeg = MdotPri - MdotPriToSec;
                if (MdotPriRCLeg < DataBranchAirLoopPlant::MassFlowTolerance) MdotPriRCLeg = 0.0;

                // eq  6
                if ((MdotPriToSec + MdotSecRCLeg) > DataBranchAirLoopPlant::MassFlowTolerance) {
                    TempSecInlet = (MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut) / (MdotPriToSec + MdotSecRCLeg);
                } else {
                    TempSecInlet = TempPriOutTankOut;
                }
            }

        } else {
            //???      CALL ShowFatalError('ManageTwoWayCommonPipe: Calling Case Fall Through')
        }
    }

    // update
    PlantCommonPipe(LoopNum).PriToSecFlow = MdotPriToSec;
    PlantCommonPipe(LoopNum).SecToPriFlow = MdotPriToSec;
    PlantCommonPipe(LoopNum).PriCPLegFlow = MdotPriRCLeg;
    PlantCommonPipe(LoopNum).SecCPLegFlow = MdotSecRCLeg;
    state.dataLoopNodes->Node(NodeNumSecIn).Temp = TempSecInlet;
    state.dataLoopNodes->Node(NodeNumPriIn).Temp = TempPriInlet;
}

void SetupCommonPipes(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Jan. 2010
    //       MODIFIED       B. Griffith Oct. 2011
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // collect allocation, outputs, and other set up for common pipes

    // Using/Aliasing
    using namespace DataPlant;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CurLoopNum; // local do loop counter

    auto &PlantCommonPipe(state.dataHVACInterfaceMgr->PlantCommonPipe);

    PlantCommonPipe.allocate(state.dataPlnt->TotNumLoops);

    for (CurLoopNum = 1; CurLoopNum <= state.dataPlnt->TotNumLoops; ++CurLoopNum) {

        // reference to easily lookup the first item once
        auto &first_demand_component_typenum(state.dataPlnt->PlantLoop(CurLoopNum).LoopSide(DemandSide).Branch(1).Comp(1).TypeOf_Num);
        auto &first_supply_component_typenum(state.dataPlnt->PlantLoop(CurLoopNum).LoopSide(SupplySide).Branch(1).Comp(1).TypeOf_Num);

        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(CurLoopNum).CommonPipeType);
            if (SELECT_CASE_var == DataPlant::iCommonPipeType::No) {
                PlantCommonPipe(CurLoopNum).CommonPipeType = DataPlant::iCommonPipeType::No;

            } else if (SELECT_CASE_var == DataPlant::iCommonPipeType::Single) { // Uncontrolled ('single') common pipe
                PlantCommonPipe(CurLoopNum).CommonPipeType = DataPlant::iCommonPipeType::Single;
                SetupOutputVariable(state,
                                    "Plant Common Pipe Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantCommonPipe(CurLoopNum).Flow,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);
                SetupOutputVariable(state,
                                    "Plant Common Pipe Temperature",
                                    OutputProcessor::Unit::C,
                                    PlantCommonPipe(CurLoopNum).Temp,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);
                SetupOutputVariable(state,
                                    "Plant Common Pipe Flow Direction Status",
                                    OutputProcessor::Unit::None,
                                    PlantCommonPipe(CurLoopNum).FlowDir,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);

                if (first_supply_component_typenum == TypeOf_PumpVariableSpeed) {
                    // If/when the model supports variable-pumping primary, this can be removed.
                    ShowWarningError(state, "SetupCommonPipes: detected variable speed pump on supply inlet of CommonPipe plant loop");
                    ShowContinueError(state, "Occurs on plant loop name = " + state.dataPlnt->PlantLoop(CurLoopNum).Name);
                    ShowContinueError(state, "The common pipe model does not support varying the flow rate on the primary/supply side");
                    ShowContinueError(state, "The primary/supply side will operate as if constant speed, and the simulation continues");
                }

            } else if (SELECT_CASE_var == DataPlant::iCommonPipeType::TwoWay) { // Controlled ('two-way') common pipe
                PlantCommonPipe(CurLoopNum).CommonPipeType = DataPlant::iCommonPipeType::TwoWay;
                SetupOutputVariable(state,
                                    "Plant Common Pipe Primary Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantCommonPipe(CurLoopNum).PriCPLegFlow,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);
                SetupOutputVariable(state,
                                    "Plant Common Pipe Secondary Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantCommonPipe(CurLoopNum).SecCPLegFlow,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);
                SetupOutputVariable(state,
                                    "Plant Common Pipe Primary to Secondary Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantCommonPipe(CurLoopNum).PriToSecFlow,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);
                SetupOutputVariable(state,
                                    "Plant Common Pipe Secondary to Primary Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantCommonPipe(CurLoopNum).SecToPriFlow,
                                    "System",
                                    "Average",
                                    state.dataPlnt->PlantLoop(CurLoopNum).Name);

                // check type of pump on supply side inlet
                if (first_supply_component_typenum == TypeOf_PumpConstantSpeed) {
                    PlantCommonPipe(CurLoopNum).SupplySideInletPumpType = FlowType::Constant;
                } else if (first_supply_component_typenum == TypeOf_PumpVariableSpeed) {
                    PlantCommonPipe(CurLoopNum).SupplySideInletPumpType = FlowType::Variable;
                    // If/when the model supports variable-pumping primary, this can be removed.
                    ShowWarningError(state, "SetupCommonPipes: detected variable speed pump on supply inlet of TwoWayCommonPipe plant loop");
                    ShowContinueError(state, "Occurs on plant loop name = " + state.dataPlnt->PlantLoop(CurLoopNum).Name);
                    ShowContinueError(state, "The common pipe model does not support varying the flow rate on the primary/supply side");
                    ShowContinueError(state, "The primary/supply side will operate as if constant speed, and the simulation continues");
                }
                // check type of pump on demand side inlet
                if (first_demand_component_typenum == TypeOf_PumpConstantSpeed) {
                    PlantCommonPipe(CurLoopNum).DemandSideInletPumpType = FlowType::Constant;
                } else if (first_demand_component_typenum == TypeOf_PumpVariableSpeed) {
                    PlantCommonPipe(CurLoopNum).DemandSideInletPumpType = FlowType::Variable;
                }
            }
        }
    }

    state.dataHVACInterfaceMgr->CommonPipeSetupFinished = true;
}

} // namespace EnergyPlus::HVACInterfaceManager
