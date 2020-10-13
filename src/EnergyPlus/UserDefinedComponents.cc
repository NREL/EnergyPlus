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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus {

namespace UserDefinedComponents {

    // Module containing the routines dealing with the User Defined HVAC and Plant component models

    // MODULE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   January 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Collect component models for custom program with Erl.

    PlantComponent *UserPlantComponentStruct::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (state.dataUserDefinedComponents->GetPlantCompInput) {
            GetUserDefinedPlantComponents(state);
            state.dataUserDefinedComponents->GetPlantCompInput = false;
        }
        // Now look for this particular object
        for (auto &thisComp : state.dataUserDefinedComponents->UserPlantComp) {
            if (thisComp.Name == objectName) {
                return &thisComp;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalUserDefinedPlantComponentFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void UserPlantComponentStruct::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        bool anyEMSRan;
        Real64 myLoad = 0.0;
        int thisLoop = 0;

        this->initialize(state, calledFromLocation.loopNum, myLoad);

        for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
            if (calledFromLocation.loopNum != this->Loop(loop).LoopNum) continue;
            if (calledFromLocation.loopSideNum != this->Loop(loop).LoopSideNum) continue;
            thisLoop = loop;
        }

        if (thisLoop > 0) {
            if (this->Loop(thisLoop).ErlInitProgramMngr > 0) {
                EMSManager::ManageEMS(
                    state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, this->Loop(thisLoop).ErlInitProgramMngr);
            } else if (this->Loop(thisLoop).initPluginLocation > -1) {
                EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, this->Loop(thisLoop).initPluginLocation);
            }

            PlantUtilities::InitComponentNodes(this->Loop(thisLoop).MassFlowRateMin,
                                               this->Loop(thisLoop).MassFlowRateMax,
                                               this->Loop(thisLoop).InletNodeNum,
                                               this->Loop(thisLoop).OutletNodeNum,
                                               this->Loop(thisLoop).LoopNum,
                                               this->Loop(thisLoop).LoopSideNum,
                                               this->Loop(thisLoop).BranchNum,
                                               this->Loop(thisLoop).CompNum);

            PlantUtilities::RegisterPlantCompDesignFlow(this->Loop(thisLoop).InletNodeNum, this->Loop(thisLoop).DesignVolumeFlowRate);

        } else {
            // throw warning
            ShowFatalError("SimUserDefinedPlantComponent: did not find where called from loop number called from =" +
                           General::TrimSigDigits(calledFromLocation.loopNum) +
                           " , loop side called from =" + General::TrimSigDigits(calledFromLocation.loopSideNum));
        }
    }

    void UserPlantComponentStruct::getDesignCapacities(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        int thisLoop = 0;
        for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
            if (calledFromLocation.loopNum != this->Loop(loop).LoopNum) continue;
            if (calledFromLocation.loopSideNum != this->Loop(loop).LoopSideNum) continue;
            thisLoop = loop;
        }

        MinLoad = this->Loop(thisLoop).MinLoad;
        MaxLoad = this->Loop(thisLoop).MaxLoad;
        OptLoad = this->Loop(thisLoop).OptLoad;
    }

    void UserPlantComponentStruct::UserPlantComponentStruct::simulate(EnergyPlusData &state, const EnergyPlus::PlantLocation &calledFromLocation,
                                                                      bool EP_UNUSED(FirstHVACIteration),
                                                                      Real64 &CurLoad,
                                                                      bool EP_UNUSED(RunFlag))
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // User Defined plant generic component

        if (state.dataGlobal->BeginEnvrnFlag) {
            this->onInitLoopEquip(state, calledFromLocation);
        }

        bool anyEMSRan;
        int thisLoop = 0;

        for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
            if (calledFromLocation.loopNum != this->Loop(loop).LoopNum) continue;
            if (calledFromLocation.loopSideNum != this->Loop(loop).LoopSideNum) continue;
            thisLoop = loop;
        }

        this->initialize(state, thisLoop, CurLoad);

        if (thisLoop > 0) {
            if (this->Loop(thisLoop).ErlSimProgramMngr > 0) {
                EMSManager::ManageEMS(
                    state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, this->Loop(thisLoop).ErlSimProgramMngr);
            } else if (this->Loop(thisLoop).simPluginLocation > -1) {
                EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, this->Loop(thisLoop).simPluginLocation);
            }
        }

        if (this->ErlSimProgramMngr > 0) {
            EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, this->ErlSimProgramMngr);
        } else if (this->simPluginLocation > -1) {
            EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, this->simPluginLocation);
        }

        this->report(thisLoop);
    }

    void SimCoilUserDefined(EnergyPlusData &state,
                            std::string const &EquipName, // user name for component
                            int &CompIndex,
                            int const AirLoopNum,
                            bool &HeatingActive,
                            bool &CoolingActive)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int CompNum;

        if (state.dataUserDefinedComponents->GetPlantCompInput) {
            GetUserDefinedPlantComponents(state);
            state.dataUserDefinedComponents->GetPlantCompInput = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            CompNum = UtilityRoutines::FindItemInList(EquipName, state.dataUserDefinedComponents->UserCoil);
            if (CompNum == 0) {
                ShowFatalError("SimUserDefinedPlantComponent: User Defined Coil not found");
            }
            CompIndex = CompNum;
        } else {
            CompNum = CompIndex;
            if (CompNum < 1 || CompNum > state.dataUserDefinedComponents->NumUserCoils) {
                ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                               ", Number of units =" + General::TrimSigDigits(state.dataUserDefinedComponents->NumUserCoils) + ", Entered Unit name = " + EquipName);
            }
            if (state.dataUserDefinedComponents->CheckUserCoilName(CompNum)) {
                if (EquipName != state.dataUserDefinedComponents->UserCoil(CompNum).Name) {
                    ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", Unit name=" + EquipName + ", stored unit name for that index=" + state.dataUserDefinedComponents->UserCoil(CompNum).Name);
                }
                state.dataUserDefinedComponents->CheckUserCoilName(CompNum) = false;
            }
        }
        bool anyEMSRan;
        if (state.dataGlobal->BeginEnvrnFlag) {
            if (state.dataUserDefinedComponents->UserCoil(CompNum).ErlInitProgramMngr > 0) {
                EMSManager::ManageEMS(
                    state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserCoil(CompNum).ErlInitProgramMngr);
            } else if (state.dataUserDefinedComponents->UserCoil(CompNum).initPluginLocation > -1) {
                EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserCoil(CompNum).initPluginLocation);
            }

            if (state.dataUserDefinedComponents->UserCoil(CompNum).PlantIsConnected) {

                PlantUtilities::InitComponentNodes(state.dataUserDefinedComponents->UserCoil(CompNum).Loop.MassFlowRateMin,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.MassFlowRateMax,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.InletNodeNum,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.OutletNodeNum,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.LoopNum,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.LoopSideNum,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.BranchNum,
                                                   state.dataUserDefinedComponents->UserCoil(CompNum).Loop.CompNum);

                PlantUtilities::RegisterPlantCompDesignFlow(state.dataUserDefinedComponents->UserCoil(CompNum).Loop.InletNodeNum, state.dataUserDefinedComponents->UserCoil(CompNum).Loop.DesignVolumeFlowRate);
            }
        }

        state.dataUserDefinedComponents->UserCoil(CompNum).initialize(state);

        if (state.dataUserDefinedComponents->UserCoil(CompNum).ErlSimProgramMngr > 0) {
            EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserCoil(CompNum).ErlSimProgramMngr);
        } else if (state.dataUserDefinedComponents->UserCoil(CompNum).simPluginLocation > -1) {
            EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserCoil(CompNum).simPluginLocation);
        }

        state.dataUserDefinedComponents->UserCoil(CompNum).report();

        if (AirLoopNum != -1) { // IF the system is not an equipment of outdoor air unit
            // determine if heating or cooling on primary air stream
            HeatingActive =
                DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).InletNodeNum).Temp < DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).OutletNodeNum).Temp;

            Real64 EnthInlet = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).InletNodeNum).Temp,
                                                          DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).InletNodeNum).HumRat);
            Real64 EnthOutlet = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).OutletNodeNum).Temp,
                                                           DataLoopNode::Node(state.dataUserDefinedComponents->UserCoil(CompNum).Air(1).OutletNodeNum).HumRat);
            CoolingActive = EnthInlet > EnthOutlet;
        }
    }

    void SimZoneAirUserDefined(EnergyPlusData &state,
                               std::string const &CompName,    // name of the packaged terminal heat pump
                               int const ZoneNum,              // number of zone being served
                               Real64 &SensibleOutputProvided, // sensible capacity delivered to zone
                               Real64 &LatentOutputProvided,   // Latent add/removal  (kg/s), dehumid = negative
                               int &CompIndex                  // index to zone hvac unit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   February, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        int CompNum;

        if (state.dataUserDefinedComponents->GetInput) {
            GetUserDefinedComponents(state);
            state.dataUserDefinedComponents->GetInput = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            CompNum = UtilityRoutines::FindItemInList(CompName, state.dataUserDefinedComponents->UserZoneAirHVAC);
            if (CompNum == 0) {
                ShowFatalError("SimUserDefinedPlantComponent: User Defined Coil not found");
            }
            CompIndex = CompNum;
        } else {
            CompNum = CompIndex;
            if (CompNum < 1 || CompNum > state.dataUserDefinedComponents->NumUserZoneAir) {
                ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                               ", Number of units =" + General::TrimSigDigits(state.dataUserDefinedComponents->NumUserZoneAir) + ", Entered Unit name = " + CompName);
            }
            if (state.dataUserDefinedComponents->CheckUserZoneAirName(CompNum)) {
                if (CompName != state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Name) {
                    ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", Unit name=" + CompName + ", stored unit name for that index=" + state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Name);
                }
                state.dataUserDefinedComponents->CheckUserZoneAirName(CompNum) = false;
            }
        }
        bool anyEMSRan;
        if (state.dataGlobal->BeginEnvrnFlag) {
            state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).initialize(state, ZoneNum);

            if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ErlInitProgramMngr > 0) {
                EMSManager::ManageEMS(
                    state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ErlInitProgramMngr);
            } else if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).initPluginLocation > -1) {
                EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).initPluginLocation);
            }
            if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).NumPlantConnections > 0) {
                for (int Loop = 1; Loop <= state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).NumPlantConnections; ++Loop) {

                    PlantUtilities::InitComponentNodes(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).MassFlowRateMin,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).MassFlowRateMax,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).InletNodeNum,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).OutletNodeNum,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).LoopNum,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).LoopSideNum,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).BranchNum,
                                                       state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).CompNum);

                    PlantUtilities::RegisterPlantCompDesignFlow(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).InletNodeNum,
                                                                state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).Loop(Loop).DesignVolumeFlowRate);
                }
            }

        } // BeginEnvrnFlag

        state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).initialize(state, ZoneNum);

        if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ErlSimProgramMngr > 0) {
            EMSManager::ManageEMS(
                state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ErlSimProgramMngr);
        } else if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).simPluginLocation > -1) {
            EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).simPluginLocation);
        }

        state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).report();

        // calculate delivered capacity
        Real64 AirMassFlow = min(DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.InletNodeNum).MassFlowRate,
                                 DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.OutletNodeNum).MassFlowRate);
        // calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
        Real64 MinHumRat = min(DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.InletNodeNum).HumRat,
                               DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.OutletNodeNum).HumRat);
        SensibleOutputProvided =
            AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.OutletNodeNum).Temp, MinHumRat) -
                           Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.InletNodeNum).Temp, MinHumRat));

        Real64 SpecHumOut = DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.OutletNodeNum).HumRat;
        Real64 SpecHumIn = DataLoopNode::Node(state.dataUserDefinedComponents->UserZoneAirHVAC(CompNum).ZoneAir.InletNodeNum).HumRat;
        LatentOutputProvided = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate, kg/s (dehumid = negative)
    }

    void SimAirTerminalUserDefined(EnergyPlusData &state,
        std::string const &CompName, bool const EP_UNUSED(FirstHVACIteration), int const ZoneNum, int const EP_UNUSED(ZoneNodeNum), int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulation call for generic air terminal

        int CompNum;

        if (state.dataUserDefinedComponents->GetInput) {
            GetUserDefinedComponents(state);
            state.dataUserDefinedComponents->GetInput = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            CompNum = UtilityRoutines::FindItemInList(CompName, state.dataUserDefinedComponents->UserAirTerminal);
            if (CompNum == 0) {
                ShowFatalError("SimUserDefinedPlantComponent: User Defined Coil not found");
            }
            CompIndex = CompNum;
        } else {
            CompNum = CompIndex;
            if (CompNum < 1 || CompNum > state.dataUserDefinedComponents->NumUserAirTerminals) {
                ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                               ", Number of units =" + General::TrimSigDigits(state.dataUserDefinedComponents->NumUserAirTerminals) + ", Entered Unit name = " + CompName);
            }
            if (state.dataUserDefinedComponents->CheckUserAirTerminal(CompNum)) {
                if (CompName != state.dataUserDefinedComponents->UserAirTerminal(CompNum).Name) {
                    ShowFatalError("SimUserDefinedPlantComponent: Invalid CompIndex passed=" + General::TrimSigDigits(CompNum) +
                                   ", Unit name=" + CompName + ", stored unit name for that index=" + state.dataUserDefinedComponents->UserAirTerminal(CompNum).Name);
                }
                state.dataUserDefinedComponents->CheckUserAirTerminal(CompNum) = false;
            }
        }
        bool anyEMSRan;
        if (state.dataGlobal->BeginEnvrnFlag) {
            state.dataUserDefinedComponents->UserAirTerminal(CompNum).initialize(state, ZoneNum);

            if (state.dataUserDefinedComponents->UserAirTerminal(CompNum).ErlInitProgramMngr > 0) {
                EMSManager::ManageEMS(
                    state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserAirTerminal(CompNum).ErlInitProgramMngr);
            } else if (state.dataUserDefinedComponents->UserAirTerminal(CompNum).initPluginLocation > -1) {
                EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserAirTerminal(CompNum).initPluginLocation);
            }
            if (state.dataUserDefinedComponents->UserAirTerminal(CompNum).NumPlantConnections > 0) {
                for (int Loop = 1; Loop <= state.dataUserDefinedComponents->UserAirTerminal(CompNum).NumPlantConnections; ++Loop) {

                    PlantUtilities::InitComponentNodes(state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).MassFlowRateMin,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).MassFlowRateMax,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).InletNodeNum,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).OutletNodeNum,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).LoopNum,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).LoopSideNum,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).BranchNum,
                                                       state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).CompNum);

                    PlantUtilities::RegisterPlantCompDesignFlow(state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).InletNodeNum,
                                                                state.dataUserDefinedComponents->UserAirTerminal(CompNum).Loop(Loop).DesignVolumeFlowRate);
                }
            }

        } // BeginEnvrnFlag

        state.dataUserDefinedComponents->UserAirTerminal(CompNum).initialize(state, ZoneNum);

        if (state.dataUserDefinedComponents->UserAirTerminal(CompNum).ErlSimProgramMngr > 0) {
            EMSManager::ManageEMS(
                state, EMSManager::EMSCallFrom::UserDefinedComponentModel, anyEMSRan, state.dataUserDefinedComponents->UserAirTerminal(CompNum).ErlSimProgramMngr);
        } else if (state.dataUserDefinedComponents->UserAirTerminal(CompNum).simPluginLocation > -1) {
            EnergyPlus::PluginManagement::pluginManager->runSingleUserDefinedPlugin(state, state.dataUserDefinedComponents->UserAirTerminal(CompNum).simPluginLocation);
        }

        state.dataUserDefinedComponents->UserAirTerminal(CompNum).report();
    }

    void GetUserDefinedPlantComponents(EnergyPlusData &state)
    {
        static ObjexxFCL::gio::Fmt fmtLD("*");

        bool ErrorsFound(false);
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        int TotalArgs; // argument for call to GetObjectDefMaxArgs
        Array1D_string cAlphaFieldNames;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        static bool lDummy; // Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference

        cCurrentModuleObject = "PlantComponent:UserDefined";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);

        cAlphaFieldNames.allocate(NumAlphas);
        cAlphaArgs.allocate(NumAlphas);
        lAlphaFieldBlanks.dimension(NumAlphas, false);
        rNumericArgs.dimension(NumNums, 0.0);

        // need to make sure GetEMSInput has run...

        state.dataUserDefinedComponents->NumUserPlantComps = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (state.dataUserDefinedComponents->NumUserPlantComps > 0) {
            state.dataUserDefinedComponents->UserPlantComp.allocate(state.dataUserDefinedComponents->NumUserPlantComps);
            state.dataUserDefinedComponents->CheckUserPlantCompName.dimension(state.dataUserDefinedComponents->NumUserPlantComps, true);
            for (int CompLoop = 1; CompLoop <= state.dataUserDefinedComponents->NumUserPlantComps; ++CompLoop) {
                inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, _);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name = cAlphaArgs(1);

                // now get program manager for model simulations
                if (!lAlphaFieldBlanks(2)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserPlantComp(CompLoop).ErlSimProgramMngr = StackMngrNum;
                    } else {
                        // check Python Plugins
                        state.dataUserDefinedComponents->UserPlantComp(CompLoop).simPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(2));
                        if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).simPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                int NumPlantConnections = std::floor(rNumericArgs(1));

                if ((NumPlantConnections >= 1) && (NumPlantConnections <= 4)) {
                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop.allocate(NumPlantConnections);
                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).NumPlantConnections = NumPlantConnections;
                    for (int ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop) {
                        const auto LoopStr = fmt::to_string(ConnectionLoop);
                        int aArgCount = (ConnectionLoop - 1) * 6 + 3;
                        state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).InletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                ConnectionLoop,
                                                                DataLoopNode::ObjectIsNotParent);
                        state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).OutletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount + 1),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                ConnectionLoop,
                                                                DataLoopNode::ObjectIsNotParent);

                        BranchNodeConnections::TestCompSet(
                            cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(aArgCount), cAlphaArgs(aArgCount + 1), "Plant Nodes " + LoopStr);

                        {
                            auto const SELECT_CASE_var(cAlphaArgs(aArgCount + 2));
                            if (SELECT_CASE_var == "DEMANDSLOAD") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_NoneDemand;
                            } else if (SELECT_CASE_var == "MEETSLOADWITHPASSIVECAPACITY") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_PassiveCap;
                            } else if (SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITY") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_ByNominalCap;
                            } else if (SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_ByNominalCapLowOutLimit;
                                // actuator for low out limit
                                SetupEMSActuator("Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "Low Outlet Temperature Limit",
                                                 "[C]",
                                                 lDummy,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).LowOutTempLimit);
                            } else if (SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_ByNominalCapHiOutLimit;
                                // actuator for hi out limit
                                SetupEMSActuator("Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "High Outlet Temperature Limit",
                                                 "[C]",
                                                 lDummy,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HiOutTempLimit);
                            }
                        }

                        {
                            auto const SELECT_CASE_var(cAlphaArgs(aArgCount + 3));
                            if (SELECT_CASE_var == "NEEDSFLOWIFLOOPON") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
                            } else if (SELECT_CASE_var == "NEEDSFLOWANDTURNSLOOPON") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).FlowPriority = DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
                            } else if (SELECT_CASE_var == "RECEIVESWHATEVERFLOWAVAILABLE") {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).FlowPriority = DataPlant::LoopFlowStatus_TakesWhatGets;
                            }
                        }

                        // find program manager for initial setup, begin environment and sizing of this plant connection
                        if (!lAlphaFieldBlanks(aArgCount + 4)) {
                            int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(aArgCount + 4), DataRuntimeLanguage::EMSProgramCallManager);
                            if (StackMngrNum > 0) { // found it
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).ErlInitProgramMngr = StackMngrNum;
                            } else {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).initPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(aArgCount + 4));
                                if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).initPluginLocation == -1) {
                                    ShowSevereError("Invalid " + cAlphaFieldNames(aArgCount + 4) + '=' + cAlphaArgs(aArgCount + 4));
                                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                                    ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                                    ErrorsFound = true;
                                }
                            }
                        }

                        // find program to call for model simulations for just this plant connection
                        if (!lAlphaFieldBlanks(aArgCount + 5)) {
                            int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(aArgCount + 5), DataRuntimeLanguage::EMSProgramCallManager);
                            if (StackMngrNum > 0) { // found it
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).ErlSimProgramMngr = StackMngrNum;
                            } else {
                                state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).simPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(aArgCount + 5));
                                if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).simPluginLocation == -1) {
                                    ShowSevereError("Invalid " + cAlphaFieldNames(aArgCount + 4) + '=' + cAlphaArgs(aArgCount + 4));
                                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                                    ShowContinueError("Program Manager Name not found.");
                                    ErrorsFound = true;
                                }
                            }
                        }
                        // Setup Internal Variables
                        // model input related internal variables
                        SetupEMSInternalVariable("Inlet Temperature for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "[C]",
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).InletTemp);
                        SetupEMSInternalVariable("Inlet Mass Flow Rate for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "[kg/s]",
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).InletMassFlowRate);
                        if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).HowLoadServed != DataPlant::HowMet_NoneDemand) {
                            SetupEMSInternalVariable("Load Request for Plant Connection " + LoopStr,
                                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                     "[W]",
                                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MyLoad);
                        }
                        SetupEMSInternalVariable("Inlet Density for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "[kg/m3]",
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).InletRho);
                        SetupEMSInternalVariable("Inlet Specific Heat for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                 "[J/kg-C]",
                                                 state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).InletCp);
                        // model results related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Outlet Temperature",
                                         "[C]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).OutletTemp);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MassFlowRateRequest);
                        // model initialization and sizing related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Minimum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MassFlowRateMin);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Maximum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MassFlowRateMax);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Design Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).DesignVolumeFlowRate);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Minimum Loading Capacity",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MinLoad);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Maximum Loading Capacity",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).MaxLoad);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Optimal Loading Capacity",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).OptLoad);
                    }
                }

                if (!lAlphaFieldBlanks(27)) {
                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletNodeNum =
                        NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(27),
                                                            ErrorsFound,
                                                            cCurrentModuleObject,
                                                            state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                            DataLoopNode::NodeType_Air,
                                                            DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                            1,
                                                            DataLoopNode::ObjectIsNotParent);
                    // model input related internal variables
                    SetupEMSInternalVariable(
                        "Inlet Temperature for Air Connection", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name, "[C]", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletTemp);
                    SetupEMSInternalVariable("Inlet Mass Flow Rate for Air Connection",
                                             state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                             "[kg/s]",
                                             state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletMassFlowRate);
                    SetupEMSInternalVariable("Inlet Humidity Ratio for Air Connection",
                                             state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                             "[kgWater/kgDryAir]",
                                             state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletHumRat);
                    SetupEMSInternalVariable(
                        "Inlet Density for Air Connection", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name, "[kg/m3]", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletRho);
                    SetupEMSInternalVariable(
                        "Inlet Specific Heat for Air Connection", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name, "[J/kg-C]", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.InletCp);
                }

                if (!lAlphaFieldBlanks(28)) {
                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(28),
                                                                                                    ErrorsFound,
                                                                                                    cCurrentModuleObject,
                                                                                                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                                                                                    DataLoopNode::NodeType_Air,
                                                                                                    DataLoopNode::NodeConnectionType_ReliefAir,
                                                                                                    1,
                                                                                                    DataLoopNode::ObjectIsNotParent);
                    // outlet air node results
                    SetupEMSActuator(
                        "Air Connection", state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name, "Outlet Temperature", "[C]", lDummy, state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.OutletTemp);
                    SetupEMSActuator("Air Connection",
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                     "Outlet Humidity Ratio",
                                     "[kgWater/kgDryAir]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.OutletHumRat);
                    SetupEMSActuator("Air Connection",
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                     "Mass Flow Rate",
                                     "[kg/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Air.OutletMassFlowRate);
                }

                if (!lAlphaFieldBlanks(29)) {
                    WaterManager::SetupTankDemandComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(29),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.SupplyTankID,
                                                           state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.SupplyTankDemandARRID);

                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.SuppliedByWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                     "Supplied Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.SupplyVdotRequest);
                }

                if (!lAlphaFieldBlanks(30)) {
                    WaterManager::SetupTankSupplyComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(30),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.CollectionTankID,
                                                           state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.CollectionTankSupplyARRID);
                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.CollectsToWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                     "Collected Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserPlantComp(CompLoop).Water.CollectedVdot);
                }

                if (!lAlphaFieldBlanks(31)) {

                    state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(31), DataHeatBalance::Zone);
                    if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ZoneNum == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Ambient Zone Name not found = " + cAlphaArgs(31));
                        ErrorsFound = true;
                    } else {
                        state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.DeviceHasInternalGains = true;
                        SetupZoneInternalGain(state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ZoneNum,
                                              cCurrentModuleObject,
                                              cAlphaArgs(1),
                                              DataHeatBalance::IntGainTypeOf_PlantComponentUserDefined,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ReturnAirConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ThermalRadiationGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.LatentGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ReturnAirLatentGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.CarbonDioxideGainRate,
                                              &state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.GenericContamGainRate);

                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Sensible Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Return Air Heat Sensible Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ReturnAirConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Thermal Radiation Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ThermalRadiationGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.LatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Return Air Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.ReturnAirLatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Carbon Dioxide Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.CarbonDioxideGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Name,
                                         "Gaseous Contaminant Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserPlantComp(CompLoop).Zone.GenericContamGainRate);
                    }
                }

                // make sure user has entered at least some erl program managers to actually calculate something
                int MgrCountTest = 0;
                if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).ErlSimProgramMngr > 0) MgrCountTest = 1;
                for (int ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop) {
                    if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).ErlInitProgramMngr > 0) ++MgrCountTest;
                    if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).ErlSimProgramMngr > 0) ++MgrCountTest;
                    if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).initPluginLocation >= 0) ++MgrCountTest;
                    if (state.dataUserDefinedComponents->UserPlantComp(CompLoop).Loop(ConnectionLoop).simPluginLocation >= 0) ++MgrCountTest;
                }
                if (MgrCountTest == 0) {
                    ShowSevereError("Invalid " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                    ShowContinueError("At least one program calling manager is needed.");
                    ErrorsFound = true;
                }
            }
        } // NumUserPlantComps > 0

        if (ErrorsFound) {
            ShowFatalError("GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input.");
        }

        cCurrentModuleObject = "Coil:UserDefined";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);

        cAlphaFieldNames.allocate(NumAlphas);
        cAlphaArgs.allocate(NumAlphas);
        lAlphaFieldBlanks.dimension(NumAlphas, false);
        rNumericArgs.dimension(NumNums, 0.0);

        state.dataUserDefinedComponents->NumUserCoils = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (state.dataUserDefinedComponents->NumUserCoils > 0) {
            state.dataUserDefinedComponents->UserCoil.allocate(state.dataUserDefinedComponents->NumUserCoils);
            state.dataUserDefinedComponents->CheckUserCoilName.dimension(state.dataUserDefinedComponents->NumUserCoils, true);
            for (int CompLoop = 1; CompLoop <= state.dataUserDefinedComponents->NumUserCoils; ++CompLoop) {
                inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, _);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                // ErrorsFound will be set to True if problem was found, left untouched otherwise
                GlobalNames::VerifyUniqueCoilName(cCurrentModuleObject, cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

                state.dataUserDefinedComponents->UserCoil(CompLoop).Name = cAlphaArgs(1);

                // now get program manager for model simulations
                if (!lAlphaFieldBlanks(2)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserCoil(CompLoop).ErlSimProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserCoil(CompLoop).simPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(2));
                        if (state.dataUserDefinedComponents->UserCoil(CompLoop).simPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                // now get program manager for model initializations
                if (!lAlphaFieldBlanks(3)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserCoil(CompLoop).ErlInitProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserCoil(CompLoop).initPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(3));
                        if (state.dataUserDefinedComponents->UserCoil(CompLoop).initPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                int NumAirConnections = std::floor(rNumericArgs(1));
                if ((NumAirConnections >= 1) && (NumAirConnections <= 2)) {
                    state.dataUserDefinedComponents->UserCoil(CompLoop).Air.allocate(NumAirConnections);
                    state.dataUserDefinedComponents->UserCoil(CompLoop).NumAirConnections = NumAirConnections;
                    for (int ConnectionLoop = 1; ConnectionLoop <= NumAirConnections; ++ConnectionLoop) {
                        int aArgCount = (ConnectionLoop - 1) * 2 + 4;
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                                DataLoopNode::NodeType_Air,
                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                1,
                                                                DataLoopNode::ObjectIsNotParent);

                        const auto LoopStr = fmt::to_string(ConnectionLoop);
                        // model input related internal variables
                        SetupEMSInternalVariable("Inlet Temperature for Air Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[C]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletTemp);
                        SetupEMSInternalVariable("Inlet Mass Flow Rate for Air Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[kg/s]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletMassFlowRate);
                        SetupEMSInternalVariable("Inlet Humidity Ratio for Air Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[kgWater/kgDryAir]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletHumRat);
                        SetupEMSInternalVariable("Inlet Density for Air Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[kg/m3]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletRho);
                        SetupEMSInternalVariable("Inlet Specific Heat for Air Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[J/kg-C]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).InletCp);

                        state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).OutletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount + 1),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                                DataLoopNode::NodeType_Air,
                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                1,
                                                                DataLoopNode::ObjectIsNotParent);
                        SetupEMSActuator("Air Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Outlet Temperature",
                                         "[C]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).OutletTemp);
                        SetupEMSActuator("Air Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Outlet Humidity Ratio",
                                         "[kgWater/kgDryAir]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).OutletHumRat);
                        SetupEMSActuator("Air Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Air(ConnectionLoop).OutletMassFlowRate);

                        BranchNodeConnections::TestCompSet(
                            cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(aArgCount), cAlphaArgs(aArgCount + 1), "Air Nodes " + LoopStr);
                    }

                    if (!lAlphaFieldBlanks(8)) {
                        {
                            auto const SELECT_CASE_var(cAlphaArgs(8));

                            if (SELECT_CASE_var == "YES") {
                                state.dataUserDefinedComponents->UserCoil(CompLoop).PlantIsConnected = true;
                            } else if (SELECT_CASE_var == "NO") {
                                state.dataUserDefinedComponents->UserCoil(CompLoop).PlantIsConnected = false;
                            }
                        }

                    } else {
                        state.dataUserDefinedComponents->UserCoil(CompLoop).PlantIsConnected = false;
                    }

                    if (state.dataUserDefinedComponents->UserCoil(CompLoop).PlantIsConnected) { // get input
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.InletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(9),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Inlet,
                                                                                                   2,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(10),
                                                                                                    ErrorsFound,
                                                                                                    cCurrentModuleObject,
                                                                                                    cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeType_Water,
                                                                                                    DataLoopNode::NodeConnectionType_Outlet,
                                                                                                    2,
                                                                                                    DataLoopNode::ObjectIsNotParent);

                        BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(9), cAlphaArgs(10), "Plant Nodes");

                        // this model is only for plant connections that are "Demand"
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.HowLoadServed = DataPlant::HowMet_NoneDemand;
                        // this model is only for plant connections that are needy and turn loop on
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.FlowPriority = DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;

                        // Setup Internal Variables
                        // model input related internal variables
                        SetupEMSInternalVariable(
                            "Inlet Temperature for Plant Connection", state.dataUserDefinedComponents->UserCoil(CompLoop).Name, "[C]", state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.InletTemp);
                        SetupEMSInternalVariable("Inlet Mass Flow Rate for Plant Connection",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                                 "[kg/s]",
                                                 state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.InletMassFlowRate);
                        SetupEMSInternalVariable(
                            "Inlet Density for Plant Connection", state.dataUserDefinedComponents->UserCoil(CompLoop).Name, "[kg/m3]", state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.InletRho);
                        SetupEMSInternalVariable(
                            "Inlet Specific Heat for Plant Connection", state.dataUserDefinedComponents->UserCoil(CompLoop).Name, "[J/kg-C]", state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.InletCp);
                        // model results related actuators
                        SetupEMSActuator(
                            "Plant Connection", state.dataUserDefinedComponents->UserCoil(CompLoop).Name, "Outlet Temperature", "[C]", lDummy, state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.OutletTemp);
                        SetupEMSActuator("Plant Connection",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.MassFlowRateRequest);
                        // model initialization and sizing related actuators
                        SetupEMSActuator("Plant Connection",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Design Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.DesignVolumeFlowRate);

                        SetupEMSActuator("Plant Connection",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Minimum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.MassFlowRateMin);
                        SetupEMSActuator("Plant Connection",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Maximum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Loop.MassFlowRateMax);
                    }

                    if (!lAlphaFieldBlanks(11)) {
                        WaterManager::SetupTankDemandComponent(state, cAlphaArgs(1),
                                                               cCurrentModuleObject,
                                                               cAlphaArgs(11),
                                                               ErrorsFound,
                                                               state.dataUserDefinedComponents->UserCoil(CompLoop).Water.SupplyTankID,
                                                               state.dataUserDefinedComponents->UserCoil(CompLoop).Water.SupplyTankDemandARRID);

                        state.dataUserDefinedComponents->UserCoil(CompLoop).Water.SuppliedByWaterSystem = true;
                        SetupEMSActuator("Water System",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Supplied Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Water.SupplyVdotRequest);
                    }

                    if (!lAlphaFieldBlanks(12)) {
                        WaterManager::SetupTankSupplyComponent(state, cAlphaArgs(1),
                                                               cCurrentModuleObject,
                                                               cAlphaArgs(12),
                                                               ErrorsFound,
                                                               state.dataUserDefinedComponents->UserCoil(CompLoop).Water.CollectionTankID,
                                                               state.dataUserDefinedComponents->UserCoil(CompLoop).Water.CollectionTankSupplyARRID);
                        state.dataUserDefinedComponents->UserCoil(CompLoop).Water.CollectsToWaterSystem = true;
                        SetupEMSActuator("Water System",
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                         "Collected Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserCoil(CompLoop).Water.CollectedVdot);
                    }

                    if (!lAlphaFieldBlanks(13)) {

                        state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(13), DataHeatBalance::Zone);
                        if (state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ZoneNum == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Ambient Zone Name not found = " + cAlphaArgs(13));
                            ErrorsFound = true;
                        } else {
                            state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.DeviceHasInternalGains = true;
                            SetupZoneInternalGain(state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ZoneNum,
                                                  cCurrentModuleObject,
                                                  cAlphaArgs(1),
                                                  DataHeatBalance::IntGainTypeOf_CoilUserDefined,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ConvectionGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ReturnAirConvectionGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ThermalRadiationGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.LatentGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ReturnAirLatentGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.CarbonDioxideGainRate,
                                                  &state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.GenericContamGainRate);

                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Sensible Heat Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ConvectionGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Return Air Heat Sensible Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ReturnAirConvectionGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Thermal Radiation Heat Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ThermalRadiationGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Latent Heat Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.LatentGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Return Air Latent Heat Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.ReturnAirLatentGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Carbon Dioxide Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.CarbonDioxideGainRate);
                            SetupEMSActuator("Component Zone Internal Gain",
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Name,
                                             "Gaseous Contaminant Gain Rate",
                                             "[W]",
                                             lDummy,
                                             state.dataUserDefinedComponents->UserCoil(CompLoop).Zone.GenericContamGainRate);
                        }
                    }
                }
            }

        } // NumUserCoils > 0

        if (ErrorsFound) {
            ShowFatalError("GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input.");
        }
    }

    void GetUserDefinedComponents(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static ObjexxFCL::gio::Fmt fmtLD("*");

        bool ErrorsFound(false);
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        int TotalArgs; // argument for call to GetObjectDefMaxArgs
        Array1D_string cAlphaFieldNames;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        static bool lDummy; // Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference

        if (state.dataUserDefinedComponents->GetPlantCompInput) {
            GetUserDefinedPlantComponents(state);
            state.dataUserDefinedComponents->GetPlantCompInput = false;
        }

        cCurrentModuleObject = "ZoneHVAC:ForcedAir:UserDefined";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);

        cAlphaFieldNames.allocate(NumAlphas);
        cAlphaArgs.allocate(NumAlphas);
        lAlphaFieldBlanks.dimension(NumAlphas, false);
        rNumericArgs.dimension(NumNums, 0.0);

        state.dataUserDefinedComponents->NumUserZoneAir = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (state.dataUserDefinedComponents->NumUserZoneAir > 0) {
            state.dataUserDefinedComponents->UserZoneAirHVAC.allocate(state.dataUserDefinedComponents->NumUserZoneAir);
            state.dataUserDefinedComponents->CheckUserZoneAirName.dimension(state.dataUserDefinedComponents->NumUserZoneAir, true);
            for (int CompLoop = 1; CompLoop <= state.dataUserDefinedComponents->NumUserZoneAir; ++CompLoop) {
                inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, _);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name = cAlphaArgs(1);

                // now get program manager for model simulations
                if (!lAlphaFieldBlanks(2)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ErlSimProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).simPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(2));
                        if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).simPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                // now get program manager for model initializations
                if (!lAlphaFieldBlanks(3)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ErlInitProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).initPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(3));
                        if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).initPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(4),
                                                                                                     ErrorsFound,
                                                                                                     cCurrentModuleObject,
                                                                                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                                                                     DataLoopNode::NodeType_Air,
                                                                                                     DataLoopNode::NodeConnectionType_Inlet,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent);
                // model input related internal variables
                SetupEMSInternalVariable("Inlet Temperature for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[C]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletTemp);
                SetupEMSInternalVariable("Inlet Humidity Ratio for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[kgWater/kgDryAir]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletHumRat);
                SetupEMSInternalVariable("Inlet Density for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[kg/m3]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletRho);
                SetupEMSInternalVariable("Inlet Specific Heat for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[J/kg-C]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletCp);

                SetupEMSInternalVariable("Remaining Sensible Load to Heating Setpoint",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[W]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).RemainingOutputToHeatingSP);
                SetupEMSInternalVariable("Remaining Sensible Load to Cooling Setpoint",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[W]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).RemainingOutputToCoolingSP);
                SetupEMSInternalVariable("Remaining Latent Load to Humidifying Setpoint",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[kg/s]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).RemainingOutputReqToHumidSP);
                SetupEMSInternalVariable("Remaining Latent Load to Dehumidifying Setpoint",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "[kg/s]",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).RemainingOutputReqToDehumidSP);

                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                 "Inlet Mass Flow Rate",
                                 "[kg/s]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.InletMassFlowRate);
                state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(5),
                                                                                                      ErrorsFound,
                                                                                                      cCurrentModuleObject,
                                                                                                      state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                                                                      DataLoopNode::NodeType_Air,
                                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent);
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                 "Outlet Temperature",
                                 "[C]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.OutletTemp);
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                 "Outlet Humidity Ratio",
                                 "[kgWater/kgDryAir]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.OutletHumRat);
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                 "Outlet Mass Flow Rate",
                                 "[kg/s]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).ZoneAir.OutletMassFlowRate);

                if (!lAlphaFieldBlanks(6)) {
                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           cCurrentModuleObject,
                                                                                                           state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                                                                           DataLoopNode::NodeType_Air,
                                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                                           2,
                                                                                                           DataLoopNode::ObjectIsNotParent);
                    // model input related internal variables
                    SetupEMSInternalVariable("Inlet Temperature for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                             "[C]",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletTemp);

                    SetupEMSInternalVariable("Inlet Humidity Ratio for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                             "[kgWater/kgDryAir]",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletHumRat);
                    SetupEMSInternalVariable("Inlet Density for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                             "[kg/m3]",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletRho);
                    SetupEMSInternalVariable("Inlet Specific Heat for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                             "[J/kg-C]",
                                             state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletCp);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Inlet Mass Flow Rate",
                                     "[kg/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletMassFlowRate);
                }

                if (!lAlphaFieldBlanks(7)) {
                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            cCurrentModuleObject,
                                                                                                            state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                                                                            DataLoopNode::NodeType_Air,
                                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                                            2,
                                                                                                            DataLoopNode::ObjectIsNotParent);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Outlet Temperature",
                                     "[C]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.OutletTemp);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Outlet Humidity Ratio",
                                     "[kgWater/kgDryAir]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.OutletHumRat);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Mass Flow Rate",
                                     "[kg/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.OutletMassFlowRate);
                }

                if ((state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.InletNodeNum > 0) && (state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).SourceAir.OutletNodeNum > 0)) {
                    //  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
                }

                int NumPlantConnections = std::floor(rNumericArgs(1));
                state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).NumPlantConnections = NumPlantConnections;
                if ((NumPlantConnections >= 1) && (NumPlantConnections <= 3)) {
                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop.allocate(NumPlantConnections);
                    for (int ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop) {
                        int aArgCount = (ConnectionLoop - 1) * 2 + 8;
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).InletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                (ConnectionLoop + 2),
                                                                DataLoopNode::ObjectIsNotParent);
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).OutletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount + 1),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                (ConnectionLoop + 2),
                                                                DataLoopNode::ObjectIsNotParent);
                        BranchNodeConnections::TestCompSet(
                            cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(aArgCount), cAlphaArgs(aArgCount + 1), "Plant Nodes");
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_NoneDemand;
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).FlowPriority = DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
                        // Setup Internal Variables
                        const auto LoopStr = fmt::to_string(ConnectionLoop);
                        // model input related internal variables
                        SetupEMSInternalVariable("Inlet Temperature for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                 "[C]",
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).InletTemp);
                        SetupEMSInternalVariable("Inlet Mass Flow Rate for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                 "[kg/s]",
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).InletMassFlowRate);
                        SetupEMSInternalVariable("Inlet Density for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                 "[kg/m3]",
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).InletRho);
                        SetupEMSInternalVariable("Inlet Specific Heat for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                                 "[J/kg-C]",
                                                 state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).InletCp);
                        // model results related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Outlet Temperature",
                                         "[C]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).OutletTemp);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).MassFlowRateRequest);
                        // model initialization and sizing related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Minimum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).MassFlowRateMin);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Maximum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).MassFlowRateMax);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Design Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Loop(ConnectionLoop).DesignVolumeFlowRate);
                    }
                }

                if (!lAlphaFieldBlanks(14)) {
                    WaterManager::SetupTankDemandComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(14),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.SupplyTankID,
                                                           state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.SupplyTankDemandARRID);

                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.SuppliedByWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Supplied Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.SupplyVdotRequest);
                }

                if (!lAlphaFieldBlanks(15)) {
                    WaterManager::SetupTankSupplyComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(15),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.CollectionTankID,
                                                           state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.CollectionTankSupplyARRID);
                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.CollectsToWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                     "Collected Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Water.CollectedVdot);
                }

                if (!lAlphaFieldBlanks(16)) {

                    state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(16), DataHeatBalance::Zone);
                    if (state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ZoneNum == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Ambient Zone Name not found = " + cAlphaArgs(16));
                        ErrorsFound = true;
                    } else {
                        state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.DeviceHasInternalGains = true;
                        SetupZoneInternalGain(state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ZoneNum,
                                              cCurrentModuleObject,
                                              cAlphaArgs(1),
                                              DataHeatBalance::IntGainTypeOf_ZoneHVACForcedAirUserDefined,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ReturnAirConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ThermalRadiationGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.LatentGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ReturnAirLatentGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.CarbonDioxideGainRate,
                                              &state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.GenericContamGainRate);

                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Sensible Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Return Air Heat Sensible Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ReturnAirConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Thermal Radiation Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ThermalRadiationGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.LatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Return Air Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ReturnAirLatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Carbon Dioxide Gain Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.CarbonDioxideGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Name,
                                         "Gaseous Contaminant Gain Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.GenericContamGainRate);
                    }
                }
            }
        } // NumUserZoneAir > 0

        if (ErrorsFound) {
            ShowFatalError("GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input.");
        }

        cCurrentModuleObject = "AirTerminal:SingleDuct:UserDefined";

        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);

        cAlphaFieldNames.allocate(NumAlphas);
        cAlphaArgs.allocate(NumAlphas);
        lAlphaFieldBlanks.dimension(NumAlphas, false);
        rNumericArgs.dimension(NumNums, 0.0);

        state.dataUserDefinedComponents->NumUserAirTerminals = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (state.dataUserDefinedComponents->NumUserAirTerminals > 0) {
            state.dataUserDefinedComponents->UserAirTerminal.allocate(state.dataUserDefinedComponents->NumUserAirTerminals);
            state.dataUserDefinedComponents->CheckUserAirTerminal.dimension(state.dataUserDefinedComponents->NumUserAirTerminals, true);
            for (int CompLoop = 1; CompLoop <= state.dataUserDefinedComponents->NumUserAirTerminals; ++CompLoop) {
                inputProcessor->getObjectItem(
                    state, cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, _);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name = cAlphaArgs(1);

                // now get program manager for model simulations
                if (!lAlphaFieldBlanks(2)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).ErlSimProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).simPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(2));
                        if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).simPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                // now get program manager for model initializations
                if (!lAlphaFieldBlanks(3)) {
                    int StackMngrNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), DataRuntimeLanguage::EMSProgramCallManager);
                    if (StackMngrNum > 0) { // found it
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).ErlInitProgramMngr = StackMngrNum;
                    } else {
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).initPluginLocation = EnergyPlus::PluginManagement::pluginManager->getLocationOfUserDefinedPlugin(cAlphaArgs(3));
                        if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).initPluginLocation == -1) {
                            ShowSevereError("Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ShowContinueError("Program Manager Name not found as an EMS Program Manager or a Python Plugin Instance object.");
                            ErrorsFound = true;
                        }
                    }
                }

                state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(4),
                                                                                                     ErrorsFound,
                                                                                                     cCurrentModuleObject,
                                                                                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                                                                     DataLoopNode::NodeType_Air,
                                                                                                     DataLoopNode::NodeConnectionType_Inlet,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent,
                                                                                                     cAlphaFieldNames(4));
                // model input related internal variables
                SetupEMSInternalVariable("Inlet Temperature for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[C]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletTemp);
                SetupEMSInternalVariable("Inlet Humidity Ratio for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[kgWater/kgDryAir]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletHumRat);
                SetupEMSInternalVariable("Inlet Density for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[kg/m3]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletRho);
                SetupEMSInternalVariable("Inlet Specific Heat for Primary Air Connection",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[J/kg-C]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletCp);

                SetupEMSInternalVariable("Remaining Sensible Load to Heating Setpoint",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[W]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).RemainingOutputToHeatingSP);
                SetupEMSInternalVariable("Remaining Sensible Load to Cooling Setpoint",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[W]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).RemainingOutputToCoolingSP);
                SetupEMSInternalVariable("Remaining Latent Load to Humidifying Setpoint",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[kg/s]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).RemainingOutputReqToHumidSP);
                SetupEMSInternalVariable("Remaining Latent Load to Dehumidifying Setpoint",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "[kg/s]",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).RemainingOutputReqToDehumidSP);

                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                 "Inlet Mass Flow Rate",
                                 "[kg/s]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletMassFlowRate);
                state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(5),
                                                                                                      ErrorsFound,
                                                                                                      cCurrentModuleObject,
                                                                                                      state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                                                                      DataLoopNode::NodeType_Air,
                                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent,
                                                                                                      cAlphaFieldNames(5));
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                 "Outlet Temperature",
                                 "[C]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletTemp);
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                 "Outlet Humidity Ratio",
                                 "[kgWater/kgDryAir]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletHumRat);
                SetupEMSActuator("Primary Air Connection",
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                 "Outlet Mass Flow Rate",
                                 "[kg/s]",
                                 lDummy,
                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletMassFlowRate);
                BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Air Nodes");

                int ADUNum = 0;
                for (ADUNum = 1; ADUNum <= DataDefineEquip::NumAirDistUnits; ++ADUNum) {
                    if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum == DataDefineEquip::AirDistUnit(ADUNum).OutletNodeNum) {
                        //        AirDistUnit(ADUNum)%InletNodeNum = IndUnitIUNum)%InletNodeNum
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).ADUNum == 0) {
                    ShowSevereError("GetUserDefinedComponents: No matching Air Distribution Unit for " + cCurrentModuleObject + " = " +
                                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name);
                    ShowContinueError("...should have outlet node=" + DataLoopNode::NodeID(state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum));
                    //          ErrorsFound=.TRUE.
                }

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (int CtrlZone = 1; CtrlZone <= DataGlobals::NumOfZones; ++CtrlZone) {
                    if (!DataZoneEquipment::ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (int SupAirIn = 1; SupAirIn <= DataZoneEquipment::ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum == DataZoneEquipment::ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (DataZoneEquipment::ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(DataLoopNode::NodeID(state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum) +
                                                  " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + cCurrentModuleObject + " = " + state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                DataZoneEquipment::ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.InletNodeNum;
                                DataZoneEquipment::ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).AirLoop.OutletNodeNum;
                            }

                            state.dataUserDefinedComponents->UserAirTerminal(CompLoop).ActualCtrlZoneNum = CtrlZone;
                        }
                    }
                }

                if (!lAlphaFieldBlanks(6)) {
                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           cCurrentModuleObject,
                                                                                                           state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                                                                           DataLoopNode::NodeType_Air,
                                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                                           2,
                                                                                                           DataLoopNode::ObjectIsNotParent,
                                                                                                           cAlphaFieldNames(6));
                    // model input related internal variables
                    SetupEMSInternalVariable("Inlet Temperature for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                             "[C]",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletTemp);

                    SetupEMSInternalVariable("Inlet Humidity Ratio for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                             "[kgWater/kgDryAir]",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletHumRat);
                    SetupEMSInternalVariable("Inlet Density for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                             "[kg/m3]",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletRho);
                    SetupEMSInternalVariable("Inlet Specific Heat for Secondary Air Connection",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                             "[J/kg-C]",
                                             state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletCp);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Inlet Mass Flow Rate",
                                     "[kg/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletMassFlowRate);
                }

                if (!lAlphaFieldBlanks(7)) {
                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            cCurrentModuleObject,
                                                                                                            state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                                                                            DataLoopNode::NodeType_Air,
                                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                                            2,
                                                                                                            DataLoopNode::ObjectIsNotParent,
                                                                                                            cAlphaFieldNames(7));
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Outlet Temperature",
                                     "[C]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.OutletTemp);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Outlet Humidity Ratio",
                                     "[kgWater/kgDryAir]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.OutletHumRat);
                    SetupEMSActuator("Secondary Air Connection",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Mass Flow Rate",
                                     "[kg/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.OutletMassFlowRate);
                }

                if ((state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.InletNodeNum > 0) && (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).SourceAir.OutletNodeNum > 0)) {
                    //  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
                }

                int NumPlantConnections = std::floor(rNumericArgs(1));
                state.dataUserDefinedComponents->UserAirTerminal(CompLoop).NumPlantConnections = NumPlantConnections;
                if ((NumPlantConnections >= 1) && (NumPlantConnections <= 2)) {
                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop.allocate(NumPlantConnections);
                    for (int ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop) {
                        int aArgCount = (ConnectionLoop - 1) * 2 + 8;
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).InletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                (ConnectionLoop + 2),
                                                                DataLoopNode::ObjectIsNotParent,
                                                                cAlphaFieldNames(aArgCount));
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).OutletNodeNum =
                            NodeInputManager::GetOnlySingleNode(state, cAlphaArgs(aArgCount + 1),
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                cAlphaArgs(1),
                                                                DataLoopNode::NodeType_Water,
                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                (ConnectionLoop + 2),
                                                                DataLoopNode::ObjectIsNotParent,
                                                                cAlphaFieldNames(aArgCount + 1));
                        BranchNodeConnections::TestCompSet(
                            cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(aArgCount), cAlphaArgs(aArgCount + 1), "Plant Nodes");
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).HowLoadServed = DataPlant::HowMet_NoneDemand;
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).FlowPriority = DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
                        // Setup Internal Variables
                        const auto LoopStr = fmt::to_string(ConnectionLoop);
                        // model input related internal variables
                        SetupEMSInternalVariable("Inlet Temperature for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                 "[C]",
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).InletTemp);
                        SetupEMSInternalVariable("Inlet Mass Flow Rate for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                 "[kg/s]",
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).InletMassFlowRate);
                        SetupEMSInternalVariable("Inlet Density for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                 "[kg/m3]",
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).InletRho);
                        SetupEMSInternalVariable("Inlet Specific Heat for Plant Connection " + LoopStr,
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                                 "[J/kg-C]",
                                                 state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).InletCp);
                        // model results related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Outlet Temperature",
                                         "[C]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).OutletTemp);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).MassFlowRateRequest);
                        // model initialization and sizing related actuators
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Minimum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).MassFlowRateMin);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Maximum Mass Flow Rate",
                                         "[kg/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).MassFlowRateMax);
                        SetupEMSActuator("Plant Connection " + LoopStr,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Design Volume Flow Rate",
                                         "[m3/s]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Loop(ConnectionLoop).DesignVolumeFlowRate);
                    }
                }

                if (!lAlphaFieldBlanks(12)) {
                    WaterManager::SetupTankDemandComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(12),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.SupplyTankID,
                                                           state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.SupplyTankDemandARRID);

                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.SuppliedByWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Supplied Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.SupplyVdotRequest);
                }

                if (!lAlphaFieldBlanks(13)) {
                    WaterManager::SetupTankSupplyComponent(state, cAlphaArgs(1),
                                                           cCurrentModuleObject,
                                                           cAlphaArgs(13),
                                                           ErrorsFound,
                                                           state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.CollectionTankID,
                                                           state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.CollectionTankSupplyARRID);
                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.CollectsToWaterSystem = true;
                    SetupEMSActuator("Water System",
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                     "Collected Volume Flow Rate",
                                     "[m3/s]",
                                     lDummy,
                                     state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Water.CollectedVdot);
                }

                if (!lAlphaFieldBlanks(14)) {

                    state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(14), DataHeatBalance::Zone);
                    if (state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ZoneNum == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Ambient Zone Name not found = " + cAlphaArgs(14));
                        ErrorsFound = true;
                    } else {
                        state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.DeviceHasInternalGains = true;
                        SetupZoneInternalGain(state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ZoneNum,
                                              cCurrentModuleObject,
                                              cAlphaArgs(1),
                                              DataHeatBalance::IntGainTypeOf_AirTerminalUserDefined,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ReturnAirConvectionGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ThermalRadiationGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.LatentGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ReturnAirLatentGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.CarbonDioxideGainRate,
                                              &state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.GenericContamGainRate);

                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Sensible Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Return Air Heat Sensible Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserZoneAirHVAC(CompLoop).Zone.ReturnAirConvectionGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Thermal Radiation Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ThermalRadiationGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.LatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Return Air Latent Heat Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.ReturnAirLatentGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Carbon Dioxide Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.CarbonDioxideGainRate);
                        SetupEMSActuator("Component Zone Internal Gain",
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Name,
                                         "Gaseous Contaminant Gain Rate",
                                         "[W]",
                                         lDummy,
                                         state.dataUserDefinedComponents->UserAirTerminal(CompLoop).Zone.GenericContamGainRate);
                    }
                }
            }
        } // NumUserZoneAir > 0

        if (ErrorsFound) {
            ShowFatalError("GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input.");
        }
    }

    void UserPlantComponentStruct::initialize(EnergyPlusData &state, int LoopNum, Real64 MyLoad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static std::string const RoutineName("InitPlantUserComponent");

        if (this->myOneTimeFlag) {
            // locate the connections to the plant loops
            for (int ConnectionNum = 1; ConnectionNum <= this->NumPlantConnections; ++ConnectionNum) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        DataPlant::TypeOf_PlantComponentUserDefined,
                                                        this->Loop(ConnectionNum).LoopNum,
                                                        this->Loop(ConnectionNum).LoopSideNum,
                                                        this->Loop(ConnectionNum).BranchNum,
                                                        this->Loop(ConnectionNum).CompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->Loop(ConnectionNum).InletNodeNum);
                if (errFlag) {
                    ShowFatalError("InitPlantUserComponent: Program terminated due to previous condition(s).");
                }

                // set user input for flow priority
                DataPlant::PlantLoop(this->Loop(ConnectionNum).LoopNum)
                    .LoopSide(this->Loop(ConnectionNum).LoopSideNum)
                    .Branch(this->Loop(ConnectionNum).BranchNum)
                    .Comp(this->Loop(ConnectionNum).CompNum)
                    .FlowPriority = this->Loop(ConnectionNum).FlowPriority;

                // set user input for how loads served
                DataPlant::PlantLoop(this->Loop(ConnectionNum).LoopNum)
                    .LoopSide(this->Loop(ConnectionNum).LoopSideNum)
                    .Branch(this->Loop(ConnectionNum).BranchNum)
                    .Comp(this->Loop(ConnectionNum).CompNum)
                    .HowLoadServed = this->Loop(ConnectionNum).HowLoadServed;
            }

            this->myOneTimeFlag = false;
        }

        if (LoopNum <= 0 || LoopNum > this->NumPlantConnections) return;

        // fill internal variable targets
        this->Loop(LoopNum).MyLoad = MyLoad;
        this->Loop(LoopNum).InletRho = FluidProperties::GetDensityGlycol(state, DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum).FluidName,
                                                                         DataLoopNode::Node(this->Loop(LoopNum).InletNodeNum).Temp,
                                                                         DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum).FluidIndex,
                                                                         RoutineName);
        this->Loop(LoopNum).InletCp = FluidProperties::GetSpecificHeatGlycol(state, DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum).FluidName,
                                                                             DataLoopNode::Node(this->Loop(LoopNum).InletNodeNum).Temp,
                                                                             DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum).FluidIndex,
                                                                             RoutineName);
         this->Loop(LoopNum).InletMassFlowRate = DataLoopNode::Node(this->Loop(LoopNum).InletNodeNum).MassFlowRate;
        this->Loop(LoopNum).InletTemp = DataLoopNode::Node(this->Loop(LoopNum).InletNodeNum).Temp;
        if (this->Air.InletNodeNum > 0) {
            this->Air.InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                   DataLoopNode::Node(this->Air.InletNodeNum).Temp,
                                                                   DataLoopNode::Node(this->Air.InletNodeNum).HumRat,
                                                                   RoutineName);
            this->Air.InletCp =
                Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->Air.InletNodeNum).HumRat);
            this->Air.InletTemp = DataLoopNode::Node(this->Air.InletNodeNum).Temp;
            this->Air.InletMassFlowRate = DataLoopNode::Node(this->Air.InletNodeNum).MassFlowRate;
            this->Air.InletHumRat = DataLoopNode::Node(this->Air.InletNodeNum).HumRat;
        }
    }

    void UserCoilComponentStruct::initialize(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static std::string const RoutineName("InitCoilUserDefined");

        if (this->myOneTimeFlag) {
            if (this->PlantIsConnected) {
                bool errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        DataPlant::TypeOf_CoilUserDefined,
                                                        this->Loop.LoopNum,
                                                        this->Loop.LoopSideNum,
                                                        this->Loop.BranchNum,
                                                        this->Loop.CompNum,
                                                        errFlag);
                if (errFlag) {
                    ShowFatalError("InitPlantUserComponent: Program terminated due to previous condition(s).");
                }
                // set user input for flow priority
                DataPlant::PlantLoop(this->Loop.LoopNum)
                    .LoopSide(this->Loop.LoopSideNum)
                    .Branch(this->Loop.BranchNum)
                    .Comp(this->Loop.CompNum)
                    .FlowPriority = this->Loop.FlowPriority;

                // set user input for how loads served
                DataPlant::PlantLoop(this->Loop.LoopNum)
                    .LoopSide(this->Loop.LoopSideNum)
                    .Branch(this->Loop.BranchNum)
                    .Comp(this->Loop.CompNum)
                    .HowLoadServed = this->Loop.HowLoadServed;
            }
            this->myOneTimeFlag = false;
        }

        // fill internal variable targets
        for (int loop = 1; loop <= this->NumAirConnections; ++loop) {
            this->Air(loop).InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                         DataLoopNode::Node(this->Air(loop).InletNodeNum).Temp,
                                                                         DataLoopNode::Node(this->Air(loop).InletNodeNum).HumRat,
                                                                         RoutineName);

            this->Air(loop).InletCp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->Air(loop).InletNodeNum).HumRat);
            this->Air(loop).InletTemp = DataLoopNode::Node(this->Air(loop).InletNodeNum).Temp;
            this->Air(loop).InletMassFlowRate = DataLoopNode::Node(this->Air(loop).InletNodeNum).MassFlowRate;
            this->Air(loop).InletHumRat = DataLoopNode::Node(this->Air(loop).InletNodeNum).HumRat;
        }

        if (this->PlantIsConnected) {
            this->Loop.InletRho = FluidProperties::GetDensityGlycol(state, DataPlant::PlantLoop(this->Loop.LoopNum).FluidName,
                                                                    DataLoopNode::Node(this->Loop.InletNodeNum).Temp,
                                                                    DataPlant::PlantLoop(this->Loop.LoopNum).FluidIndex,
                                                                    RoutineName);
            this->Loop.InletCp = FluidProperties::GetSpecificHeatGlycol(state, DataPlant::PlantLoop(this->Loop.LoopNum).FluidName,
                                                                        DataLoopNode::Node(this->Loop.InletNodeNum).Temp,
                                                                        DataPlant::PlantLoop(this->Loop.LoopNum).FluidIndex,
                                                                        RoutineName);
            this->Loop.InletTemp = DataLoopNode::Node(this->Loop.InletNodeNum).Temp;
            this->Loop.InletMassFlowRate = DataLoopNode::Node(this->Loop.InletNodeNum).MassFlowRate;
        }
    }

    void UserZoneHVACForcedAirComponentStruct::initialize(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // initialize data for user-defined zone HVAC forced air component model

        static std::string const RoutineName("InitZoneAirUserDefined");

        if (this->myOneTimeFlag) {
            if (this->NumPlantConnections > 0) {
                for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                    bool errFlag = false;
                    PlantUtilities::ScanPlantLoopsForObject(state,
                                                            this->Name,
                                                            DataPlant::TypeOf_ZoneHVACAirUserDefined,
                                                            this->Loop(loop).LoopNum,
                                                            this->Loop(loop).LoopSideNum,
                                                            this->Loop(loop).BranchNum,
                                                            this->Loop(loop).CompNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            this->Loop(loop).InletNodeNum);
                    if (errFlag) {
                        ShowFatalError("InitPlantUserComponent: Program terminated due to previous condition(s).");
                    }
                    // set user input for flow priority
                    DataPlant::PlantLoop(this->Loop(loop).LoopNum)
                        .LoopSide(this->Loop(loop).LoopSideNum)
                        .Branch(this->Loop(loop).BranchNum)
                        .Comp(this->Loop(loop).CompNum)
                        .FlowPriority = this->Loop(loop).FlowPriority;

                    // set user input for how loads served
                    DataPlant::PlantLoop(this->Loop(loop).LoopNum)
                        .LoopSide(this->Loop(loop).LoopSideNum)
                        .Branch(this->Loop(loop).BranchNum)
                        .Comp(this->Loop(loop).CompNum)
                        .HowLoadServed = this->Loop(loop).HowLoadServed;
                }
            }
            this->myOneTimeFlag = false;
        }
        // fill internal variable targets
        this->RemainingOutputToHeatingSP = DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        this->RemainingOutputToCoolingSP = DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
        this->RemainingOutputReqToDehumidSP = DataZoneEnergyDemands::ZoneSysMoistureDemand(ZoneNum).RemainingOutputReqToDehumidSP;
        this->RemainingOutputReqToHumidSP = DataZoneEnergyDemands::ZoneSysMoistureDemand(ZoneNum).RemainingOutputReqToHumidSP;

        this->ZoneAir.InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                   DataLoopNode::Node(this->ZoneAir.InletNodeNum).Temp,
                                                                   DataLoopNode::Node(this->ZoneAir.InletNodeNum).HumRat,
                                                                   RoutineName);
        this->ZoneAir.InletCp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->ZoneAir.InletNodeNum).HumRat);
        this->ZoneAir.InletTemp = DataLoopNode::Node(this->ZoneAir.InletNodeNum).Temp;
        this->ZoneAir.InletHumRat = DataLoopNode::Node(this->ZoneAir.InletNodeNum).HumRat;

        if (this->SourceAir.InletNodeNum > 0) {
            this->SourceAir.InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                         DataLoopNode::Node(this->SourceAir.InletNodeNum).Temp,
                                                                         DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat,
                                                                         RoutineName);
            this->SourceAir.InletCp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat);
            this->SourceAir.InletTemp = DataLoopNode::Node(this->SourceAir.InletNodeNum).Temp;
            this->SourceAir.InletHumRat = DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat;
        }

        if (this->NumPlantConnections > 0) {
            for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                this->Loop(loop).InletRho = FluidProperties::GetDensityGlycol(state, DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidName,
                                                                              DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp,
                                                                              DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidIndex,
                                                                              RoutineName);
                this->Loop(loop).InletCp = FluidProperties::GetSpecificHeatGlycol(state, DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidName,
                                                                                  DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp,
                                                                                  DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidIndex,
                                                                                  RoutineName);
                this->Loop(loop).InletTemp = DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp;
                this->Loop(loop).InletMassFlowRate = DataLoopNode::Node(this->Loop(loop).InletNodeNum).MassFlowRate;
            }
        }
    }

    void UserAirTerminalComponentStruct::initialize(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static std::string const RoutineName("InitAirTerminalUserDefined");

        if (this->myOneTimeFlag) {
            if (this->NumPlantConnections > 0) {
                for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                    bool errFlag = false;
                    PlantUtilities::ScanPlantLoopsForObject(state,
                                                            this->Name,
                                                            DataPlant::TypeOf_AirTerminalUserDefined,
                                                            this->Loop(loop).LoopNum,
                                                            this->Loop(loop).LoopSideNum,
                                                            this->Loop(loop).BranchNum,
                                                            this->Loop(loop).CompNum,
                                                            errFlag,
                                                            _,
                                                            _,
                                                            _,
                                                            this->Loop(loop).InletNodeNum);
                    if (errFlag) {
                        ShowFatalError("InitPlantUserComponent: Program terminated due to previous condition(s).");
                    }
                    // set user input for flow priority
                    DataPlant::PlantLoop(this->Loop(loop).LoopNum)
                        .LoopSide(this->Loop(loop).LoopSideNum)
                        .Branch(this->Loop(loop).BranchNum)
                        .Comp(this->Loop(loop).CompNum)
                        .FlowPriority = this->Loop(loop).FlowPriority;

                    // set user input for how loads served
                    DataPlant::PlantLoop(this->Loop(loop).LoopNum)
                        .LoopSide(this->Loop(loop).LoopSideNum)
                        .Branch(this->Loop(loop).BranchNum)
                        .Comp(this->Loop(loop).CompNum)
                        .HowLoadServed = this->Loop(loop).HowLoadServed;
                }
            }
            this->myOneTimeFlag = false;
        }
        // fill internal variable targets
        this->RemainingOutputToHeatingSP = DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        this->RemainingOutputToCoolingSP = DataZoneEnergyDemands::ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
        this->RemainingOutputReqToDehumidSP = DataZoneEnergyDemands::ZoneSysMoistureDemand(ZoneNum).RemainingOutputReqToDehumidSP;
        this->RemainingOutputReqToHumidSP = DataZoneEnergyDemands::ZoneSysMoistureDemand(ZoneNum).RemainingOutputReqToHumidSP;

        this->AirLoop.InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                   DataLoopNode::Node(this->AirLoop.InletNodeNum).Temp,
                                                                   DataLoopNode::Node(this->AirLoop.InletNodeNum).HumRat,
                                                                   RoutineName);
        this->AirLoop.InletCp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->AirLoop.InletNodeNum).HumRat);
        this->AirLoop.InletTemp = DataLoopNode::Node(this->AirLoop.InletNodeNum).Temp;
        this->AirLoop.InletHumRat = DataLoopNode::Node(this->AirLoop.InletNodeNum).HumRat;

        if (this->SourceAir.InletNodeNum > 0) {
            this->SourceAir.InletRho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress,
                                                                         DataLoopNode::Node(this->SourceAir.InletNodeNum).Temp,
                                                                         DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat,
                                                                         RoutineName);
            this->SourceAir.InletCp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat);
            this->SourceAir.InletTemp = DataLoopNode::Node(this->SourceAir.InletNodeNum).Temp;
            this->SourceAir.InletHumRat = DataLoopNode::Node(this->SourceAir.InletNodeNum).HumRat;
        }

        if (this->NumPlantConnections > 0) {
            for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                this->Loop(loop).InletRho = FluidProperties::GetDensityGlycol(state, DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidName,
                                                                              DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp,
                                                                              DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidIndex,
                                                                              RoutineName);
                this->Loop(loop).InletCp = FluidProperties::GetSpecificHeatGlycol(state, DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidName,
                                                                                  DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp,
                                                                                  DataPlant::PlantLoop(this->Loop(loop).LoopNum).FluidIndex,
                                                                                  RoutineName);
                this->Loop(loop).InletTemp = DataLoopNode::Node(this->Loop(loop).InletNodeNum).Temp;
                this->Loop(loop).InletMassFlowRate = DataLoopNode::Node(this->Loop(loop).InletNodeNum).MassFlowRate;
            }
        }
    }

    void UserPlantComponentStruct::report(int const LoopNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // report model results

        // METHODOLOGY EMPLOYED:
        // copy actuated values to structures elsewhere in program.

        PlantUtilities::SafeCopyPlantNode(this->Loop(LoopNum).InletNodeNum, this->Loop(LoopNum).OutletNodeNum);

        // unload Actuators to node data structure

        DataLoopNode::Node(this->Loop(LoopNum).OutletNodeNum).Temp = this->Loop(LoopNum).OutletTemp;

        // make mass flow requests, just this loop
        PlantUtilities::SetComponentFlowRate(this->Loop(LoopNum).MassFlowRateRequest,
                                             this->Loop(LoopNum).InletNodeNum,
                                             this->Loop(LoopNum).OutletNodeNum,
                                             this->Loop(LoopNum).LoopNum,
                                             this->Loop(LoopNum).LoopSideNum,
                                             this->Loop(LoopNum).BranchNum,
                                             this->Loop(LoopNum).CompNum);

        if (this->Air.OutletNodeNum > 0) {
            DataLoopNode::Node(this->Air.OutletNodeNum).Temp = this->Air.OutletTemp;
            DataLoopNode::Node(this->Air.OutletNodeNum).HumRat = this->Air.OutletHumRat;
            DataLoopNode::Node(this->Air.OutletNodeNum).MassFlowRate = this->Air.OutletMassFlowRate;
            DataLoopNode::Node(this->Air.OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->Air.OutletTemp, this->Air.OutletHumRat);
        }

        if (this->Water.SuppliedByWaterSystem) {
            DataWater::WaterStorage(this->Water.SupplyTankID).VdotRequestDemand(this->Water.SupplyTankDemandARRID) = this->Water.SupplyVdotRequest;
        }

        if (this->Water.CollectsToWaterSystem) {
            DataWater::WaterStorage(this->Water.CollectionTankID).VdotAvailSupply(this->Water.CollectionTankSupplyARRID) = this->Water.CollectedVdot;
        }

        if (this->Loop(LoopNum).HowLoadServed == DataPlant::HowMet_ByNominalCapLowOutLimit) {
            DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum)
                .LoopSide(this->Loop(LoopNum).LoopSideNum)
                .Branch(this->Loop(LoopNum).BranchNum)
                .Comp(this->Loop(LoopNum).CompNum)
                .MinOutletTemp = this->Loop(LoopNum).LowOutTempLimit;
        }

        if (this->Loop(LoopNum).HowLoadServed == DataPlant::HowMet_ByNominalCapHiOutLimit) {
            DataPlant::PlantLoop(this->Loop(LoopNum).LoopNum)
                .LoopSide(this->Loop(LoopNum).LoopSideNum)
                .Branch(this->Loop(LoopNum).BranchNum)
                .Comp(this->Loop(LoopNum).CompNum)
                .MaxOutletTemp = this->Loop(LoopNum).HiOutTempLimit;
        }
    }

    void UserCoilComponentStruct::report()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // report model outputs

        for (int loop = 1; loop <= this->NumAirConnections; ++loop) {
            if (this->Air(loop).OutletNodeNum > 0) {
                DataLoopNode::Node(this->Air(loop).OutletNodeNum).Temp = this->Air(loop).OutletTemp;
                DataLoopNode::Node(this->Air(loop).OutletNodeNum).HumRat = this->Air(loop).OutletHumRat;
                DataLoopNode::Node(this->Air(loop).OutletNodeNum).MassFlowRate = this->Air(loop).OutletMassFlowRate;
                DataLoopNode::Node(this->Air(loop).OutletNodeNum).Enthalpy =
                    Psychrometrics::PsyHFnTdbW(this->Air(loop).OutletTemp, this->Air(loop).OutletHumRat);

                DataLoopNode::Node(this->Air(loop).OutletNodeNum).MassFlowRateMinAvail =
                    DataLoopNode::Node(this->Air(loop).InletNodeNum).MassFlowRateMinAvail;
                DataLoopNode::Node(this->Air(loop).OutletNodeNum).MassFlowRateMaxAvail =
                    DataLoopNode::Node(this->Air(loop).InletNodeNum).MassFlowRateMaxAvail;
            }
        }

        if (this->PlantIsConnected) {
            // make mass flow requests
            PlantUtilities::SetComponentFlowRate(this->Loop.MassFlowRateRequest,
                                                 this->Loop.InletNodeNum,
                                                 this->Loop.OutletNodeNum,
                                                 this->Loop.LoopNum,
                                                 this->Loop.LoopSideNum,
                                                 this->Loop.BranchNum,
                                                 this->Loop.CompNum);
            PlantUtilities::SafeCopyPlantNode(this->Loop.InletNodeNum, this->Loop.OutletNodeNum);
            // unload Actuators to node data structure
            DataLoopNode::Node(this->Loop.OutletNodeNum).Temp = this->Loop.OutletTemp;
        }

        if (this->Water.SuppliedByWaterSystem) {
            DataWater::WaterStorage(this->Water.SupplyTankID).VdotRequestDemand(this->Water.SupplyTankDemandARRID) = this->Water.SupplyVdotRequest;
        }

        if (this->Water.CollectsToWaterSystem) {
            DataWater::WaterStorage(this->Water.CollectionTankID).VdotAvailSupply(this->Water.CollectionTankSupplyARRID) = this->Water.CollectedVdot;
        }
    }

    void UserZoneHVACForcedAirComponentStruct::report()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // report model outputs

        DataLoopNode::Node(this->ZoneAir.InletNodeNum).MassFlowRate = this->ZoneAir.InletMassFlowRate;

        DataLoopNode::Node(this->ZoneAir.OutletNodeNum).Temp = this->ZoneAir.OutletTemp;
        DataLoopNode::Node(this->ZoneAir.OutletNodeNum).HumRat = this->ZoneAir.OutletHumRat;
        DataLoopNode::Node(this->ZoneAir.OutletNodeNum).MassFlowRate = this->ZoneAir.OutletMassFlowRate;
        DataLoopNode::Node(this->ZoneAir.OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->ZoneAir.OutletTemp, this->ZoneAir.OutletHumRat);

        if (this->SourceAir.OutletNodeNum > 0) {
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).Temp = this->SourceAir.OutletTemp;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).HumRat = this->SourceAir.OutletHumRat;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).MassFlowRate = this->SourceAir.OutletMassFlowRate;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).Enthalpy =
                Psychrometrics::PsyHFnTdbW(this->SourceAir.OutletTemp, this->SourceAir.OutletHumRat);
        }

        if (this->NumPlantConnections > 0) {
            for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                // make mass flow requests
                PlantUtilities::SetComponentFlowRate(this->Loop(loop).MassFlowRateRequest,
                                                     this->Loop(loop).InletNodeNum,
                                                     this->Loop(loop).OutletNodeNum,
                                                     this->Loop(loop).LoopNum,
                                                     this->Loop(loop).LoopSideNum,
                                                     this->Loop(loop).BranchNum,
                                                     this->Loop(loop).CompNum);
                PlantUtilities::SafeCopyPlantNode(this->Loop(loop).InletNodeNum, this->Loop(loop).OutletNodeNum);
                // unload Actuators to node data structure
                DataLoopNode::Node(this->Loop(loop).OutletNodeNum).Temp = this->Loop(loop).OutletTemp;
            }
        }

        if (this->Water.SuppliedByWaterSystem) {
            DataWater::WaterStorage(this->Water.SupplyTankID).VdotRequestDemand(this->Water.SupplyTankDemandARRID) = this->Water.SupplyVdotRequest;
        }

        if (this->Water.CollectsToWaterSystem) {
            DataWater::WaterStorage(this->Water.CollectionTankID).VdotAvailSupply(this->Water.CollectionTankSupplyARRID) = this->Water.CollectedVdot;
        }
    }

    void UserAirTerminalComponentStruct::report()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        DataLoopNode::Node(this->AirLoop.InletNodeNum).MassFlowRate = this->AirLoop.InletMassFlowRate;

        DataLoopNode::Node(this->AirLoop.OutletNodeNum).Temp = this->AirLoop.OutletTemp;
        DataLoopNode::Node(this->AirLoop.OutletNodeNum).HumRat = this->AirLoop.OutletHumRat;
        DataLoopNode::Node(this->AirLoop.OutletNodeNum).MassFlowRate = this->AirLoop.OutletMassFlowRate;
        DataLoopNode::Node(this->AirLoop.OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->AirLoop.OutletTemp, this->AirLoop.OutletHumRat);
        if (this->SourceAir.OutletNodeNum > 0) {
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).Temp = this->SourceAir.OutletTemp;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).HumRat = this->SourceAir.OutletHumRat;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).MassFlowRate = this->SourceAir.OutletMassFlowRate;
            DataLoopNode::Node(this->SourceAir.OutletNodeNum).Enthalpy =
                Psychrometrics::PsyHFnTdbW(this->SourceAir.OutletTemp, this->SourceAir.OutletHumRat);
        }

        if (this->NumPlantConnections > 0) {
            for (int loop = 1; loop <= this->NumPlantConnections; ++loop) {
                // make mass flow requests
                PlantUtilities::SetComponentFlowRate(this->Loop(loop).MassFlowRateRequest,
                                                     this->Loop(loop).InletNodeNum,
                                                     this->Loop(loop).OutletNodeNum,
                                                     this->Loop(loop).LoopNum,
                                                     this->Loop(loop).LoopSideNum,
                                                     this->Loop(loop).BranchNum,
                                                     this->Loop(loop).CompNum);
                PlantUtilities::SafeCopyPlantNode(this->Loop(loop).InletNodeNum, this->Loop(loop).OutletNodeNum);
                // unload Actuators to node data structure
                DataLoopNode::Node(this->Loop(loop).OutletNodeNum).Temp = this->Loop(loop).OutletTemp;
            }
        }

        if (this->Water.SuppliedByWaterSystem) {
            DataWater::WaterStorage(this->Water.SupplyTankID).VdotRequestDemand(this->Water.SupplyTankDemandARRID) = this->Water.SupplyVdotRequest;
        }

        if (this->Water.CollectsToWaterSystem) {
            DataWater::WaterStorage(this->Water.CollectionTankID).VdotAvailSupply(this->Water.CollectionTankSupplyARRID) = this->Water.CollectedVdot;
        }
    }

    void GetUserDefinedCoilIndex(EnergyPlusData &state, std::string const &CoilName, int &CoilIndex, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets an index for a given user defined Cooling Coil -- issues error message if that
        // coil is not a legal user defined Cooling Coil.

        // Obtains and allocates TESCoil related parameters from input file
        if (state.dataUserDefinedComponents->GetInput) { // First time subroutine has been called, get input data
            GetUserDefinedComponents(state);
            state.dataUserDefinedComponents->GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataUserDefinedComponents->NumUserCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, state.dataUserDefinedComponents->UserCoil, state.dataUserDefinedComponents->NumUserCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetUserDefinedCoilIndex: User Defined Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
        }
    }

    void GetUserDefinedCoilAirInletNode(EnergyPlusData &state, std::string const &CoilName, int &CoilAirInletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given user defined Cooling Coil's air inlet node -- issues error message if that
        // coil is not a legal user defined Cooling Coil and sets air node to 0, otherwise, returns inlet air node number.

        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (state.dataUserDefinedComponents->GetInput) { // First time subroutine has been called, get input data
            GetUserDefinedComponents(state);
            state.dataUserDefinedComponents->GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataUserDefinedComponents->NumUserCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, state.dataUserDefinedComponents->UserCoil, state.dataUserDefinedComponents->NumUserCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilAirInletNode = 0;
        } else {
            CoilAirInletNode = state.dataUserDefinedComponents->UserCoil(CoilIndex).Air(1).InletNodeNum;
        }
    }

    void
    GetUserDefinedCoilAirOutletNode(EnergyPlusData &state, std::string const &CoilName, int &CoilAirOutletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given user defined Cooling Coil's air outlet node -- issues error message if that
        // coil is not a legal user defined Cooling Coil and sets air node to 0, otherwise, returns outlet air node number.

        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (state.dataUserDefinedComponents->GetInput) { // First time subroutine has been called, get input data
            GetUserDefinedComponents(state);
            state.dataUserDefinedComponents->GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (state.dataUserDefinedComponents->NumUserCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, state.dataUserDefinedComponents->UserCoil, state.dataUserDefinedComponents->NumUserCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilAirOutletNode = 0;
        } else {
            CoilAirOutletNode = state.dataUserDefinedComponents->UserCoil(CoilIndex).Air(1).OutletNodeNum;
        }
    }

} // namespace UserDefinedComponents

} // namespace EnergyPlus
