// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/EquipAndOperations.hh>
#include <EnergyPlus/Plant/Loop.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {
namespace DataPlant {

    void ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme(EnergyPlusData &state)
    {
        if (this->oneTimeSetupComplete) return;

        SetupOutputVariable(state,
                            "Supervisory Plant Operation Mode",
                            OutputProcessor::Unit::unknown,
                            this->Report.OutputOpMode,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Supervisory Plant Operation Sensed Heating Load",
                            OutputProcessor::Unit::W,
                            this->Report.SensedHeatingLoad,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Supervisory Plant Operation Sensed Cooling Load",
                            OutputProcessor::Unit::W,
                            this->Report.SensedCoolingLoad,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        // routine for setup of chiller heater supervisory plant operation scheme
        for (int zoneListNum = 1; zoneListNum <= state.dataHeatBal->NumOfZoneLists; ++zoneListNum) {
            if (this->ZoneListName == state.dataHeatBal->ZoneList(zoneListNum).Name) {

                this->PlantOps.NumOfZones = state.dataHeatBal->ZoneList(zoneListNum).NumOfZones;
                this->ZonePtrs.allocate(this->PlantOps.NumOfZones);
                for (int zoneNumInList = 1; zoneNumInList <= state.dataHeatBal->ZoneList(zoneListNum).NumOfZones; ++zoneNumInList) {
                    this->ZonePtrs(zoneNumInList) = state.dataHeatBal->ZoneList(zoneListNum).Zone(zoneNumInList);
                }
            }
        }

        if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
            this->AirLoopPtrs.allocate(state.dataHVACGlobal->NumPrimaryAirSys); // size to all, if zero then that airloop is not served
            this->PlantOps.NumOfAirLoops = state.dataHVACGlobal->NumPrimaryAirSys;
            this->AirLoopPtrs = 0;
            for (int AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the air systems
                auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex));
                for (int ZonesPolledIndex = 1; ZonesPolledIndex <= this->PlantOps.NumOfZones; ++ZonesPolledIndex) {
                    for (int ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo.NumZonesCooled; ++ZonesCooledIndex) {
                        if (AirToZoneNodeInfo.CoolCtrlZoneNums(ZonesCooledIndex) == this->ZonePtrs(ZonesPolledIndex)) {
                            this->AirLoopPtrs(AirLoopIndex) = AirLoopIndex;
                        }
                    }
                    for (int ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo.NumZonesHeated; ++ZonesHeatedIndex) {
                        if (AirToZoneNodeInfo.HeatCtrlZoneNums(ZonesHeatedIndex) == this->ZonePtrs(ZonesPolledIndex)) {
                            this->AirLoopPtrs(AirLoopIndex) = AirLoopIndex;
                        }
                    }
                }
            }
        }

        if (this->PlantOps.NumSimultHeatCoolHeatingEquipLists > 0 && this->PlantOps.NumSimultHeatCoolCoolingEquipLists > 0) {
            this->PlantOps.SimultHeatCoolOpAvailable = true;
        }

        this->PlantLoopIndicesBeingSupervised.allocate(state.dataPlnt->TotNumLoops);
        this->PlantLoopIndicesBeingSupervised = 0;
        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            auto &this_plant_loop(state.dataPlnt->PlantLoop(LoopNum));
            for (int OpNum = 1, OpNum_end = this_plant_loop.NumOpSchemes; OpNum <= OpNum_end; ++OpNum) {
                auto &this_op_scheme(this_plant_loop.OpScheme(OpNum));
                if (this_op_scheme.Type == OpScheme::ChillerHeaterSupervisory) {
                    this->PlantLoopIndicesBeingSupervised(LoopNum) = LoopNum;
                }
            }
        }

        int numLoadProfileOnSupervisedLoops = 0;
        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                // search for any plant load profile on loop demand side
                auto &this_plant_loopside(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DataPlant::LoopSideLocation::Demand));
                for (int BranchNum = 1; BranchNum <= this_plant_loopside.TotalBranches; ++BranchNum) {
                    for (int CompNum = 1; CompNum <= this_plant_loopside.Branch(BranchNum).TotalComponents; ++CompNum) {
                        if (this_plant_loopside.Branch(BranchNum).Comp(CompNum).Type == DataPlant::PlantEquipmentType::PlantLoadProfile) {
                            ++numLoadProfileOnSupervisedLoops;
                        }
                    }
                }
            }
        }
        this->PlantOps.numPlantLoadProfiles = numLoadProfileOnSupervisedLoops;

        this->PlantLoadProfileComps.allocate(this->PlantOps.numPlantLoadProfiles);
        int loadProfileCompNum = 1;
        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                // search for any plant load profile on loop demand side
                auto &this_plant_loopside(state.dataPlnt->PlantLoop(LoopNum).LoopSide(DataPlant::LoopSideLocation::Demand));
                for (int BranchNum = 1; BranchNum <= this_plant_loopside.TotalBranches; ++BranchNum) {
                    for (int CompNum = 1; CompNum <= this_plant_loopside.Branch(BranchNum).TotalComponents; ++CompNum) {
                        if (this_plant_loopside.Branch(BranchNum).Comp(CompNum).Type == DataPlant::PlantEquipmentType::PlantLoadProfile) {
                            PlantLocation foundLoc;
                            foundLoc.loopNum = LoopNum;
                            foundLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
                            foundLoc.branchNum = BranchNum;
                            foundLoc.compNum = CompNum;
                            PlantLoadProfileComps(loadProfileCompNum) = foundLoc;
                            ++loadProfileCompNum;
                        }
                    }
                }
            }
        }

        // setup Comp.SetPointNodeNum for machines
        if (this->PlantOps.NumCoolingOnlyEquipLists > 0) {
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumCoolingOnlyEquipLists; ++equipListNum) {

                int NumComps = this->CoolingOnlyEquipList(equipListNum).NumComps;
                for (int compNum = 1; compNum <= NumComps; ++compNum) {
                    auto &this_equip(this->CoolingOnlyEquipList(equipListNum).Comp(compNum));
                    PlantLocation compLoc;
                    DataPlant::PlantEquipmentType Type = static_cast<DataPlant::PlantEquipmentType>(
                        getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase(this_equip.TypeOf)));
                    bool errFlag1(false);
                    int NumSearchResults(0);
                    PlantUtilities::ScanPlantLoopsForObject(state, this_equip.Name, Type, compLoc, errFlag1, _, _, NumSearchResults);
                    if (NumSearchResults == 1) {

                        this_equip.LoopNumPtr = compLoc.loopNum;
                        this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                        this_equip.BranchNumPtr = compLoc.branchNum;
                        this_equip.CompNumPtr = compLoc.compNum;

                    } else if (NumSearchResults > 1) {
                        bool foundit = false;
                        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                                int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                                if (PltSizNum > 0) {
                                    if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Cooling) {
                                        int innerNumSearchResults = 0;
                                        PlantUtilities::ScanPlantLoopsForObject(
                                            state, this_equip.Name, Type, compLoc, errFlag1, _, _, innerNumSearchResults, _, LoopNum);
                                        if (innerNumSearchResults == 1) {
                                            this_equip.LoopNumPtr = compLoc.loopNum;
                                            this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                                            this_equip.BranchNumPtr = compLoc.branchNum;
                                            this_equip.CompNumPtr = compLoc.compNum;
                                            foundit = true;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        if (!foundit) {
                            ShowSevereError(state,
                                            format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                                   "component \"{}\" was not found on a cooling plant loop.",
                                                   this->Name,
                                                   this_equip.Name));
                        }
                    } else if (NumSearchResults == 0) {
                        ShowSevereError(state,
                                        format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                               "component \"{}\" was not found on a plant loop.",
                                               this->Name,
                                               this_equip.Name));
                    }
                    int inletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                        .LoopSide(this_equip.LoopSideNumPtr)
                                        .Branch(this_equip.BranchNumPtr)
                                        .Comp(this_equip.CompNumPtr)
                                        .NodeNumIn;
                    int outletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                         .LoopSide(this_equip.LoopSideNumPtr)
                                         .Branch(this_equip.BranchNumPtr)
                                         .Comp(this_equip.CompNumPtr)
                                         .NodeNumOut;
                    this_equip.DemandNodeNum = inletNode;
                    this_equip.SetPointNodeNum = outletNode;
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme.allocate(1);
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme(1)
                        .OpSchemePtr = 1; // TODO check
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .CurOpSchemeType = this->Type;
                }
            }
        }
        int HWPlantIndex = 0;
        if (this->PlantOps.NumHeatingOnlyEquipLists > 0) {
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumHeatingOnlyEquipLists; ++equipListNum) {

                int NumComps = this->HeatingOnlyEquipList(equipListNum).NumComps;
                for (int compNum = 1; compNum <= NumComps; ++compNum) {
                    auto &this_equip(this->HeatingOnlyEquipList(equipListNum).Comp(compNum));
                    PlantLocation compLoc;
                    DataPlant::PlantEquipmentType Type;
                    Type = static_cast<DataPlant::PlantEquipmentType>(
                        getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase(this_equip.TypeOf)));
                    bool errFlag1(false);
                    int NumSearchResults(0);
                    PlantUtilities::ScanPlantLoopsForObject(state, this_equip.Name, Type, compLoc, errFlag1, _, _, NumSearchResults);
                    if (NumSearchResults == 1) {

                        this_equip.LoopNumPtr = compLoc.loopNum;
                        this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                        this_equip.BranchNumPtr = compLoc.branchNum;
                        this_equip.CompNumPtr = compLoc.compNum;

                    } else if (NumSearchResults > 1) {

                        bool foundit = false;
                        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                                int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                                if (PltSizNum > 0) {
                                    if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Heating) {
                                        int innerNumSearchResults = 0;
                                        PlantUtilities::ScanPlantLoopsForObject(
                                            state, this_equip.Name, Type, compLoc, errFlag1, _, _, innerNumSearchResults, _, LoopNum);
                                        if (innerNumSearchResults == 1) {
                                            this_equip.LoopNumPtr = compLoc.loopNum;
                                            this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                                            this_equip.BranchNumPtr = compLoc.branchNum;
                                            this_equip.CompNumPtr = compLoc.compNum;
                                            foundit = true;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        if (!foundit) {
                            ShowSevereError(state,
                                            format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                                   "component \"{}\" was not found on a heating plant loop.",
                                                   this->Name,
                                                   this_equip.Name));
                        }
                    } else if (NumSearchResults == 0) {
                        ShowSevereError(state,
                                        format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                               "component \"{}\" was not found on a plant loop.",
                                               this->Name,
                                               this_equip.Name));
                    }
                    int inletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                        .LoopSide(this_equip.LoopSideNumPtr)
                                        .Branch(this_equip.BranchNumPtr)
                                        .Comp(this_equip.CompNumPtr)
                                        .NodeNumIn;
                    int outletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                         .LoopSide(this_equip.LoopSideNumPtr)
                                         .Branch(this_equip.BranchNumPtr)
                                         .Comp(this_equip.CompNumPtr)
                                         .NodeNumOut;
                    this_equip.DemandNodeNum = inletNode;
                    this_equip.SetPointNodeNum = outletNode;
                    HWPlantIndex = this_equip.LoopNumPtr;
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme.allocate(1);
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme(1)
                        .OpSchemePtr = 1; // TODO check
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .CurOpSchemeType = this->Type;
                }
            }
        }

        if (this->PlantOps.NumSimultHeatCoolCoolingEquipLists > 0) {
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolCoolingEquipLists; ++equipListNum) {

                int NumComps = this->SimultHeatCoolCoolingEquipList(equipListNum).NumComps;
                for (int compNum = 1; compNum <= NumComps; ++compNum) {
                    auto &this_equip(this->SimultHeatCoolCoolingEquipList(equipListNum).Comp(compNum));
                    PlantLocation compLoc;
                    DataPlant::PlantEquipmentType Type;
                    Type = static_cast<DataPlant::PlantEquipmentType>(
                        getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase(this_equip.TypeOf)));
                    bool errFlag1(false);
                    int NumSearchResults(0);
                    PlantUtilities::ScanPlantLoopsForObject(state, this_equip.Name, Type, compLoc, errFlag1, _, _, NumSearchResults);
                    if (NumSearchResults == 1) {

                        this_equip.LoopNumPtr = compLoc.loopNum;
                        this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                        this_equip.BranchNumPtr = compLoc.branchNum;
                        this_equip.CompNumPtr = compLoc.compNum;

                    } else if (NumSearchResults > 1) {

                        bool foundit = false;
                        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                                int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                                if (PltSizNum > 0) {
                                    if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Cooling) {
                                        int innerNumSearchResults = 0;
                                        PlantUtilities::ScanPlantLoopsForObject(
                                            state, this_equip.Name, Type, compLoc, errFlag1, _, _, innerNumSearchResults, _, LoopNum);
                                        if (innerNumSearchResults == 1) {
                                            this_equip.LoopNumPtr = compLoc.loopNum;
                                            this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                                            this_equip.BranchNumPtr = compLoc.branchNum;
                                            this_equip.CompNumPtr = compLoc.compNum;
                                            foundit = true;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        if (!foundit) {
                            ShowSevereError(state,
                                            format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                                   "component \"{}\" was not found on a cooling plant loop.",
                                                   this->Name,
                                                   this_equip.Name));
                        }
                    } else if (NumSearchResults == 0) {
                        ShowSevereError(state,
                                        format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                               "component \"{}\" was not found on a plant loop.",
                                               this->Name,
                                               this_equip.Name));
                    }
                    int inletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                        .LoopSide(this_equip.LoopSideNumPtr)
                                        .Branch(this_equip.BranchNumPtr)
                                        .Comp(this_equip.CompNumPtr)
                                        .NodeNumIn;
                    int outletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                         .LoopSide(this_equip.LoopSideNumPtr)
                                         .Branch(this_equip.BranchNumPtr)
                                         .Comp(this_equip.CompNumPtr)
                                         .NodeNumOut;
                    this_equip.DemandNodeNum = inletNode;
                    this_equip.SetPointNodeNum = outletNode;
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme.allocate(1);
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme(1)
                        .OpSchemePtr = 1; // TODO check
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .CurOpSchemeType = this->Type;
                }
            }
        }

        if (this->PlantOps.NumSimultHeatCoolHeatingEquipLists > 0) {
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolHeatingEquipLists; ++equipListNum) {

                int NumComps = this->SimultHeatCoolHeatingEquipList(equipListNum).NumComps;
                for (int compNum = 1; compNum <= NumComps; ++compNum) {
                    auto &this_equip(this->SimultHeatCoolHeatingEquipList(equipListNum).Comp(compNum));
                    PlantLocation compLoc;
                    DataPlant::PlantEquipmentType Type;
                    Type = static_cast<DataPlant::PlantEquipmentType>(
                        getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase(this_equip.TypeOf)));
                    bool errFlag1(false);
                    int NumSearchResults(0);
                    PlantUtilities::ScanPlantLoopsForObject(state, this_equip.Name, Type, compLoc, errFlag1, _, _, NumSearchResults);
                    if (NumSearchResults == 1) {

                        this_equip.LoopNumPtr = compLoc.loopNum;
                        this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                        this_equip.BranchNumPtr = compLoc.branchNum;
                        this_equip.CompNumPtr = compLoc.compNum;

                    } else if (NumSearchResults > 1) {

                        bool foundit = false;
                        for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                            if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                                int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                                if (PltSizNum > 0) {
                                    if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Heating) {
                                        int innerNumSearchResults = 0;
                                        PlantUtilities::ScanPlantLoopsForObject(
                                            state, this_equip.Name, Type, compLoc, errFlag1, _, _, innerNumSearchResults, _, LoopNum);
                                        if (innerNumSearchResults == 1) {
                                            this_equip.LoopNumPtr = compLoc.loopNum;
                                            this_equip.LoopSideNumPtr = compLoc.loopSideNum;
                                            this_equip.BranchNumPtr = compLoc.branchNum;
                                            this_equip.CompNumPtr = compLoc.compNum;
                                            foundit = true;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        if (!foundit) {
                            ShowSevereError(state,
                                            format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                                   "component \"{}\" was not found on a heating plant loop.",
                                                   this->Name,
                                                   this_equip.Name));
                        }
                    } else if (NumSearchResults == 0) {
                        ShowSevereError(state,
                                        format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" "
                                               "component \"{}\" was not found on a plant loop.",
                                               this->Name,
                                               this_equip.Name));
                    }
                    int inletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                        .LoopSide(this_equip.LoopSideNumPtr)
                                        .Branch(this_equip.BranchNumPtr)
                                        .Comp(this_equip.CompNumPtr)
                                        .NodeNumIn;
                    int outletNode = state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                         .LoopSide(this_equip.LoopSideNumPtr)
                                         .Branch(this_equip.BranchNumPtr)
                                         .Comp(this_equip.CompNumPtr)
                                         .NodeNumOut;
                    this_equip.DemandNodeNum = inletNode;
                    this_equip.SetPointNodeNum = outletNode;
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme.allocate(1);
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .OpScheme(1)
                        .OpSchemePtr = 1; // TODO check
                    state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                        .LoopSide(this_equip.LoopSideNumPtr)
                        .Branch(this_equip.BranchNumPtr)
                        .Comp(this_equip.CompNumPtr)
                        .CurOpSchemeType = this->Type;
                }
            }
        }

        // process dedicated heat recovery water to water heatpumps to control
        if (this->PlantOps.DedicatedHR_ChWRetControl_Input) {
            PlantLocation compLoc;
            DataPlant::PlantEquipmentType Type;
            Type = static_cast<DataPlant::PlantEquipmentType>(
                getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase("HeatPump:PlantLoop:EIR:Cooling")));
            bool errFlag1(false);
            int NumSearchResults(0);

            bool founditCooling = false;
            bool founditHeating = false;
            for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                    int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                    if (PltSizNum > 0) {
                        if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Cooling) {
                            PlantUtilities::ScanPlantLoopsForObject(
                                state, this->DedicatedHR_ChWRetControl_Name, Type, compLoc, errFlag1, _, _, NumSearchResults, _, LoopNum, true);
                            if (NumSearchResults == 1) {
                                this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr = compLoc.loopNum;
                                this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr = compLoc.loopSideNum;
                                this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr = compLoc.branchNum;
                                this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr = compLoc.compNum;
                                founditCooling = true;
                            }
                        } else if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Heating) {
                            PlantUtilities::ScanPlantLoopsForObject(
                                state, this->DedicatedHR_ChWRetControl_Name, Type, compLoc, errFlag1, _, _, NumSearchResults, _, LoopNum, true);
                            if (NumSearchResults == 1) {
                                this->DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr = compLoc.loopNum;
                                this->DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr = compLoc.loopSideNum;
                                this->DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr = compLoc.branchNum;
                                this->DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr = compLoc.compNum;
                                founditHeating = true;
                            }
                        }
                    }
                }
            }
            if (!founditCooling) {
                ShowSevereError(state,
                                format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" component "
                                       "\"{}\" was not found on a cooling plant loop.",
                                       this->Name,
                                       this->DedicatedHR_ChWRetControl_Name));
            }
            if (!founditHeating) {
                ShowSevereError(state,
                                format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" component "
                                       "\"{}\" was not found on a heating plant loop.",
                                       this->Name,
                                       this->DedicatedHR_ChWRetControl_Name));
            }
            if (founditCooling) {
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .OpScheme.allocate(1);
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .OpScheme(1)
                    .OpSchemePtr = 1; // TODO check
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .CurOpSchemeType = this->Type;

                int outletNode = state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                                     .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                                     .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                                     .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                                     .NodeNumOut;
                this->DedicatedHR_ChWRetControl_LoadSideComp.SetPointNodeNum = outletNode;
            }
            if (founditHeating) {
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                    .OpScheme.allocate(1);
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                    .OpScheme(1)
                    .OpSchemePtr = 1; // TODO check
                state.dataPlnt->PlantLoop(DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                    .CurOpSchemeType = this->Type;
            }
        }
        if (this->PlantOps.DedicatedHR_HWRetControl_Input) {
            PlantLocation compLoc;
            DataPlant::PlantEquipmentType Type;
            Type = static_cast<DataPlant::PlantEquipmentType>(
                getEnumerationValue(PlantEquipTypeNamesUC, UtilityRoutines::MakeUPPERCase("HeatPump:PlantLoop:EIR:Heating")));
            bool errFlag1(false);
            int NumSearchResults(0);
            bool founditCooling = false;
            bool founditHeating = false;

            for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                if (this->PlantLoopIndicesBeingSupervised(LoopNum) > 0) {
                    int PltSizNum = state.dataPlnt->PlantLoop(LoopNum).PlantSizNum;
                    if (PltSizNum > 0) {
                        if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Cooling) {
                            PlantUtilities::ScanPlantLoopsForObject(
                                state, this->DedicatedHR_HWRetControl_Name, Type, compLoc, errFlag1, _, _, NumSearchResults, _, LoopNum, true);
                            if (NumSearchResults == 1) {
                                this->DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr = compLoc.loopNum;
                                this->DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr = compLoc.loopSideNum;
                                this->DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr = compLoc.branchNum;
                                this->DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr = compLoc.compNum;
                                founditCooling = true;
                            }
                        } else if (state.dataSize->PlantSizData(PltSizNum).LoopType == DataSizing::TypeOfPlantLoop::Heating) {
                            PlantUtilities::ScanPlantLoopsForObject(
                                state, this->DedicatedHR_HWRetControl_Name, Type, compLoc, errFlag1, _, _, NumSearchResults, _, LoopNum, true);
                            if (NumSearchResults == 1) {
                                this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr = compLoc.loopNum;
                                this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr = compLoc.loopSideNum;
                                this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr = compLoc.branchNum;
                                this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr = compLoc.compNum;
                                founditHeating = true;
                            }
                        }
                    }
                }
            }
            if (!founditCooling) {
                ShowSevereError(state,
                                format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" component "
                                       "\"{}\" was not found on a cooling plant loop.",
                                       this->Name,
                                       this->DedicatedHR_HWRetControl_Name));
            }
            if (!founditHeating) {
                ShowSevereError(state,
                                format("ChillerHeaterSupervisoryOperationData::OneTimeInitChillerHeaterChangeoverOpScheme problem=\"{}\" component "
                                       "\"{}\" was not found on a heating plant loop.",
                                       this->Name,
                                       this->DedicatedHR_HWRetControl_Name));
            }
            if (founditHeating) {
                int outletNode = state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                                     .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                                     .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                                     .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                                     .NodeNumOut;
                this->DedicatedHR_HWRetControl_LoadSideComp.SetPointNodeNum = outletNode;

                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .OpScheme.allocate(1);
                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .OpScheme(1)
                    .OpSchemePtr = 1; // TODO check
                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .CurOpSchemeType = this->Type;
            }

            if (founditCooling) {

                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                    .OpScheme.allocate(1);
                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                    .OpScheme(1)
                    .OpSchemePtr = 1; // TODO check
                state.dataPlnt->PlantLoop(DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                    .CurOpSchemeType = this->Type;
            }
        }

        this->oneTimeSetupComplete = true;
    }

    void ChillerHeaterSupervisoryOperationData::EvaluateChillerHeaterChangeoverOpScheme(EnergyPlusData &state, bool const FirstHVACIteration)
    {

        // Poll the loads on the zones to help decide how to run

        Real64 sumZonePredictedHeatingLoad(0.0);
        Real64 sumZonePredictedCoolingLoad(0.0);
        for (int zoneIndexinList = 1; zoneIndexinList <= this->PlantOps.NumOfZones; ++zoneIndexinList) {
            int thisZoneIndex = this->ZonePtrs(zoneIndexinList);
            Real64 ZoneMult = state.dataHeatBal->Zone(thisZoneIndex).Multiplier * state.dataHeatBal->Zone(thisZoneIndex).ListMultiplier;
            // aggregate required outputs to setpoint, with zone multipliers included
            sumZonePredictedCoolingLoad +=
                min(0.0,
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(thisZoneIndex).OutputRequiredToCoolingSP * ZoneMult); // sum only negative values
            sumZonePredictedHeatingLoad +=
                max(0.0,
                    state.dataZoneEnergyDemand->ZoneSysEnergyDemand(thisZoneIndex).OutputRequiredToHeatingSP * ZoneMult); // sum only positive values
        }

        // now add in ventilation loading at the central air system level
        Real64 sumAirSysVentHeatingLoad(0.0);
        Real64 sumAirSysVentCoolingLoad(0.0);

        for (int airLoopsServedIndex = 1; airLoopsServedIndex <= this->PlantOps.NumOfAirLoops; ++airLoopsServedIndex) {
            int AirLoopNum = this->AirLoopPtrs(airLoopsServedIndex);
            Real64 outAir_H = state.dataEnvrn->OutEnthalpy;
            Real64 outAirMdot = state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFlow;
            Real64 retAir_Tdb = state.dataLoopNodes->Node(state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopReturnNodeNum(1)).Temp;
            Real64 retAir_w = state.dataLoopNodes->Node(state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopReturnNodeNum(1)).HumRat;
            Real64 ventLoad = outAirMdot * (Psychrometrics::PsyHFnTdbW(retAir_Tdb, retAir_w) - outAir_H); // negative is cooling
            if (ventLoad > DataHVACGlobals::SmallLoad) {                                                  // add to heating
                sumAirSysVentHeatingLoad += ventLoad;
            } else if (ventLoad < DataPrecisionGlobals::constant_minusone * DataHVACGlobals::SmallLoad) { // add to cooling
                sumAirSysVentCoolingLoad += ventLoad;
            }
        }

        // now add in process loads from plant load profiles on the controlled loops.
        Real64 sumLoadProfileHeatingLoad(0.0);
        Real64 sumLoadProfileCoolingLoad(0.0);
        for (int NumProcLoad = 1; NumProcLoad <= this->PlantOps.numPlantLoadProfiles; ++NumProcLoad) {
            Real64 load = 0.0;
            DataPlant::CompData::getPlantComponent(state, PlantLoadProfileComps(NumProcLoad)).compPtr->getCurrentPower(state, load);
            if (load > 0.0) {
                sumLoadProfileHeatingLoad += load;
            } else {
                sumLoadProfileCoolingLoad += load;
            }
        }

        this->Report.SensedCoolingLoad = sumZonePredictedCoolingLoad + sumAirSysVentCoolingLoad + sumLoadProfileCoolingLoad;
        this->Report.SensedHeatingLoad = sumZonePredictedHeatingLoad + sumAirSysVentHeatingLoad + sumLoadProfileHeatingLoad;

        // process logic based on poll results
        bool CoolingOnly(false);
        bool CoolingDominated(false);
        bool HeatingOnly(false);
        bool HeatingDominated(false);
        bool SimultaneousHeatingAndCooling(false);
        bool SimultaneousHeatingAndCoolingRequested(false);
        if (sumZonePredictedHeatingLoad < DataHVACGlobals::SmallLoad &&
            sumZonePredictedCoolingLoad < DataPrecisionGlobals::constant_minusone * DataHVACGlobals::SmallLoad) {
            CoolingOnly = true;
        } else if (sumZonePredictedCoolingLoad > DataPrecisionGlobals::constant_minusone * DataHVACGlobals::SmallLoad &&
                   sumZonePredictedHeatingLoad > DataHVACGlobals::SmallLoad) {
            HeatingOnly = true;
        } else if (sumZonePredictedCoolingLoad < DataPrecisionGlobals::constant_minusone * DataHVACGlobals::SmallLoad &&
                   sumZonePredictedHeatingLoad > DataHVACGlobals::SmallLoad) {
            SimultaneousHeatingAndCoolingRequested = true;
            SimultaneousHeatingAndCooling = true;
            if (sumZonePredictedHeatingLoad > abs(sumZonePredictedCoolingLoad)) {
                HeatingDominated = true;
                if (this->PlantOps.SimultHeatCoolOpAvailable) {
                    SimultaneousHeatingAndCooling = true;
                } else {
                    HeatingOnly = true;
                    SimultaneousHeatingAndCooling = false;
                }
            } else if (abs(sumZonePredictedCoolingLoad) > sumZonePredictedHeatingLoad) {
                CoolingDominated = true;
                if (this->PlantOps.SimultHeatCoolOpAvailable) {
                    SimultaneousHeatingAndCooling = true;
                } else {
                    CoolingOnly = true;
                    SimultaneousHeatingAndCooling = false;
                }
            }
        }

        //_____________________________________________________________________________
        // initialize all possible equipment to turn off machines before applying controls to turn them on.
        // turn .Available and .ON to false in plant structure
        if (FirstHVACIteration) {

            if (this->PlantOps.NumCoolingOnlyEquipLists > 0) {
                for (int equipListNum = 1; equipListNum <= this->PlantOps.NumCoolingOnlyEquipLists; ++equipListNum) {
                    int NumComps = this->CoolingOnlyEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->CoolingOnlyEquipList(equipListNum).Comp(compNum));
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .Available = false;
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .ON = false;
                    }
                }
            }

            if (this->PlantOps.NumHeatingOnlyEquipLists > 0) {
                for (int equipListNum = 1; equipListNum <= this->PlantOps.NumHeatingOnlyEquipLists; ++equipListNum) {
                    int NumComps = this->HeatingOnlyEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->HeatingOnlyEquipList(equipListNum).Comp(compNum));
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .Available = false;
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .ON = false;
                    }
                }
            }

            if (this->PlantOps.NumSimultHeatCoolCoolingEquipLists > 0) {
                for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolCoolingEquipLists; ++equipListNum) {

                    int NumComps = this->SimultHeatCoolCoolingEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->SimultHeatCoolCoolingEquipList(equipListNum).Comp(compNum));
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .Available = false;
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .ON = false;
                    }
                }
            }

            if (this->PlantOps.NumSimultHeatCoolHeatingEquipLists > 0) {
                for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolHeatingEquipLists; ++equipListNum) {

                    int NumComps = this->SimultHeatCoolHeatingEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->SimultHeatCoolHeatingEquipList(equipListNum).Comp(compNum));
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .Available = false;
                        state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                            .LoopSide(this_equip.LoopSideNumPtr)
                            .Branch(this_equip.BranchNumPtr)
                            .Comp(this_equip.CompNumPtr)
                            .ON = false;
                    }
                }
            }
        }
        // end init machines off
        //___________________________________________________________________________
        if (CoolingOnly) {
            // use zone loads to find range based cooling loads
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumCoolingOnlyEquipLists; ++equipListNum) {
                // zone cooling loads are negative, switch to positive for range based limiting
                if (sumZonePredictedCoolingLoad * DataPrecisionGlobals::constant_minusone >
                        this->CoolingOnlyEquipList(equipListNum).RangeLowerLimit &&
                    this->CoolingOnlyEquipList(equipListNum).RangeUpperLimit >
                        sumZonePredictedCoolingLoad * DataPrecisionGlobals::constant_minusone) {
                    // found that this equipment list load ranges match the zone predicted cooling loads

                    int NumComps = this->CoolingOnlyEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->CoolingOnlyEquipList(equipListNum).Comp(compNum));
                        // set cooling setpoint at outlet

                        // todo, oa reset ?

                        state.dataLoopNodes->Node(this_equip.SetPointNodeNum).TempSetPoint = this->Setpoint.CW;
                        if (state.dataLoopNodes->Node(this_equip.DemandNodeNum).Temp > this->Setpoint.CW) {

                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .Available = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .ON = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .CurOpSchemeType = this->Type;
                        }
                        //
                    }
                }
            }
        }

        //____________________________________________________________________________
        if (HeatingOnly) { // Use Heating Only equipment operation

            // use zone loads to find range based heating loads
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumHeatingOnlyEquipLists; ++equipListNum) {
                if (sumZonePredictedHeatingLoad > this->HeatingOnlyEquipList(equipListNum).RangeLowerLimit &&
                    this->HeatingOnlyEquipList(equipListNum).RangeUpperLimit > sumZonePredictedHeatingLoad) {
                    // found that this equipment list load ranges match the zone predicted heating loads

                    int NumComps = this->HeatingOnlyEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->HeatingOnlyEquipList(equipListNum).Comp(compNum));
                        // set heating setpoint at outlet

                        // todo, oa reset

                        state.dataLoopNodes->Node(this_equip.SetPointNodeNum).TempSetPoint = this->Setpoint.HW;

                        if (state.dataLoopNodes->Node(this_equip.DemandNodeNum).Temp < this->Setpoint.HW) {
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .Available = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .ON = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .CurOpSchemeType = this->Type;
                        }
                        //
                    }
                }
            }
        }

        if (SimultaneousHeatingAndCooling) {

            // use zone cooling loads to find range based equipment
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolCoolingEquipLists; ++equipListNum) {
                // zone cooling loads are negative, switch to positive for range based limiting
                if (sumZonePredictedCoolingLoad * DataPrecisionGlobals::constant_minusone >
                        this->SimultHeatCoolCoolingEquipList(equipListNum).RangeLowerLimit &&
                    this->SimultHeatCoolCoolingEquipList(equipListNum).RangeUpperLimit >
                        sumZonePredictedCoolingLoad * DataPrecisionGlobals::constant_minusone) {
                    // found that this equipment list load ranges match the zone predicted cooling loads

                    int NumComps = this->SimultHeatCoolCoolingEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->SimultHeatCoolCoolingEquipList(equipListNum).Comp(compNum));
                        // set cooling setpoint at outlet

                        // todo, oa reset ?

                        state.dataLoopNodes->Node(this_equip.SetPointNodeNum).TempSetPoint = this->Setpoint.CW;
                        if (state.dataLoopNodes->Node(this_equip.DemandNodeNum).Temp > this->Setpoint.CW) {
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .Available = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .ON = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .CurOpSchemeType = this->Type;
                        }
                        //
                    }
                }
            }

            // use zone loads to find range based heating loads
            for (int equipListNum = 1; equipListNum <= this->PlantOps.NumSimultHeatCoolHeatingEquipLists; ++equipListNum) {
                if (sumZonePredictedHeatingLoad > this->SimultHeatCoolHeatingEquipList(equipListNum).RangeLowerLimit &&
                    this->SimultHeatCoolHeatingEquipList(equipListNum).RangeUpperLimit > sumZonePredictedHeatingLoad) {
                    // found that this equipment list load ranges match the zone predicted heating loads

                    int NumComps = this->SimultHeatCoolHeatingEquipList(equipListNum).NumComps;
                    for (int compNum = 1; compNum <= NumComps; ++compNum) {
                        auto &this_equip(this->SimultHeatCoolHeatingEquipList(equipListNum).Comp(compNum));
                        // set heating setpoint at outlet

                        // todo, oa reset

                        state.dataLoopNodes->Node(this_equip.SetPointNodeNum).TempSetPoint = this->Setpoint.HW;
                        if (state.dataLoopNodes->Node(this_equip.DemandNodeNum).Temp < this->Setpoint.HW) {
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .Available = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .ON = true;
                            state.dataPlnt->PlantLoop(this_equip.LoopNumPtr)
                                .LoopSide(this_equip.LoopSideNumPtr)
                                .Branch(this_equip.BranchNumPtr)
                                .Comp(this_equip.CompNumPtr)
                                .CurOpSchemeType = this->Type;
                        }
                        //
                    }
                }
            }
        }

        if (HeatingOnly) {
            this->Report.OutputOpMode = 1;
        } else if (CoolingOnly) {
            this->Report.OutputOpMode = 2;
        } else if (SimultaneousHeatingAndCooling) {
            this->Report.OutputOpMode = 3;
        } else {
            this->Report.OutputOpMode = 0;
        }

        // evaluate if and how dedicated heat exchanger should run

        if (FirstHVACIteration) {
            // initialize off
            if (PlantOps.DedicatedHR_ChWRetControl_Input) {
                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .Available = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .ON = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                    .Available = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                    .ON = false;
            }
            if (PlantOps.DedicatedHR_HWRetControl_Input) {
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .Available = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .ON = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                    .Available = false;
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                    .ON = false;
            }
        }

        if (PlantOps.DedicatedHR_ChWRetControl_Input) { // see if chilled water return is warmer than hot water return

            int inletChWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                                            .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                                            .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                                            .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                                            .NodeNumIn;
            int inletHWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopNumPtr)
                                           .LoopSide(this->DedicatedHR_ChWRetControl_SourceSideComp.LoopSideNumPtr)
                                           .Branch(this->DedicatedHR_ChWRetControl_SourceSideComp.BranchNumPtr)
                                           .Comp(this->DedicatedHR_ChWRetControl_SourceSideComp.CompNumPtr)
                                           .NodeNumIn;
            if (state.dataLoopNodes->Node(inletChWReturnNodeNum).Temp > state.dataLoopNodes->Node(inletHWReturnNodeNum).Temp) {
                // turn ON load side of this water to water heat pump

                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .Available = true;
                state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                    .ON = true;

                int OutletChWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopNumPtr)
                                                 .LoopSide(this->DedicatedHR_ChWRetControl_LoadSideComp.LoopSideNumPtr)
                                                 .Branch(this->DedicatedHR_ChWRetControl_LoadSideComp.BranchNumPtr)
                                                 .Comp(this->DedicatedHR_ChWRetControl_LoadSideComp.CompNumPtr)
                                                 .NodeNumOut;
                state.dataLoopNodes->Node(OutletChWReturnNodeNum).TempSetPoint = this->Setpoint.CW;
            }
        }

        if (PlantOps.DedicatedHR_HWRetControl_Input) { // see if hot water return is cooler than chilled water return
            int inletChWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_SourceSideComp.LoopNumPtr)
                                            .LoopSide(this->DedicatedHR_HWRetControl_SourceSideComp.LoopSideNumPtr)
                                            .Branch(this->DedicatedHR_HWRetControl_SourceSideComp.BranchNumPtr)
                                            .Comp(this->DedicatedHR_HWRetControl_SourceSideComp.CompNumPtr)
                                            .NodeNumIn;
            int inletHWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                                           .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                                           .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                                           .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                                           .NodeNumIn;
            if (state.dataLoopNodes->Node(inletChWReturnNodeNum).Temp > state.dataLoopNodes->Node(inletHWReturnNodeNum).Temp) {
                // turn load side of this water to water heat pump
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .Available = true;
                state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                    .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                    .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                    .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                    .ON = true;
                int OutletHWReturnNodeNum = state.dataPlnt->PlantLoop(this->DedicatedHR_HWRetControl_LoadSideComp.LoopNumPtr)
                                                .LoopSide(this->DedicatedHR_HWRetControl_LoadSideComp.LoopSideNumPtr)
                                                .Branch(this->DedicatedHR_HWRetControl_LoadSideComp.BranchNumPtr)
                                                .Comp(this->DedicatedHR_HWRetControl_LoadSideComp.CompNumPtr)
                                                .NodeNumOut;

                state.dataLoopNodes->Node(OutletHWReturnNodeNum).TempSetPoint = this->Setpoint.HW;
            }
        }
    }

} // namespace DataPlant
} // namespace EnergyPlus
