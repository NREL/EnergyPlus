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

#include <EnergyPlus/Autosizing/BaseSizerWithFanHeatInputs.hh>
#include <EnergyPlus/Autosizing/BaseSizerWithScalableInputs.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <string>

namespace EnergyPlus {

void BaseSizerWithScalableInputs::initializeWithinEP(EnergyPlusData &state,
                                                     const std::string &_compType,
                                                     const std::string &_compName,
                                                     const bool &_printWarningFlag,
                                                     const std::string &_callingRoutine)
{
    BaseSizerWithFanHeatInputs::initializeWithinEP(state, _compType, _compName, _printWarningFlag, _callingRoutine);

    this->dataScalableSizingON = DataSizing::DataScalableSizingON;
    this->dataScalableCapSizingON = DataSizing::DataScalableCapSizingON;
    this->dataHRFlowSizingFlag = DataSizing::HRFlowSizingFlag;
    this->zoneCoolingOnlyFan = DataSizing::ZoneCoolingOnlyFan;
    this->zoneHeatingOnlyFan = DataSizing::ZoneHeatingOnlyFan;
    this->dataFracOfAutosizedCoolingAirflow = DataSizing::DataFracOfAutosizedCoolingAirflow;
    this->dataFracOfAutosizedHeatingAirflow = DataSizing::DataFracOfAutosizedHeatingAirflow;
    this->dataFlowPerCoolingCapacity = DataSizing::DataFlowPerCoolingCapacity;
    this->dataAutosizedCoolingCapacity = DataSizing::DataAutosizedCoolingCapacity;
    this->dataFlowPerHeatingCapacity = DataSizing::DataFlowPerHeatingCapacity;
    this->dataAutosizedHeatingCapacity = DataSizing::DataAutosizedHeatingCapacity;

    this->dataCoilSizingAirInTemp = DataSizing::DataCoilSizingAirInTemp;
    this->dataCoilSizingAirInHumRat = DataSizing::DataCoilSizingAirInHumRat;
    this->dataCoilSizingAirOutTemp = DataSizing::DataCoilSizingAirOutTemp;
    this->dataCoilSizingAirOutHumRat = DataSizing::DataCoilSizingAirOutHumRat;
    this->dataCoilSizingFanCoolLoad = DataSizing::DataCoilSizingFanCoolLoad;
    this->dataCoilSizingCapFT = DataSizing::DataCoilSizingCapFT;
    this->dataTotCapCurveIndex = DataSizing::DataTotCapCurveIndex;
    this->dataTotCapCurveValue = DataSizing::DataTotCapCurveValue;
    this->dataFracOfAutosizedCoolingCapacity = DataSizing::DataFracOfAutosizedCoolingCapacity;
    this->dataFracOfAutosizedHeatingCapacity = DataSizing::DataFracOfAutosizedHeatingCapacity;
    this->dataCoolCoilCap = DataSizing::DataCoolCoilCap;
    this->dataCoilIsSuppHeater = DataSizing::DataCoilIsSuppHeater;
    this->suppHeatCap = DataSizing::SuppHeatCap;
    this->unitaryHeatCap = DataSizing::UnitaryHeatCap;

    this->zoneHVACSizing = DataSizing::ZoneHVACSizing;

    // set supply air fan properties
    if (this->isCoilReportObject && this->curSysNum > 0 && int(this->primaryAirSystem.size()) > 0 &&
        this->curSysNum <= DataHVACGlobals::NumPrimaryAirSys) {
        int SupFanNum = this->primaryAirSystem(this->curSysNum).SupFanNum;
        // int RetFanNum = this->primaryAirSystem(this->curSysNum).RetFanNum;
        switch (this->primaryAirSystem(this->curSysNum).supFanModelTypeEnum) {
        case DataAirSystems::structArrayLegacyFanModels: {
            if (SupFanNum > 0) {
                coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                             this->compName,
                                                             this->compType,
                                                             Fans::Fan(SupFanNum).FanName,
                                                             DataAirSystems::structArrayLegacyFanModels,
                                                             this->primaryAirSystem(this->curSysNum).SupFanNum);
            }
            break;
        }
        case DataAirSystems::objectVectorOOFanSystemModel: {
            if (this->primaryAirSystem(this->curSysNum).supFanVecIndex >= 0) {
                coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                             this->compName,
                                                             this->compType,
                                                             HVACFan::fanObjs[this->primaryAirSystem(this->curSysNum).supFanVecIndex]->name,
                                                             DataAirSystems::objectVectorOOFanSystemModel,
                                                             this->primaryAirSystem(this->curSysNum).supFanVecIndex);
            }
            break;
        }
        case DataAirSystems::fanModelTypeNotYetSet: {
            // do nothing
            break;
        }
        } // end switch
    }

    if (this->curZoneEqNum) {
        if (this->zoneHVACSizingIndex > 0) {
            int coolingSAFMethod = this->zoneHVACSizing(this->zoneHVACSizingIndex).CoolingSAFMethod;
            this->zoneAirFlowSizMethod = coolingSAFMethod;
            this->dataFractionUsedForSizing = 1.0;
            this->dataConstantUsedForSizing = this->zoneHVACSizing(this->zoneHVACSizingIndex).MaxCoolAirVolFlow;
            if (coolingSAFMethod == DataSizing::FlowPerFloorArea) {
                DataSizing::DataScalableSizingON = true;
                this->dataConstantUsedForSizing =
                    this->zoneHVACSizing(this->zoneHVACSizingIndex).MaxCoolAirVolFlow * DataHeatBalance::Zone(DataSizing::DataZoneNumber).FloorArea;
            } else if (coolingSAFMethod == DataSizing::FractionOfAutosizedCoolingAirflow) {
                DataSizing::DataFracOfAutosizedCoolingAirflow = this->zoneHVACSizing(this->zoneHVACSizingIndex).MaxCoolAirVolFlow;
                DataSizing::DataScalableSizingON = true;
            }
        } else {
            if (int(this->zoneEqSizing.size()) > 0 && int(this->zoneEqSizing(this->curZoneEqNum).SizingMethod.size()) > 0) {
                this->zoneAirFlowSizMethod = this->zoneEqSizing(this->curZoneEqNum).SizingMethod(int(this->sizingType));
            } else {
                this->zoneAirFlowSizMethod = 0;
            }
            if (this->zoneAirFlowSizMethod == 0) {
                // do nothing, sizing method not set
            } else if (this->zoneAirFlowSizMethod == DataSizing::SupplyAirFlowRate || this->zoneAirFlowSizMethod == DataSizing::None) {
            }
        }
    }
}

void BaseSizerWithScalableInputs::setHVACSizingIndexData(int const index)
{
    this->zoneHVACSizingIndex = index;
}

} // namespace EnergyPlus
