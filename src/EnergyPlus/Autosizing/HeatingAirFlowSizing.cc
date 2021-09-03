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

#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

Real64 HeatingAirFlowSizer::size(EnergyPlusData &state, Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(state, errorsFound)) {
        return 0.0;
    }
    this->preSize(state, _originalValue);
    std::string DDNameFanPeak = "";
    std::string dateTimeFanPeak = "";

    if (this->dataEMSOverrideON) {
        this->autoSizedValue = this->dataEMSOverride;
    } else if (this->dataConstantUsedForSizing > 0 && this->dataFractionUsedForSizing > 0) {
        this->autoSizedValue = this->dataConstantUsedForSizing * this->dataFractionUsedForSizing;
    } else {
        if (this->curZoneEqNum > 0) {
            if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
                this->autoSizedValue = _originalValue;
            } else if (this->zoneEqSizing(this->curZoneEqNum).DesignSizeFromParent) {
                this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).AirVolFlow;
            } else {
                auto const SELECT_CASE_var(this->zoneEqSizing(this->curZoneEqNum).SizingMethod(DataHVACGlobals::HeatingAirflowSizing));
                if ((SELECT_CASE_var == DataSizing::SupplyAirFlowRate) || (SELECT_CASE_var == DataSizing::None) ||
                    (SELECT_CASE_var == DataSizing::FlowPerFloorArea)) {
                    if (this->zoneEqSizing(this->curZoneEqNum).SystemAirFlow) {
                        this->autoSizedValue = max(this->zoneEqSizing(this->curZoneEqNum).AirVolFlow,
                                                   this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow,
                                                   this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow);
                    } else {
                        if (state.dataSize->ZoneCoolingOnlyFan) {
                            this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow;
                        } else if (state.dataSize->ZoneHeatingOnlyFan) {
                            this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow;
                        } else if (this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                            this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow;
                        } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                            this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow;
                        } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                            this->autoSizedValue = max(this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow,
                                                       this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow);
                        } else {
                            this->autoSizedValue = max(this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow,
                                                       this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow);
                        }
                    }
                } else if (SELECT_CASE_var == DataSizing::FractionOfAutosizedCoolingAirflow) {
                    if (state.dataSize->ZoneCoolingOnlyFan) {
                        this->autoSizedValue = this->dataFracOfAutosizedCoolingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow;
                    } else if (state.dataSize->ZoneHeatingOnlyFan) {
                        this->autoSizedValue = this->dataFracOfAutosizedHeatingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                        this->autoSizedValue = this->dataFracOfAutosizedCoolingAirflow * this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = this->dataFracOfAutosizedHeatingAirflow * this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue =
                            max(this->dataFracOfAutosizedCoolingAirflow * this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow,
                                this->dataFracOfAutosizedHeatingAirflow * this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow);
                    } else {
                        this->autoSizedValue =
                            max(this->dataFracOfAutosizedCoolingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow,
                                this->dataFracOfAutosizedHeatingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow);
                    }
                } else if (SELECT_CASE_var == DataSizing::FractionOfAutosizedHeatingAirflow) {
                    if (state.dataSize->ZoneCoolingOnlyFan) {
                        this->autoSizedValue = this->dataFracOfAutosizedCoolingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow;
                    } else if (state.dataSize->ZoneHeatingOnlyFan) {
                        this->autoSizedValue = this->dataFracOfAutosizedHeatingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                        this->autoSizedValue = this->dataFracOfAutosizedCoolingAirflow * this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = this->dataFracOfAutosizedHeatingAirflow * this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue =
                            max(this->dataFracOfAutosizedCoolingAirflow * this->zoneEqSizing(this->curZoneEqNum).CoolingAirVolFlow,
                                this->dataFracOfAutosizedHeatingAirflow * this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow);
                    } else {
                        this->autoSizedValue =
                            max(this->dataFracOfAutosizedCoolingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow,
                                this->dataFracOfAutosizedHeatingAirflow * this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow);
                    }
                } else if (SELECT_CASE_var == DataSizing::FlowPerCoolingCapacity) {
                    if (state.dataSize->ZoneCoolingOnlyFan) {
                        this->autoSizedValue = this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity;
                    } else if (state.dataSize->ZoneHeatingOnlyFan) {
                        this->autoSizedValue = this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                        this->autoSizedValue = this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = max(this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity,
                                                   this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity);
                    } else {
                        this->autoSizedValue = max(this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity,
                                                   this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity);
                    }
                } else if (SELECT_CASE_var == DataSizing::FlowPerHeatingCapacity) {
                    if (state.dataSize->ZoneCoolingOnlyFan) {
                        this->autoSizedValue = this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity;
                    } else if (state.dataSize->ZoneHeatingOnlyFan) {
                        this->autoSizedValue = this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                        this->autoSizedValue = this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && !this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity;
                    } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow && this->zoneEqSizing(this->curZoneEqNum).CoolingAirFlow) {
                        this->autoSizedValue = max(this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity,
                                                   this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity);
                    } else {
                        this->autoSizedValue = max(this->dataFlowPerCoolingCapacity * this->dataAutosizedCoolingCapacity,
                                                   this->dataFlowPerHeatingCapacity * this->dataAutosizedHeatingCapacity);
                    }
                } else {
                    if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                        this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow;
                    } else if (state.dataSize->ZoneCoolingOnlyFan) { // probably shouldn't be here
                        this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow;
                    } else if (this->termUnitIU && (this->curTermUnitSizingNum > 0)) {
                        this->autoSizedValue = this->termUnitSizing(this->curTermUnitSizingNum).AirVolFlow;
                    } else if (this->zoneEqFanCoil) {
                        this->autoSizedValue = this->zoneEqSizing(this->curZoneEqNum).AirVolFlow;
                    } else if (this->zoneHeatingOnlyFan) {
                        this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow;
                    } else {
                        this->autoSizedValue =
                            max(this->finalZoneSizing(this->curZoneEqNum).DesCoolVolFlow, this->finalZoneSizing(this->curZoneEqNum).DesHeatVolFlow);
                    }
                }
            }
        } else if (this->curSysNum > 0) {
            if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
                this->autoSizedValue = _originalValue;
            } else {
                if (this->curOASysNum > 0) {
                    if (this->oaSysEqSizing(this->curOASysNum).AirFlow) {
                        // Parent object sets system flow rate
                        this->autoSizedValue = this->oaSysEqSizing(this->curOASysNum).AirVolFlow;
                    } else if (this->oaSysEqSizing(this->curOASysNum).HeatingAirFlow) {
                        // Parent object sets heating flow rate
                        this->autoSizedValue = this->oaSysEqSizing(this->curOASysNum).HeatingAirVolFlow;
                    } else if (outsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                        this->autoSizedValue =
                            this->airloopDOAS[outsideAirSys(this->curOASysNum).AirLoopDOASNum].SizingMassFlow / state.dataEnvrn->StdRhoAir;
                    } else {
                        this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesOutAirVolFlow;
                    }
                } else {
                    if (this->unitarySysEqSizing(this->curSysNum).AirFlow) {
                        this->autoSizedValue = this->unitarySysEqSizing(this->curSysNum).AirVolFlow;
                    } else if (this->unitarySysEqSizing(this->curSysNum).HeatingAirFlow) {
                        this->autoSizedValue = this->unitarySysEqSizing(this->curSysNum).HeatingAirVolFlow;
                    } else {
                        if (this->curDuctType == DataHVACGlobals::Main) {
                            if (UtilityRoutines::SameString(this->compType, "COIL:HEATING:WATER")) {
                                if (this->finalSysSizing(this->curSysNum).SysAirMinFlowRat > 0.0 && !this->dataDesicRegCoil) {
                                    this->autoSizedValue =
                                        this->finalSysSizing(this->curSysNum).SysAirMinFlowRat * this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                                } else {
                                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                                }
                            } else {
                                this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                            }
                        } else if (this->curDuctType == DataHVACGlobals::Cooling) {
                            if (UtilityRoutines::SameString(this->compType, "COIL:HEATING:WATER")) {
                                if (this->finalSysSizing(this->curSysNum).SysAirMinFlowRat > 0.0 && !this->dataDesicRegCoil) {
                                    this->autoSizedValue =
                                        this->finalSysSizing(this->curSysNum).SysAirMinFlowRat * this->finalSysSizing(this->curSysNum).DesCoolVolFlow;
                                } else {
                                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesCoolVolFlow;
                                }
                            } else {
                                this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesCoolVolFlow;
                            }
                        } else if (this->curDuctType == DataHVACGlobals::Heating) {
                            this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesHeatVolFlow;
                        } else if (this->curDuctType == DataHVACGlobals::Other) {
                            this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                        } else {
                            this->autoSizedValue = this->finalSysSizing(this->curSysNum).DesMainVolFlow;
                        }
                    }
                }
            }
        } else if (this->dataNonZoneNonAirloopValue > 0) {
            this->autoSizedValue = this->dataNonZoneNonAirloopValue;
        } else if (!this->wasAutoSized) {
            this->autoSizedValue = this->originalValue;
        } else {
            std::string msg = this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
            ShowSevereError(state, msg);
            this->addErrorMessage(msg);
            msg = format("SizingString = {}, SizingResult = {:.1T}", this->sizingString, this->autoSizedValue);
            ShowContinueError(state, msg);
            this->addErrorMessage(msg);
            errorsFound = true;
        }
        // override sizing string
        if (this->overrideSizeString) {
            if (this->isEpJSON) this->sizingString = "heating_supply_air_flow_rate [m3/s]";
        }
        if (this->dataScalableSizingON) {
            if (this->zoneAirFlowSizMethod == DataSizing::SupplyAirFlowRate || this->zoneAirFlowSizMethod == DataSizing::None) {
                this->sizingStringScalable = "(scaled by flow / zone) ";
            } else if (this->zoneAirFlowSizMethod == DataSizing::FlowPerFloorArea) {
                this->sizingStringScalable = "(scaled by flow / area) ";
            } else if (this->zoneAirFlowSizMethod == DataSizing::FractionOfAutosizedCoolingAirflow ||
                       this->zoneAirFlowSizMethod == DataSizing::FractionOfAutosizedHeatingAirflow) {
                this->sizingStringScalable = "(scaled by fractional multiplier) ";
            } else if (this->zoneAirFlowSizMethod == DataSizing::FlowPerCoolingCapacity ||
                       this->zoneAirFlowSizMethod == DataSizing::FlowPerHeatingCapacity) {
                this->sizingStringScalable = "(scaled by flow / capacity) ";
            }
        }
    }
    this->selectSizerOutput(state, errorsFound);

    if (this->isCoilReportObject) {
        // SizingResult is airflow in m3/s
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(
            state, this->compName, this->compType, this->autoSizedValue, this->wasAutoSized);
    }
    if (this->isFanReportObject) {
        //  fill fan peak day and time here
        if (this->dataScalableSizingON) {
            DDNameFanPeak = "Scaled size, not from any peak";
            dateTimeFanPeak = "Scaled size, not from any peak";
        }
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanDesDay, this->compName, DDNameFanPeak);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPkTime, this->compName, dateTimeFanPeak);
    }
    return this->autoSizedValue;
}

void HeatingAirFlowSizer::clearState()
{
    BaseSizerWithScalableInputs::clearState();
}

} // namespace EnergyPlus
