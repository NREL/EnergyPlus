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

#include <EnergyPlus/Autosizing/HeatingWaterDesAirInletTempSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>

namespace EnergyPlus {

Real64 HeatingWaterDesAirInletTempSizer::size(EnergyPlusData &state, Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(state, errorsFound)) {
        return 0.0;
    }
    this->preSize(state, _originalValue);

    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->termUnitPIU && (this->curTermUnitSizingNum > 0)) {
                Real64 MinFlowFrac = this->termUnitSizing(this->curTermUnitSizingNum).MinFlowFrac;
                if (this->termUnitSizing(this->curTermUnitSizingNum).InducesPlenumAir) {
                    this->autoSizedValue = (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU * MinFlowFrac) +
                                           (this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneRetTempAtHeatPeak * (1.0 - MinFlowFrac));
                } else {
                    this->autoSizedValue = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU * MinFlowFrac +
                                           this->finalZoneSizing(this->curZoneEqNum).ZoneTempAtHeatPeak * (1.0 - MinFlowFrac);
                }
            } else if (this->termUnitIU && (this->curTermUnitSizingNum > 0)) {
                this->autoSizedValue = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).ZoneTempAtHeatPeak;
            } else if (this->termUnitSingDuct && (this->curTermUnitSizingNum > 0)) {
                this->autoSizedValue = this->termUnitFinalZoneSizing(this->curTermUnitSizingNum).DesHeatCoilInTempTU;
            } else {
                Real64 DesMassFlow;
                if (this->zoneEqSizing(this->curZoneEqNum).SystemAirFlow) {
                    DesMassFlow = this->zoneEqSizing(this->curZoneEqNum).AirVolFlow * state.dataEnvrn->StdRhoAir;
                } else if (this->zoneEqSizing(this->curZoneEqNum).HeatingAirFlow) {
                    DesMassFlow = this->zoneEqSizing(this->curZoneEqNum).HeatingAirVolFlow * state.dataEnvrn->StdRhoAir;
                } else {
                    DesMassFlow = this->finalZoneSizing(this->curZoneEqNum).DesHeatMassFlow;
                }
                this->autoSizedValue =
                    this->setHeatCoilInletTempForZoneEqSizing(this->setOAFracForZoneEqSizing(state, DesMassFlow, this->zoneEqSizing(this->curZoneEqNum)),
                                                              this->zoneEqSizing(this->curZoneEqNum),
                                                              this->finalZoneSizing(this->curZoneEqNum));
            }
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            Real64 OutAirFrac = 1.0;
            if (this->curOASysNum > 0) {
                OutAirFrac = 1.0;
            } else if (this->finalSysSizing(this->curSysNum).HeatOAOption == this->minOA) {
                if (this->dataFlowUsedForSizing > 0.0) {
                    OutAirFrac = this->finalSysSizing(this->curSysNum).DesOutAirVolFlow / this->dataFlowUsedForSizing;
                } else {
                    OutAirFrac = 1.0;
                }
                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
            }
            // coil inlet temperature
            if (this->curOASysNum == 0 && this->primaryAirSystem(this->curSysNum).NumOAHeatCoils > 0) {
                this->autoSizedValue = OutAirFrac * this->finalSysSizing(this->curSysNum).PreheatTemp +
                                       (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetTemp;
            } else if (this->curOASysNum > 0 && this->outsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                this->autoSizedValue = this->airloopDOAS[this->outsideAirSys(this->curOASysNum).AirLoopDOASNum].HeatOutTemp;
            } else {
                this->autoSizedValue = OutAirFrac * this->finalSysSizing(this->curSysNum).HeatOutTemp +
                                       (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).HeatRetTemp;
            }
        }
    }
    if (this->overrideSizeString) {
        if (this->isEpJSON) this->sizingString = "rated_inlet_air_temperature";
    }
    this->selectSizerOutput(state, errorsFound);
    // report not written for OA coils and needs to be corrected
    if (this->curSysNum <= this->numPrimaryAirSys) {
        if (this->isCoilReportObject)
            coilSelectionReportObj->setCoilEntAirTemp(state, this->compName, this->compType, this->autoSizedValue, this->curSysNum, this->curZoneEqNum);
    }
    return this->autoSizedValue;
}

} // namespace EnergyPlus
