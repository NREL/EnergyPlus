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

#include <EnergyPlus/Autosizing/HeatingAirflowUASizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/api/TypeDefs.h>

namespace EnergyPlus {

    void HeatingAirflowUASizer::setParameters(EnergyPlusData &state,
                                          CommonFlags &_baseFlags,
                                          HeatingAirflowUASizerFlags &_flags,
                                          Array1D<EnergyPlus::DataSizing::TermUnitSizingData> &_termUnitSizing,
                                          Array1D<EnergyPlus::DataSizing::ZoneSizingData> &_finalZoneSizing,
                                          Array1D<EnergyPlus::DataSizing::ZoneEqSizingData> &_zoneEqSizing,
                                          Array1D<EnergyPlus::DataSizing::SystemSizingInputData> &_sysSizingInputData,
                                          Array1D<EnergyPlus::DataSizing::SystemSizingData> &_finalSysSizing,
                                          Array1D<DataAirLoop::OutsideAirSysProps> &_outsideAirSys,
                                          Array1D<DataSizing::ZoneEqSizingData> &_oaSysEqSizing,
                                          std::vector<AirLoopHVACDOAS::AirLoopDOAS> &_airloopDOAS)
{
    this->baseFlags = _baseFlags;
    this->flags = _flags;
    this->termUnitSizing = _termUnitSizing;
    this->finalZoneSizing = _finalZoneSizing;
    this->zoneEqSizing = _zoneEqSizing;
    this->sysSizingInputData = _sysSizingInputData;
    this->finalSysSizing = _finalSysSizing;
    this->outsideAirSys = _outsideAirSys;
    this->oaSysEqSizing = _oaSysEqSizing;
    this->airloopDOAS = _airloopDOAS;
}


AutoSizingResultType HeatingAirflowUASizer::size(EnergyPlusData &state, Real64 _originalValue)
{
    AutoSizingResultType errorsFound = AutoSizingResultType::NoError;
    this->preSize(state, this->baseFlags, _originalValue);
    if (this->baseFlags.curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            if (this->baseFlags.printWarningFlag && this->originalValue > 0.0) {
                this->reportSizerOutput(
                    this->baseFlags.compType, this->baseFlags.compName, "User-Specified " + this->flags.sizingString, _originalValue);
            }
            this->autoSizedValue = _originalValue;
        } else {
            if (this->baseFlags.termUnitSingDuct && (this->baseFlags.curTermUnitSizingNum > 0)) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->termUnitSizing(this->baseFlags.curTermUnitSizingNum).AirVolFlow;
            } else if ((this->baseFlags.termUnitPIU || this->baseFlags.termUnitIU) && (this->baseFlags.curTermUnitSizingNum > 0)) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->termUnitSizing(this->baseFlags.curTermUnitSizingNum).AirVolFlow *
                                       this->termUnitSizing(this->baseFlags.curTermUnitSizingNum).ReheatAirFlowMult;
            } else if (this->baseFlags.zoneEqFanCoil) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->finalZoneSizing(this->baseFlags.curZoneEqNum).DesHeatVolFlow;
            } else if (this->baseFlags.otherEqType) {
                if (this->zoneEqSizing(this->baseFlags.curZoneEqNum).SystemAirFlow) {
                    this->autoSizedValue = this->zoneEqSizing(this->baseFlags.curZoneEqNum).AirVolFlow * DataEnvironment::StdRhoAir;
                } else if (this->zoneEqSizing(this->baseFlags.curZoneEqNum).HeatingAirFlow) {
                    this->autoSizedValue = this->zoneEqSizing(this->baseFlags.curZoneEqNum).HeatingAirVolFlow * DataEnvironment::StdRhoAir;
                } else {
                    this->autoSizedValue = this->finalZoneSizing(this->baseFlags.curZoneEqNum).DesHeatMassFlow;
                }
            } else {
                errorsFound = AutoSizingResultType::ErrorType1;
            }
        }
    } else if (baseFlags.curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            if (this->baseFlags.printWarningFlag && this->originalValue > 0.0) {
                this->reportSizerOutput(
                    this->baseFlags.compType, this->baseFlags.compName, "User-Specified " + this->flags.sizingString, _originalValue);
            }
            this->autoSizedValue = _originalValue;
        } else {
            if (this->baseFlags.curOASysNum > 0) {
                if (this->outsideAirSys(this->baseFlags.curOASysNum).AirLoopDOASNum > -1) {
                    this->autoSizedValue = this->airloopDOAS[this->outsideAirSys(this->baseFlags.curOASysNum).AirLoopDOASNum].SizingMassFlow /
                                           DataEnvironment::StdRhoAir;
                } else {
                    this->autoSizedValue = this->finalSysSizing(baseFlags.curSysNum).DesOutAirVolFlow;
                }
            } else {
                if (this->baseFlags.curDuctType == DataHVACGlobals::Main) {
                    if (this->finalSysSizing(baseFlags.curSysNum).SysAirMinFlowRat > 0.0) {
                        this->autoSizedValue =
                            this->finalSysSizing(baseFlags.curSysNum).SysAirMinFlowRat * this->finalSysSizing(baseFlags.curSysNum).DesMainVolFlow;
                    } else {
                        this->autoSizedValue = this->finalSysSizing(baseFlags.curSysNum).DesMainVolFlow;
                    }
                } else if (this->baseFlags.curDuctType == DataHVACGlobals::Cooling) {
                    if (this->finalSysSizing(baseFlags.curSysNum).SysAirMinFlowRat > 0.0) {
                        this->autoSizedValue =
                            this->finalSysSizing(baseFlags.curSysNum).SysAirMinFlowRat * this->finalSysSizing(baseFlags.curSysNum).DesCoolVolFlow;
                    } else {
                        this->autoSizedValue = this->finalSysSizing(baseFlags.curSysNum).DesCoolVolFlow;
                    }
                } else if (this->baseFlags.curDuctType == DataHVACGlobals::Heating) {
                    this->autoSizedValue = this->finalSysSizing(baseFlags.curSysNum).DesHeatVolFlow;
                } else {
                    this->autoSizedValue = this->finalSysSizing(baseFlags.curSysNum).DesMainVolFlow;
                }
            }
            this->autoSizedValue *= DataEnvironment::StdRhoAir;
        }
    }
    if (this->autoSizedValue < DataHVACGlobals::SmallAirVolFlow) this->autoSizedValue = 0.0;
    if (this->wasAutoSized || this->baseFlags.curOASysNum > 0) this->selectSizerOutput(this->flags.sizingString);
    return errorsFound;
}

} // namespace EnergyPlus
