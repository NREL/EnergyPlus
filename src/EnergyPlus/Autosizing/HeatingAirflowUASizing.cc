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

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/api/TypeDefs.h>
#include <EnergyPlus/Autosizing/HeatingAirflowUASizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

void HeatingAirflowUASizer::setParameters(CommonFlags &_baseFlags,
                                          HeatingAirflowUASizerFlags &_flags,
                                          Array1D<EnergyPlus::DataSizing::TermUnitSizingData> &_termUnitSizing,
                                          Array1D<EnergyPlus::DataSizing::ZoneSizingData> &_finalZoneSizing,
                                          Array1D<EnergyPlus::DataSizing::ZoneEqSizingData> &_zoneEqSizing)
{
    this->baseFlags = _baseFlags;
    this->flags = _flags;
    this->termUnitSizing = _termUnitSizing;
    this->finalZoneSizing = _finalZoneSizing;
    this->zoneEqSizing = _zoneEqSizing;
}

AutoSizingResultType HeatingAirflowUASizer::size(Real64 _originalValue)
{
    AutoSizingResultType errorsFound = AutoSizingResultType::NoError;
    this->preSize(this->baseFlags, _originalValue);
    if (this->flags.curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            if (this->baseFlags.printWarningFlag && this->originalValue > 0.0) {
                this->reportSizerOutput(
                    this->baseFlags.compType, this->baseFlags.compName, "User-Specified " + this->flags.sizingString, _originalValue);
            }
        } else {
            if (this->flags.termUnitSingDuct && (this->flags.curTermUnitSizingNum > 0)) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->termUnitSizing(this->flags.curTermUnitSizingNum).AirVolFlow;
            } else if ((this->flags.termUnitPIU || this->flags.termUnitIU) && (this->flags.curTermUnitSizingNum > 0)) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->termUnitSizing(this->flags.curTermUnitSizingNum).AirVolFlow *
                                       this->termUnitSizing(this->flags.curTermUnitSizingNum).ReheatAirFlowMult;
            } else if (this->flags.zoneEqFanCoil) {
                this->autoSizedValue = DataEnvironment::StdRhoAir * this->finalZoneSizing(this->flags.curZoneEqNum).DesHeatVolFlow;
            } else if (this->flags.otherEqType) {
                if (this->zoneEqSizing(this->flags.curZoneEqNum).SystemAirFlow) {
                    this->autoSizedValue = this->zoneEqSizing(this->flags.curZoneEqNum).AirVolFlow * DataEnvironment::StdRhoAir;
                } else if (this->zoneEqSizing(this->flags.curZoneEqNum).HeatingAirFlow) {
                    this->autoSizedValue = this->zoneEqSizing(this->flags.curZoneEqNum).HeatingAirVolFlow * DataEnvironment::StdRhoAir;
                } else {
                    this->autoSizedValue = this->finalZoneSizing(this->flags.curZoneEqNum).DesHeatMassFlow;
                }
            } else {
                errorsFound = AutoSizingResultType::ErrorType1;
            }
        }
    } else if (flags.curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            if (this->baseFlags.printWarningFlag && this->originalValue > 0.0) {
                this->reportSizerOutput(
                    this->baseFlags.compType, this->baseFlags.compName, "User-Specified " + this->flags.sizingString, _originalValue);
            }
        } else {
            if (this->baseFlags.curOASysNum > 0) {
                if (this->outsideAirSys(this->baseFlags.curOASysNum).AirLoopDOASNum > -1) {
                    this->autoSizedValue = this->airloopDOAS[this->outsideAirSys(this->baseFlags.curOASysNum).AirLoopDOASNum].SizingMassFlow /
                                     DataEnvironment::StdRhoAir;
                } else {
                    this->autoSizedValue = this->finalSysSizing(flags.curSysNum).DesOutAirVolFlow;
                }
            } else {
                if (this->baseFlags.curDuctType == DataHVACGlobals::Main) {
                    if (this->finalSysSizing(flags.curSysNum).SysAirMinFlowRat > 0.0) {
                        this->autoSizedValue =
                            this->finalSysSizing(flags.curSysNum).SysAirMinFlowRat * this->finalSysSizing(flags.curSysNum).DesMainVolFlow;
                    } else {
                        this->autoSizedValue = this->finalSysSizing(flags.curSysNum).DesMainVolFlow;
                    }
                } else if (this->baseFlags.curDuctType == DataHVACGlobals::Cooling) {
                    if (this->finalSysSizing(flags.curSysNum).SysAirMinFlowRat > 0.0) {
                        this->autoSizedValue =
                            this->finalSysSizing(flags.curSysNum).SysAirMinFlowRat * this->finalSysSizing(flags.curSysNum).DesCoolVolFlow;
                    } else {
                        this->autoSizedValue = this->finalSysSizing(flags.curSysNum).DesCoolVolFlow;
                    }
                } else if (this->baseFlags.curDuctType == DataHVACGlobals::Heating) {
                    this->autoSizedValue = this->finalSysSizing(flags.curSysNum).DesHeatVolFlow;
                } else {
                    this->autoSizedValue = this->finalSysSizing(flags.curSysNum).DesMainVolFlow;
                }
            }
            this->autoSizedValue *= DataEnvironment::StdRhoAir;
        }
    }
    if (this->baseFlags.printWarningFlag) {
        if (this->wasAutoSized && this->autoSizedValue > 0.0) {
            this->reportSizerOutput(
                this->baseFlags.compType, this->baseFlags.compName, "Design Size " + this->flags.sizingString, this->autoSizedValue);
        } else if (this->autoSizedValue > 0.0) {
            if ((std::abs(this->autoSizedValue - _originalValue) / _originalValue) > DataSizing::AutoVsHardSizingThreshold) {
                this->reportSizerOutput(this->baseFlags.compType,
                                             this->baseFlags.compName,
                                             "Design Size " + this->flags.sizingString,
                                             this->autoSizedValue,
                                             "User-Specified " + this->flags.sizingString,
                                             _originalValue);
            } else {
                this->reportSizerOutput(
                    this->baseFlags.compType, this->baseFlags.compName, "User-Specified " + this->flags.sizingString, _originalValue);
            }
            if (DataGlobals::DisplayExtraWarnings) {
                if ((std::abs(this->autoSizedValue - _originalValue) / _originalValue) > DataSizing::AutoVsHardSizingThreshold) {
                    ShowMessage(this->baseFlags.callingRoutine + ": Potential issue with equipment sizing for " + this->baseFlags.compType + ' ' +
                                this->baseFlags.compName);
                    ShowContinueError("User-Specified " + this->flags.sizingString + " = " + General::RoundSigDigits(_originalValue, 5));
                    ShowContinueError("differs from Design Size " + this->flags.sizingString + " = " +
                                      General::RoundSigDigits(this->autoSizedValue, 5));
                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                }
            }
        } else {
            ShowSevereError(this->baseFlags.callingRoutine + ' ' + this->baseFlags.compType + ' ' + this->baseFlags.compName +
                            ", Developer Error: Component sizing incomplete.");
            ShowContinueError("SizingString = " + this->flags.sizingString + ", SizingResult = " + General::TrimSigDigits(_originalValue, 1));
        }
    }
    return errorsFound;
}

} // namespace EnergyPlus
