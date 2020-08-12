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

#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Psychrometrics.hh>

namespace EnergyPlus {

Real64 AutoCalculateSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->dataEMSOverrideON) {
        this->autoSizedValue = this->dataEMSOverride;
    } else {
        this->autoSizedValue = this->dataConstantUsedForSizing * this->dataFractionUsedForSizing;
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 MaxHeaterOutletTempSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).HeatDesTemp;
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            this->autoSizedValue = this->finalSysSizing(this->curSysNum).HeatSupTemp;
        }
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 ZoneCoolingLoadSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesCoolLoad;
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            // not implemented
            this->errorType = AutoSizingResultType::ErrorType1;
            this->autoSizedValue = 0.0;
            std::string msg = "Developer Error: For autosizing of " + this->compType + ' ' + this->compName +
                ", Airloop equipment not implemented.";
            this->addErrorMessage(msg);
        }
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 ZoneHeatingLoadSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesHeatLoad;
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            // not implemented
            this->errorType = AutoSizingResultType::ErrorType1;
            this->autoSizedValue = 0.0;
            std::string msg = "Developer Error: For autosizing of " + this->compType + ' ' + this->compName +
                ", Airloop equipment not implemented.";
            this->addErrorMessage(msg);
        }
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 ASHRAEMinSATCoolingSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing > 0.0 && this->dataFlowUsedForSizing > 0.0) {
                this->autoSizedValue =
                    this->finalZoneSizing(this->curZoneEqNum).ZoneTempAtCoolPeak -
                    (this->dataCapacityUsedForSizing / (this->dataFlowUsedForSizing * DataEnvironment::StdRhoAir *
                                                        Psychrometrics::PsyCpAirFnW(this->finalZoneSizing(this->curZoneEqNum).ZoneHumRatAtCoolPeak)));
            } else {
                this->errorType = AutoSizingResultType::ErrorType1;
                std::string msg =
                    this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataCapacityUsedForSizing = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString + ", DataFlowUsedForSizing = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
            }
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing > 0.0 && this->dataFlowUsedForSizing > 0.0 && this->dataZoneUsedForSizing > 0) {
                this->autoSizedValue = this->finalZoneSizing(this->dataZoneUsedForSizing).ZoneTempAtCoolPeak -
                                       (this->dataCapacityUsedForSizing /
                                        (this->dataFlowUsedForSizing * DataEnvironment::StdRhoAir *
                                         Psychrometrics::PsyCpAirFnW(this->finalZoneSizing(this->dataZoneUsedForSizing).ZoneHumRatAtCoolPeak)));
            } else {
                this->errorType = AutoSizingResultType::ErrorType1;
                std::string msg =
                    this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataCapacityUsedForSizing = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString + ", DataFlowUsedForSizing = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataZoneUsedForSizing = " + General::TrimSigDigits(Real64(this->dataZoneUsedForSizing), 0);
                ShowContinueError(msg);
            }
        }
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 ASHRAEMaxSATHeatingSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing > 0.0 && this->dataFlowUsedForSizing > 0.0) {
                this->autoSizedValue =
                    this->finalZoneSizing(this->curZoneEqNum).ZoneTempAtHeatPeak +
                    (this->dataCapacityUsedForSizing / (this->dataFlowUsedForSizing * DataEnvironment::StdRhoAir *
                                                        Psychrometrics::PsyCpAirFnW(this->finalZoneSizing(this->curZoneEqNum).ZoneHumRatAtHeatPeak)));
            } else {
                this->errorType = AutoSizingResultType::ErrorType1;
                std::string msg =
                    this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataCapacityUsedForSizing = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString + ", DataFlowUsedForSizing = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
            }
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataCapacityUsedForSizing > 0.0 && this->dataFlowUsedForSizing > 0.0 && this->dataZoneUsedForSizing > 0) {
                this->autoSizedValue = this->finalZoneSizing(this->dataZoneUsedForSizing).ZoneTempAtHeatPeak +
                                       (this->dataCapacityUsedForSizing /
                                        (this->dataFlowUsedForSizing * DataEnvironment::StdRhoAir *
                                         Psychrometrics::PsyCpAirFnW(this->finalZoneSizing(this->dataZoneUsedForSizing).ZoneHumRatAtHeatPeak)));
            } else {
                this->errorType = AutoSizingResultType::ErrorType1;
                std::string msg =
                    this->callingRoutine + ' ' + this->compType + ' ' + this->compName + ", Developer Error: Component sizing incomplete.";
                this->addErrorMessage(msg);
                ShowSevereError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataCapacityUsedForSizing = " + General::TrimSigDigits(this->dataCapacityUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString + ", DataFlowUsedForSizing = " + General::TrimSigDigits(this->dataFlowUsedForSizing, 1);
                this->addErrorMessage(msg);
                ShowContinueError(msg);
                msg = "SizingString = " + this->sizingString +
                      ", DataZoneUsedForSizing = " + General::TrimSigDigits(Real64(this->dataZoneUsedForSizing), 0);
                ShowContinueError(msg);
            }
        }
    }
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 DesiccantDehumidifierBFPerfDataFaceVelocitySizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->dataEMSOverrideON) {
        this->autoSizedValue = this->dataEMSOverride;
    } else {
        this->autoSizedValue = 4.30551 + 0.01969 * this->dataAirFlowUsedForSizing;
        this->autoSizedValue = min(6.0, this->autoSizedValue);
    }
    if (this->isEpJSON) this->sizingString = "nominal_air_face_velocity [m/s]";
    this->selectSizerOutput(errorsFound);
    return this->autoSizedValue;
}

Real64 HeatingCoilDesAirInletTempSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            // not implemented
            this->errorType = AutoSizingResultType::ErrorType1;
            this->autoSizedValue = 0.0;
            std::string msg = "Developer Error: For autosizing of " + this->compType + ' ' + this->compName +
                ", Zone equipment not implemented.";
            this->addErrorMessage(msg);
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataDesicRegCoil && this->dataDesicDehumNum > 0) {
                // change to Data* global
                if (DesiccantDehumidifiers::DesicDehum(this->dataDesicDehumNum).RegenInletIsOutsideAirNode) {
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).HeatOutTemp;
                } else {
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).HeatRetTemp;
                }
            }
        }
    }
    this->selectSizerOutput(errorsFound);
    if (this->getCoilReportObject) coilSelectionReportObj->setCoilEntAirTemp(this->compName, this->compType, this->autoSizedValue, this->curSysNum, this->curZoneEqNum);
    return this->autoSizedValue;
}

Real64 HeatingCoilDesAirOutletTempSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            // not implemented
            this->errorType = AutoSizingResultType::ErrorType1;
            this->autoSizedValue = 0.0;
            std::string msg = "Developer Error: For autosizing of " + this->compType + ' ' + this->compName +
                ", Zone equipment not implemented.";
            this->addErrorMessage(msg);
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataDesicRegCoil && this->dataDesicDehumNum > 0) {
                // change to Data* global
                this->autoSizedValue = DesiccantDehumidifiers::DesicDehum(this->dataDesicDehumNum).RegenSetPointTemp;
            }
        }
    }
    this->selectSizerOutput(errorsFound);
    if (this->getCoilReportObject) coilSelectionReportObj->setCoilLvgAirTemp(this->compName, this->compType, this->autoSizedValue);
    return this->autoSizedValue;
}

Real64 HeatingCoilDesAirInletHumRatSizer::size(Real64 _originalValue, bool &errorsFound)
{
    if (!this->checkInitialized(errorsFound)) {
        return 0.0;
    }
    this->preSize(_originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            // not implemented
            this->errorType = AutoSizingResultType::ErrorType1;
            this->autoSizedValue = 0.0;
            std::string msg = "Developer Error: For autosizing of " + this->compType + ' ' + this->compName +
                ", Zone equipment not implemented.";
            this->addErrorMessage(msg);
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->dataDesicRegCoil) {
                // change to Data* global
                if (DesiccantDehumidifiers::DesicDehum(this->dataDesicDehumNum).RegenInletIsOutsideAirNode) {
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).HeatOutHumRat;
                } else {
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).HeatRetHumRat;
                }
            }
        }
    }
    this->selectSizerOutput(errorsFound);
    if (this->getCoilReportObject) coilSelectionReportObj->setCoilEntAirHumRat(this->compName, this->compType, this->autoSizedValue);
    return this->autoSizedValue;
}

} // namespace EnergyPlus
