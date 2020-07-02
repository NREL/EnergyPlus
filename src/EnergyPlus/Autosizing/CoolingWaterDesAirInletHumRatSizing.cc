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

#include <EnergyPlus/Autosizing/CoolingWaterDesAirInletHumRatSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/api/TypeDefs.h>

#include <EnergyPlus/ReportSizingManager.hh>

namespace EnergyPlus {

void CoolingWaterDesAirInletHumRatSizer::initializeWithinEP(EnergyPlusData &state,
                                                            std::string const &_compType,
                                                            std::string const &_compName,
                                                            bool const printWarningFlag)
{
    BaseSizer::initializeWithinEP(state, _compType, _compName, printWarningFlag);
    this->sizingString = "Design Inlet Air Humidity Ratio";
    this->dataDesInletAirHumRat = DataSizing::DataDesInletAirHumRat;
    this->dataFlowUsedForSizing = DataSizing::DataFlowUsedForSizing;
}

Real64 CoolingWaterDesAirInletHumRatSizer::size(EnergyPlusData &state, Real64 _originalValue, bool &errorsFound)
{
    if (this->isNotInitialized) {
        this->errorType = AutoSizingResultType::ErrorType2;
        this->autoSizedValue = 0.0;
        errorsFound = true;
        ShowSevereError("Developer Error: autosizing of water cooling coil design air inlet humidity ratio failed.");
        ShowContinueError("Occurs in water cooling coil object= " + this->compName);
        return this->autoSizedValue;
    }
    this->isNotInitialized = true; // force use of Init then Size in subsequent calls

    this->errorType = EnergyPlus::AutoSizingResultType::NoError;
    this->preSize(state, _originalValue);
    if (this->curZoneEqNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisZone) {
            this->autoSizedValue = _originalValue;
        } else {
            if (this->termUnitIU) {
                this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).ZoneHumRatAtCoolPeak;
            } else if (this->zoneEqFanCoil) {
                Real64 desMassFlow = this->finalZoneSizing(this->curZoneEqNum).DesCoolMassFlow;
                this->autoSizedValue = ReportSizingManager::setCoolCoilInletHumRatForZoneEqSizing(
                    ReportSizingManager::setOAFracForZoneEqSizing(desMassFlow, this->zoneEqSizing(this->curZoneEqNum)),
                    this->zoneEqSizing(this->curZoneEqNum),
                    this->finalZoneSizing(this->curZoneEqNum));
            } else {
                this->autoSizedValue = this->finalZoneSizing(this->curZoneEqNum).DesCoolCoilInHumRat;
            }
        }
    } else if (this->curSysNum > 0) {
        if (!this->wasAutoSized && !this->sizingDesRunThisAirSys) {
            this->autoSizedValue = _originalValue;
        } else {
            Real64 OutAirFrac = 1.0;
            if (this->curOASysNum > 0) { // coil is in OA stream
                if (DataAirLoop::OutsideAirSys(this->curOASysNum).AirLoopDOASNum > -1) {
                    this->autoSizedValue =
                    state.dataAirLoopHVACDOAS.airloopDOAS[DataAirLoop::OutsideAirSys(this->curOASysNum).AirLoopDOASNum].SizingCoolOAHumRat;
                } else {
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).OutHumRatAtCoolPeak;
                }
            } else if (this->dataDesInletAirHumRat > 0.0) {
                this->autoSizedValue = this->dataDesInletAirHumRat;
            } else {                                                                         // coil is in main air loop
                if (DataAirSystems::PrimaryAirSystem(this->curSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                    this->autoSizedValue = this->finalSysSizing(this->curSysNum).MixHumRatAtCoolPeak;
                } else { // there is precooling of the OA stream
                    if (this->dataFlowUsedForSizing > 0.0) {
                        OutAirFrac = this->finalSysSizing(this->curSysNum).DesOutAirVolFlow / this->dataFlowUsedForSizing;
                    } else {
                        OutAirFrac = 1.0;
                    }
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    this->autoSizedValue = OutAirFrac * this->finalSysSizing(this->curSysNum).PrecoolHumRat +
                                           (1.0 - OutAirFrac) * this->finalSysSizing(this->curSysNum).RetHumRatAtCoolPeak;
                }
            }
        }
    }
    this->selectSizerOutput();
    if (this->getCoilReportObject) coilSelectionReportObj->setCoilEntAirHumRat(this->compName, this->compType, this->autoSizedValue);
    if (this->errorType != AutoSizingResultType::NoError) {
        ShowSevereError("Developer Error: autosizing of water cooling coil design air inlet humidity ratio failed.");
        ShowContinueError("Occurs in water cooling coil object= " + this->compName);
        errorsFound = true;
    }
    return this->autoSizedValue;
}

} // namespace EnergyPlus
