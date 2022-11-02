// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>

namespace EnergyPlus::DataZoneEnergyDemands {

void ZoneSystemSensibleDemand::beginEnvironmentInit()
{
    this->RemainingOutputRequired = 0.0;
    this->TotalOutputRequired = 0.0;
    if (allocated(this->SequencedOutputRequired)) {
        for (int equipNum = 1; equipNum <= this->NumZoneEquipment; ++equipNum) {
            this->SequencedOutputRequired(equipNum) = 0.0;
            this->SequencedOutputRequiredToHeatingSP(equipNum) = 0.0;
            this->SequencedOutputRequiredToCoolingSP(equipNum) = 0.0;
        }
    }
    this->ZoneSNLoadHeatEnergy = 0.0;
    this->ZoneSNLoadCoolEnergy = 0.0;
    this->ZoneSNLoadHeatRate = 0.0;
    this->ZoneSNLoadCoolRate = 0.0;
    this->ZoneSNLoadPredictedRate = 0.0;
    this->ZoneSNLoadPredictedHSPRate = 0.0;
    this->ZoneSNLoadPredictedCSPRate = 0.0;
}

void ZoneSystemMoistureDemand::beginEnvironmentInit()
{
    this->RemainingOutputRequired = 0.0;
    this->TotalOutputRequired = 0.0;
    if (allocated(this->SequencedOutputRequired)) {
        for (int equipNum = 1; equipNum <= this->NumZoneEquipment; ++equipNum) {
            this->SequencedOutputRequired(equipNum) = 0.0;
            this->SequencedOutputRequiredToHumidSP(equipNum) = 0.0;
            this->SequencedOutputRequiredToDehumidSP(equipNum) = 0.0;
        }
    }
    this->ZoneLTLoadHeatEnergy = 0.0;
    this->ZoneLTLoadCoolEnergy = 0.0;
    this->ZoneLTLoadHeatRate = 0.0;
    this->ZoneLTLoadCoolRate = 0.0;
    this->ZoneSensibleHeatRatio = 0.0;
    this->ZoneVaporPressureDifference = 0.0;
    this->ZoneMoisturePredictedRate = 0.0;
    this->ZoneMoisturePredictedHumSPRate = 0.0;
    this->ZoneMoisturePredictedDehumSPRate = 0.0;
}
void ZoneSystemSensibleDemand::reportZoneAirSystemSensibleLoads(EnergyPlusData &state, Real64 const SNLoad)
{
    this->ZoneSNLoadHeatRate = max(SNLoad, 0.0);
    this->ZoneSNLoadCoolRate = std::abs(min(SNLoad, 0.0));
    this->ZoneSNLoadHeatEnergy = this->ZoneSNLoadHeatRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->ZoneSNLoadCoolEnergy = this->ZoneSNLoadCoolRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
}
void ZoneSystemSensibleDemand::reportSensibleLoadsZoneMultiplier(
    EnergyPlusData &state, int const zoneNum, Real64 const totalLoad, Real64 const loadToHeatingSetPoint, Real64 const loadToCoolingSetPoint)
{
    Real64 loadCorrFactor = state.dataHeatBalFanSys->LoadCorrectionFactor(zoneNum);

    this->ZoneSNLoadPredictedRate = totalLoad * loadCorrFactor;
    this->ZoneSNLoadPredictedHSPRate = loadToHeatingSetPoint * loadCorrFactor;
    this->ZoneSNLoadPredictedCSPRate = loadToCoolingSetPoint * loadCorrFactor;

    Real64 ZoneMultFac = state.dataHeatBal->Zone(zoneNum).Multiplier * state.dataHeatBal->Zone(zoneNum).ListMultiplier;
    this->TotalOutputRequired = this->ZoneSNLoadPredictedRate * ZoneMultFac;
    this->OutputRequiredToHeatingSP = this->ZoneSNLoadPredictedHSPRate * ZoneMultFac;
    this->OutputRequiredToCoolingSP = this->ZoneSNLoadPredictedCSPRate * ZoneMultFac;

    // init each sequenced demand to the full output
    if (state.dataHeatBal->Zone(zoneNum).IsControlled && this->NumZoneEquipment > 0) {
        for (int equipNum = 1; equipNum <= this->NumZoneEquipment; ++equipNum) {
            this->SequencedOutputRequired(equipNum) = this->TotalOutputRequired;
            this->SequencedOutputRequiredToHeatingSP(equipNum) = this->OutputRequiredToHeatingSP;
            this->SequencedOutputRequiredToCoolingSP(equipNum) = this->OutputRequiredToCoolingSP;
        }
    }
}

void ZoneSystemMoistureDemand::reportZoneAirSystemMoistureLoads(EnergyPlusData &state,
                                                                Real64 const latentGain,
                                                                Real64 const sensibleLoad,
                                                                Real64 const vaporPressureDiff)
{
    this->ZoneLTLoadHeatRate = std::abs(min(latentGain, 0.0));
    this->ZoneLTLoadCoolRate = max(latentGain, 0.0);
    this->ZoneLTLoadHeatEnergy = this->ZoneLTLoadHeatRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->ZoneLTLoadCoolEnergy = this->ZoneLTLoadCoolRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    if ((sensibleLoad + latentGain) != 0.0) {
        this->ZoneSensibleHeatRatio = sensibleLoad / (sensibleLoad + latentGain);
    } else if (sensibleLoad != 0.0) {
        this->ZoneSensibleHeatRatio = 1.0;
    } else {
        this->ZoneSensibleHeatRatio = 0.0;
    }
    this->ZoneVaporPressureDifference = vaporPressureDiff;
}

void ZoneSystemMoistureDemand::reportMoistLoadsZoneMultiplier(
    EnergyPlusData &state, int const zoneNum, Real64 const totalLoad, Real64 const loadToHumidifySetPoint, Real64 const loadToDehumidifySetPoint)
{
    this->ZoneMoisturePredictedRate = totalLoad;
    this->ZoneMoisturePredictedHumSPRate = loadToHumidifySetPoint;
    this->ZoneMoisturePredictedDehumSPRate = loadToDehumidifySetPoint;

    Real64 zoneMultFac = state.dataHeatBal->Zone(zoneNum).Multiplier * state.dataHeatBal->Zone(zoneNum).ListMultiplier;

    this->TotalOutputRequired = totalLoad * zoneMultFac;
    this->OutputRequiredToHumidifyingSP = loadToHumidifySetPoint * zoneMultFac;
    this->OutputRequiredToDehumidifyingSP = loadToDehumidifySetPoint * zoneMultFac;

    // init each sequenced demand to the full output
    if (state.dataHeatBal->Zone(zoneNum).IsControlled && this->NumZoneEquipment > 0) {
        for (int equipNum = 1; equipNum <= this->NumZoneEquipment; ++equipNum) {
            this->SequencedOutputRequired(equipNum) = this->TotalOutputRequired;
            this->SequencedOutputRequiredToHumidSP(equipNum) = this->OutputRequiredToHumidifyingSP;
            this->SequencedOutputRequiredToDehumidSP(equipNum) = this->OutputRequiredToDehumidifyingSP;
        }
    }
}

} // namespace EnergyPlus::DataZoneEnergyDemands
