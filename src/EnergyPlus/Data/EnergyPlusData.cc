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

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Data/CommonIncludes.hh>

#include <memory>

namespace EnergyPlus {

    EnergyPlusData::EnergyPlusData() {
        // todo, try to eliminate the need for the singleton
        IOFiles::setSingleton(&files);
        this->dataAirLoopHVACDOAS = std::unique_ptr<AirLoopHVACDOASData>(new AirLoopHVACDOASData);
        this->dataBaseboardRadiator = std::unique_ptr<BaseboardRadiatorData>(new BaseboardRadiatorData);
        this->dataBaseboardElectric =  std::unique_ptr<BaseboardElectricData>(new BaseboardElectricData);

        this->dataWaterCoils =  std::unique_ptr<WaterCoilsData>(new WaterCoilsData);
        this->dataWaterManager =  std::unique_ptr<WaterManagerData>(new WaterManagerData);
        this->dataWaterThermalTanks =  std::unique_ptr<WaterThermalTanksData>(new WaterThermalTanksData);
        this->dataWaterToAirHeatPump =  std::unique_ptr<WaterToAirHeatPumpData>(new WaterToAirHeatPumpData);
        this->dataWaterToAirHeatPumpSimple =  std::unique_ptr<WaterToAirHeatPumpSimpleData>(new WaterToAirHeatPumpSimpleData);
        this->dataWaterUse =  std::unique_ptr<WaterUseData>(new WaterUseData);
        this->dataWeatherManager =  std::unique_ptr<WeatherManagerData>(new WeatherManagerData);
        this->dataWindowAC =  std::unique_ptr<WindowACData>(new WindowACData);
        this->dataWindowComplexManager =  std::unique_ptr<WindowComplexManagerData>(new WindowComplexManagerData);
        this->dataWindowEquivalentLayer =  std::unique_ptr<WindowEquivalentLayerData>(new WindowEquivalentLayerData);
        this->dataWindowManager =  std::unique_ptr<WindowManagerData>(new WindowManagerData);
        this->dataWindTurbine =  std::unique_ptr<WindTurbineData>(new WindTurbineData);
        this->dataZoneAirLoopEquipmentManager =  std::unique_ptr<ZoneAirLoopEquipmentManagerData>(new ZoneAirLoopEquipmentManagerData);
        this->dataZoneContaminantPredictorCorrector =  std::unique_ptr<ZoneContaminantPredictorCorrectorData>(new ZoneContaminantPredictorCorrectorData);
        this->dataZoneDehumidifier =  std::unique_ptr<ZoneDehumidifierData>(new ZoneDehumidifierData);
        this->dataZoneEquipmentManager =  std::unique_ptr<ZoneEquipmentManagerData>(new ZoneEquipmentManagerData);
        this->dataZonePlenum =  std::unique_ptr<ZonePlenumData>(new ZonePlenumData);
        this->dataZoneTempPredictorCorrector =  std::unique_ptr<ZoneTempPredictorCorrectorData>(new ZoneTempPredictorCorrectorData);
    }

    void EnergyPlusData::clear_state() {
        dataAirLoopHVACDOAS->clear_state();
        dataBaseboardElectric->clear_state();
        dataBaseboardRadiator->clear_state();
        dataBoilers.clear_state();
        dataBranchInputManager.clear_state();
        dataSteamBoilers.clear_state();
        dataChilledCeilingPanelSimple.clear_state();
        dataChillerAbsorbers.clear_state();
        dataChillerElectricEIR.clear_state();
        dataChillerExhaustAbsorption.clear_state();
        dataChillerGasAbsorption.clear_state();
        dataChillerIndirectAbsorption.clear_state();
        dataChillerReformulatedEIR.clear_state();
        dataConvectionCoefficients.clear_state();
        dataCondenserLoopTowers.clear_state();
        dataCostEstimateManager.clear_state();
        dataCoolTower.clear_state();
        dataCTElectricGenerator.clear_state();
        dataCrossVentMgr.clear_state();
        dataGlobals.clear_state();
        exteriorEnergyUse.clear_state();
        fans.clear_state();
        //outputReportTabular.clear_state();
        pipes.clear_state();
        dataPlantChillers.clear_state();

        dataWaterCoils->clear_state();
        dataWaterManager->clear_state();
        dataWaterThermalTanks->clear_state();
        dataWaterToAirHeatPump->clear_state();
        dataWaterToAirHeatPumpSimple->clear_state();
        dataWaterUse->clear_state();
        dataWeatherManager->clear_state();
        dataWindowAC->clear_state();
        dataWindowComplexManager->clear_state();
        dataWindowEquivalentLayer->clear_state();
        dataWindowManager->clear_state();
        dataWindTurbine->clear_state();
        dataZoneAirLoopEquipmentManager->clear_state();
        dataZoneContaminantPredictorCorrector->clear_state();
        dataZoneDehumidifier->clear_state();
        dataZoneEquipmentManager->clear_state();
        dataZonePlenum->clear_state();
        dataZoneTempPredictorCorrector->clear_state();
    }

}
