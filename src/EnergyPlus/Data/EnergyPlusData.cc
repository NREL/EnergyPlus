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
        this->dataAirLoop = std::unique_ptr<DataAirLoopData>(new DataAirLoopData);
        this->dataAirLoopHVACDOAS = std::unique_ptr<AirLoopHVACDOASData>(new AirLoopHVACDOASData);
        this->dataBaseboardElectric =  std::unique_ptr<BaseboardElectricData>(new BaseboardElectricData);
        this->dataBaseboardRadiator = std::unique_ptr<BaseboardRadiatorData>(new BaseboardRadiatorData);
        this->dataBoilers = std::unique_ptr<BoilersData>(new BoilersData);
        this->dataBoilerSteam = std::unique_ptr<BoilerSteamData>(new BoilerSteamData);
        this->dataBranchInputManager = std::unique_ptr<BranchInputManagerData>(new BranchInputManagerData);
        this->dataChilledCeilingPanelSimple = std::unique_ptr<ChilledCeilingPanelSimpleData>(new ChilledCeilingPanelSimpleData);
        this->dataChillerAbsorber = std::unique_ptr<ChillerAbsorberData>(new ChillerAbsorberData);
        this->dataChillerElectricEIR = std::unique_ptr<ChillerElectricEIRData>(new ChillerElectricEIRData);
        this->dataChillerExhaustAbsorption = std::unique_ptr<ChillerExhaustAbsorptionData>(new ChillerExhaustAbsorptionData);
        this->dataChillerGasAbsorption = std::unique_ptr<ChillerGasAbsorptionData>(new ChillerGasAbsorptionData);
        this->dataChillerIndirectAbsorption = std::unique_ptr<ChillerIndirectAbsoprtionData>(new ChillerIndirectAbsoprtionData);
        this->dataChillerReformulatedEIR = std::unique_ptr<ChillerReformulatedEIRData>(new ChillerReformulatedEIRData);
        this->dataConvectionCoefficient = std::unique_ptr<ConvectionCoefficientsData>(new ConvectionCoefficientsData);
        this->dataCurveManager = std::unique_ptr<CurveManagerData>(new CurveManagerData);
    }

    void EnergyPlusData::clear_state() {
        this->dataAirLoop->clear_state();
        this->dataAirLoopHVACDOAS->clear_state();
        this->dataBaseboardElectric->clear_state();
        this->dataBaseboardRadiator->clear_state();
        this->dataBoilers->clear_state();
        this->dataBoilerSteam->clear_state();
        this->dataBranchInputManager->clear_state();
        this->dataChilledCeilingPanelSimple->clear_state();
        this->dataChillerAbsorber->clear_state();
        this->dataChillerElectricEIR->clear_state();
        this->dataChillerExhaustAbsorption->clear_state();
        this->dataChillerGasAbsorption->clear_state();
        this->dataChillerIndirectAbsorption->clear_state();
        this->dataChillerReformulatedEIR->clear_state();
        this->dataConvectionCoefficient->clear_state();
        this->dataCurveManager->clear_state();

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
        dataWaterUse.clear_state();
        dataWindowAC.clear_state();
        dataWindowComplexManager.clear_state();
        dataWindowEquivalentLayer.clear_state();
        dataWindowManager.clear_state();
        dataWindTurbine.clear_state();
        dataZoneAirLoopEquipmentManager.clear_state();
        dataZoneContaminantPredictorCorrector.clear_state();
        dataZoneDehumidifier.clear_state();
        dataZoneEquipmentManager.clear_state();
        dataZonePlenum.clear_state();
        dataZoneTempPredictorCorrector.clear_state();
    }

}