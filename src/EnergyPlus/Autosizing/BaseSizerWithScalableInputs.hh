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

#ifndef BaseSizerWithScalableInputs_hh_INCLUDED
#define BaseSizerWithScalableInputs_hh_INCLUDED

#include <EnergyPlus/Autosizing/BaseSizerWithFanHeatInputs.hh>
#include <EnergyPlus/Autosizing/BaseSizerWithScalableInputs.hh>
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/api/TypeDefs.h>
#include <string>

namespace EnergyPlus {

struct EnergyPlusData;

struct BaseSizerWithScalableInputs : BaseSizerWithFanHeatInputs
{

    bool zoneCoolingOnlyFan = false;
    bool zoneHeatingOnlyFan = false;
    bool dataHRFlowSizingFlag = false;
    Real64 dataFracOfAutosizedCoolingAirflow = 0.0;
    Real64 dataFracOfAutosizedHeatingAirflow = 0.0;
    Real64 dataFlowPerCoolingCapacity = 0.0;
    Real64 dataAutosizedCoolingCapacity = 0.0;
    Real64 dataFlowPerHeatingCapacity = 0.0;
    Real64 dataAutosizedHeatingCapacity = 0.0;

    // capacity sizing
    Real64 dataCoilSizingAirInTemp = 0.0;
    Real64 dataCoilSizingAirInHumRat = 0.0;
    Real64 dataCoilSizingAirOutTemp = 0.0;
    Real64 dataCoilSizingAirOutHumRat = 0.0;
    Real64 dataCoilSizingFanCoolLoad = 0.0;
    Real64 dataCoilSizingCapFT = 0.0;
    Real64 dataTotCapCurveIndex = 0.0;
    Real64 dataTotCapCurveValue = 0.0;
    Real64 dataFracOfAutosizedCoolingCapacity = 0.0;
    Real64 dataFracOfAutosizedHeatingCapacity = 0.0;
    Real64 dataCoolCoilCap = 0.0;
    Real64 dataCoilIsSuppHeater = false;
    Real64 suppHeatCap = 0.0;
    Real64 unitaryHeatCap = 0.0;

    int zoneHVACSizingIndex = 0;
    EPVector<DataSizing::ZoneHVACSizingData> zoneHVACSizing;

    void initializeWithinEP(EnergyPlusData &state,
                            std::string_view const _compType,
                            std::string_view const _compName,
                            bool const &_printWarningFlag,
                            std::string_view const _callingRoutine) override;

    void clearState()
    {
        BaseSizerWithFanHeatInputs::clearState();
        zoneCoolingOnlyFan = false;
        zoneHeatingOnlyFan = false;
        dataHRFlowSizingFlag = false;
        dataFracOfAutosizedCoolingAirflow = 0.0;
        dataFracOfAutosizedHeatingAirflow = 0.0;
        dataFlowPerCoolingCapacity = 0.0;
        dataAutosizedCoolingCapacity = 0.0;
        dataFlowPerHeatingCapacity = 0.0;
        dataAutosizedHeatingCapacity = 0.0;
        dataCoilSizingAirInTemp = 0.0;
        dataCoilSizingAirInHumRat = 0.0;
        dataCoilSizingAirOutTemp = 0.0;
        dataCoilSizingAirOutHumRat = 0.0;
        dataCoilSizingFanCoolLoad = 0.0;
        dataCoilSizingCapFT = 0.0;
        dataTotCapCurveIndex = 0.0;
        dataTotCapCurveValue = 0.0;
        dataFracOfAutosizedCoolingCapacity = 0.0;
        dataFracOfAutosizedHeatingCapacity = 0.0;
        dataCoolCoilCap = 0.0;
        dataCoilIsSuppHeater = false;
        suppHeatCap = 0.0;
        unitaryHeatCap = 0.0;
        zoneHVACSizingIndex = 0;
        zoneHVACSizing.clear();
    }

    void setHVACSizingIndexData(int const index);
};

struct BaseSizerWithScalableInputsData : BaseGlobalStruct
{

    void clear_state() override
    {
    }
};

} // namespace EnergyPlus

#endif
