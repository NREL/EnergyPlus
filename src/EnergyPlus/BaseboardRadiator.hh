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

#ifndef BaseboardRadiator_hh_INCLUDED
#define BaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace BaseboardRadiator {

    struct BaseboardParams
    {
        // Members
        std::string EquipID;
        std::string Schedule;
        int SchedPtr;
        int EquipType;
        int ZonePtr;
        int WaterInletNode;
        int WaterOutletNode;
        int ControlCompTypeNum;
        int CompErrIndex;
        Real64 UA;
        Real64 WaterMassFlowRate;
        Real64 WaterVolFlowRateMax;  // m3/s
        Real64 WaterMassFlowRateMax; // kg/s
        Real64 Offset;
        Real64 AirMassFlowRate;    // kg/s
        Real64 DesAirMassFlowRate; // kg/s
        Real64 WaterInletTemp;
        Real64 WaterOutletTemp;
        Real64 WaterInletEnthalpy;
        Real64 WaterOutletEnthalpy;
        Real64 AirInletTemp;
        Real64 AirInletHumRat;
        Real64 AirOutletTemp;
        Real64 Power;
        Real64 Energy;
        int LoopNum;     // plant loop index
        int LoopSideNum; // plant loop side index
        int BranchNum;   // plant loop branch index
        int CompNum;     // plant loop component index
        int BBLoadReSimIndex;
        int BBMassFlowReSimIndex;
        int BBInletTempFlowReSimIndex;
        int HeatingCapMethod; // - Method for water baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity,
                              // CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
        Real64 ScaledHeatingCapacity; // -  water baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC
                                      // equipment, {-}, or {W/m2}

        bool MySizeFlag;
        bool CheckEquipName;
        bool SetLoopIndexFlag;
        bool MyEnvrnFlag;

        // Default Constructor
        BaseboardParams()
            : SchedPtr(0), EquipType(0), ZonePtr(0), WaterInletNode(0), WaterOutletNode(0), ControlCompTypeNum(0), CompErrIndex(0), UA(0.0),
              WaterMassFlowRate(0.0), WaterVolFlowRateMax(0.0), WaterMassFlowRateMax(0.0), Offset(0.0), AirMassFlowRate(0.0), DesAirMassFlowRate(0.0),
              WaterInletTemp(0.0), WaterOutletTemp(0.0), WaterInletEnthalpy(0.0), WaterOutletEnthalpy(0.0), AirInletTemp(0.0), AirInletHumRat(0.0),
              AirOutletTemp(0.0), Power(0.0), Energy(0.0), LoopNum(0), LoopSideNum(0), BranchNum(0), CompNum(0), BBLoadReSimIndex(0),
              BBMassFlowReSimIndex(0), BBInletTempFlowReSimIndex(0), HeatingCapMethod(0), ScaledHeatingCapacity(0.0), MySizeFlag(true),
              CheckEquipName(true), SetLoopIndexFlag(true), MyEnvrnFlag(true)
        {
        }
    };

    struct BaseboardParamsNumericFieldData
    {
        // Members
        Array1D_string FieldNames;

        // Default Constructor
        BaseboardParamsNumericFieldData() = default;
    };

    void SimBaseboard(EnergyPlusData &state,
                      std::string const &EquipName,
                      int ActualZoneNum,
                      int ControlledZoneNum,
                      bool FirstHVACIteration,
                      Real64 &PowerMet,
                      int &CompIndex);

    void GetBaseboardInput(EnergyPlusData &state);

    void InitBaseboard(EnergyPlusData &state, int BaseboardNum, int ControlledZoneNumSub);

    void SizeBaseboard(EnergyPlusData &state, int BaseboardNum);

    void SimHWConvective(EnergyPlusData &state, int &BaseboardNum, Real64 &LoadMet);

    void UpdateBaseboard(EnergyPlusData &state, int &BaseboardNum);

    Real64 HWBaseboardUAResidual(EnergyPlusData &state,
                                 Real64 UA,                       // UA of coil
                                 std::array<Real64, 2> const &Par // par(1) = design coil load [W]
    );

} // namespace BaseboardRadiator

struct BaseboardRadiatorData : BaseGlobalStruct
{

    int NumBaseboards = 0;
    bool getInputFlag = true;
    bool ZoneEquipmentListChecked = false;
    Array1D<BaseboardRadiator::BaseboardParams> Baseboard;
    Array1D<BaseboardRadiator::BaseboardParamsNumericFieldData> BaseboardParamsNumericFields;

    void clear_state() override
    {
        this->NumBaseboards = 0;
        this->getInputFlag = true;
        this->ZoneEquipmentListChecked = false;
        this->Baseboard.deallocate();
        this->BaseboardParamsNumericFields.deallocate();
    }
};

} // namespace EnergyPlus

#endif
