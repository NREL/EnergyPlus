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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

void SetupZoneInternalGain(EnergyPlusData &state,
                           int const ZoneNum,
                           std::string const &cComponentObject, // object class name for device contributing internal gain
                           std::string const &cComponentName,   // user unique name for device
                           int const IntGainComp_TypeOfNum,
                           Real64 *ConvectionGainRate, // pointer target for remote convection gain value to be accessed
                           Real64 *ReturnAirConvectionGainRate,
                           Real64 *ThermalRadiationGainRate, // pointer target for remote IR radiation gain value to be accessed
                           Real64 *LatentGainRate,
                           Real64 *ReturnAirLatentGainRate,
                           Real64 *CarbonDioxideGainRate,
                           Real64 *GenericContamGainRate,
                           int RetNodeNum // for return air heat gains
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   November 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provide a general interface for setting up devices with internal gains

    // METHODOLOGY EMPLOYED:
    // use pointers to access gain rates in device models
    // devices are internal gains like people, lights, electric equipment
    // and HVAC components with skin loss models like thermal tanks, and power conditioning.

    // Using/Aliasing
    using namespace DataHeatBalance;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const DeviceAllocInc(100);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int IntGainsNum;
    bool FoundIntGainsType;
    bool FoundDuplicate;
    std::string UpperCaseObjectType;
    std::string UpperCaseObjectName;

    // Object Data

    FoundIntGainsType = false;
    FoundDuplicate = false;
    UpperCaseObjectType = UtilityRoutines::MakeUPPERCase(cComponentObject);
    UpperCaseObjectName = UtilityRoutines::MakeUPPERCase(cComponentName);

    // Check if IntGainComp_TypeOfNum and cComponentObject are consistent
    if (!UtilityRoutines::SameString(UpperCaseObjectType, ZoneIntGainDeviceTypes(IntGainComp_TypeOfNum))) {
        ShowSevereError(state,
                        "SetupZoneInternalGain: developer error, trapped inconsistent internal gains object types sent to SetupZoneInternalGain");
        ShowContinueError(state, "Object type character = " + cComponentObject);
        ShowContinueError(state, "Type of Num object name = " + ZoneIntGainDeviceTypes(IntGainComp_TypeOfNum));
        return;
    }

    for (IntGainsNum = 1; IntGainsNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++IntGainsNum) {
        if ((state.dataHeatBal->ZoneIntGain(ZoneNum).Device(IntGainsNum).CompObjectType == UpperCaseObjectType) &&
            (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(IntGainsNum).CompTypeOfNum == IntGainComp_TypeOfNum)) {
            FoundIntGainsType = true;
            if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(IntGainsNum).CompObjectName == UpperCaseObjectName) {
                FoundDuplicate = true;
                break;
            }
        }
    }

    if (FoundDuplicate) {
        ShowSevereError(state, "SetupZoneInternalGain: developer error, trapped duplicate internal gains sent to SetupZoneInternalGain");
        ShowContinueError(state, "The duplicate object user name =" + cComponentName);
        ShowContinueError(state, "The duplicate object type = " + cComponentObject);
        ShowContinueError(state, "This internal gain will not be modeled, and the simulation continues");
        return;
    }

    if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device.allocate(DeviceAllocInc);
        state.dataHeatBal->ZoneIntGain(ZoneNum).MaxNumberOfDevices = DeviceAllocInc;
    } else {
        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices + 1 > state.dataHeatBal->ZoneIntGain(ZoneNum).MaxNumberOfDevices) {
            state.dataHeatBal->ZoneIntGain(ZoneNum).Device.redimension(state.dataHeatBal->ZoneIntGain(ZoneNum).MaxNumberOfDevices += DeviceAllocInc);
        }
    }
    ++state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices;

    state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).CompObjectType = UpperCaseObjectType;
    state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).CompObjectName = UpperCaseObjectName;
    state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).CompTypeOfNum = IntGainComp_TypeOfNum;

    // note pointer assignments in code below!
    if (ConvectionGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrConvectGainRate =
            ConvectionGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrConvectGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (ReturnAirConvectionGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirConvGainRate =
            ReturnAirConvectionGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirConvGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (ThermalRadiationGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrRadiantGainRate =
            ThermalRadiationGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrRadiantGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (LatentGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrLatentGainRate = LatentGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrLatentGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (ReturnAirLatentGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirLatentGainRate =
            ReturnAirLatentGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirLatentGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (CarbonDioxideGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrCarbonDioxideGainRate =
            CarbonDioxideGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrCarbonDioxideGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    if (GenericContamGainRate) {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrGenericContamGainRate =
            GenericContamGainRate;
    } else {
        state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).PtrGenericContamGainRate =
            &state.dataHeatBal->zeroPointerVal;
    }
    state.dataHeatBal->ZoneIntGain(ZoneNum).Device(state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices).ReturnAirNodeNum = RetNodeNum;
}

} // namespace EnergyPlus
