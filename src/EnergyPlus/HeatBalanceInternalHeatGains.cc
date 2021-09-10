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
    // Distribute zone internal gain across all spaces in the zone weighted by floor area
    Real64 gainFrac = 1.0;
    for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
        if (state.dataHeatBal->Zone(ZoneNum).numSpaces > 1) {
            gainFrac = state.dataHeatBal->space(spaceNum).floorArea / state.dataHeatBal->Zone(ZoneNum).FloorArea;
        }
        SetupSpaceInternalGain(state,
                               spaceNum,
                               gainFrac,
                               cComponentObject,
                               cComponentName,
                               IntGainComp_TypeOfNum,
                               ConvectionGainRate,
                               ReturnAirConvectionGainRate,
                               ThermalRadiationGainRate,
                               LatentGainRate,
                               ReturnAirLatentGainRate,
                               CarbonDioxideGainRate,
                               GenericContamGainRate,
                               RetNodeNum);
    }
}
void SetupSpaceInternalGain(EnergyPlusData &state,
                            int const spaceNum,
                            Real64 spaceGainFraction,            // Fraction of gain value assigned to this space
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

    // PURPOSE OF THIS SUBROUTINE:
    // provide a general interface for setting up devices with internal gains

    // METHODOLOGY EMPLOYED:
    // use pointers to access gain rates in device models
    // devices are internal gains like people, lights, electric equipment
    // and HVAC components with skin loss models like thermal tanks, and power conditioning.

    using namespace DataHeatBalance;

    int const DeviceAllocInc(100);

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

    auto &thisIntGain = state.dataHeatBal->spaceIntGainDevices(spaceNum);
    for (IntGainsNum = 1; IntGainsNum <= thisIntGain.numberOfDevices; ++IntGainsNum) {
        if ((thisIntGain.device(IntGainsNum).CompObjectType == UpperCaseObjectType) &&
            (thisIntGain.device(IntGainsNum).CompTypeOfNum == IntGainComp_TypeOfNum)) {
            FoundIntGainsType = true;
            if (thisIntGain.device(IntGainsNum).CompObjectName == UpperCaseObjectName) {
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

    if (thisIntGain.numberOfDevices == 0) {
        thisIntGain.device.allocate(DeviceAllocInc);
        thisIntGain.maxNumberOfDevices = DeviceAllocInc;
    } else {
        if (thisIntGain.numberOfDevices + 1 > thisIntGain.maxNumberOfDevices) {
            thisIntGain.device.redimension(thisIntGain.maxNumberOfDevices += DeviceAllocInc);
        }
    }
    ++thisIntGain.numberOfDevices;

    thisIntGain.device(thisIntGain.numberOfDevices).CompObjectType = UpperCaseObjectType;
    thisIntGain.device(thisIntGain.numberOfDevices).CompObjectName = UpperCaseObjectName;
    thisIntGain.device(thisIntGain.numberOfDevices).CompTypeOfNum = IntGainComp_TypeOfNum;
    thisIntGain.device(thisIntGain.numberOfDevices).spaceGainFrac = spaceGainFraction;

    // note pointer assignments in code below!
    if (ConvectionGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrConvectGainRate = ConvectionGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrConvectGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (ReturnAirConvectionGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrReturnAirConvGainRate = ReturnAirConvectionGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrReturnAirConvGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (ThermalRadiationGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrRadiantGainRate = ThermalRadiationGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrRadiantGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (LatentGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrLatentGainRate = LatentGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrLatentGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (ReturnAirLatentGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrReturnAirLatentGainRate = ReturnAirLatentGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrReturnAirLatentGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (CarbonDioxideGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrCarbonDioxideGainRate = CarbonDioxideGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrCarbonDioxideGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    if (GenericContamGainRate) {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrGenericContamGainRate = GenericContamGainRate;
    } else {
        thisIntGain.device(thisIntGain.numberOfDevices).PtrGenericContamGainRate = &state.dataHeatBal->zeroPointerVal;
    }
    thisIntGain.device(thisIntGain.numberOfDevices).ReturnAirNodeNum = RetNodeNum;
}

} // namespace EnergyPlus
