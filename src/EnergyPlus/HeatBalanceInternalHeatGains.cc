// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

// outside of Module

void SetupZoneInternalGain(int const ZoneNum,
                           std::string const &cComponentObject, // object class name for device contributing internal gain
                           std::string const &cComponentName,   // user unique name for device
                           int const IntGainComp_TypeOfNum,
                           Optional<Real64> ConvectionGainRate, // pointer target for remote convection gain value to be accessed
                           Optional<Real64> ReturnAirConvectionGainRate,
                           Optional<Real64> ThermalRadiationGainRate, // pointer target for remote IR radiation gain value to be accessed
                           Optional<Real64> LatentGainRate,
                           Optional<Real64> ReturnAirLatentGainRate,
                           Optional<Real64> CarbonDioxideGainRate,
                           Optional<Real64> GenericContamGainRate,
                           Optional<int> RetNodeNum // for return air heat gains
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
    using namespace DataPrecisionGlobals;
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
        ShowSevereError("SetupZoneInternalGain: developer error, trapped inconsistent internal gains object types sent to SetupZoneInternalGain");
        ShowContinueError("Object type character = " + cComponentObject);
        ShowContinueError("Type of Num object name = " + ZoneIntGainDeviceTypes(IntGainComp_TypeOfNum));
        return;
    }

    for (IntGainsNum = 1; IntGainsNum <= ZoneIntGain(ZoneNum).NumberOfDevices; ++IntGainsNum) {
        if ((ZoneIntGain(ZoneNum).Device(IntGainsNum).CompObjectType == UpperCaseObjectType) &&
            (ZoneIntGain(ZoneNum).Device(IntGainsNum).CompTypeOfNum == IntGainComp_TypeOfNum)) {
            FoundIntGainsType = true;
            if (ZoneIntGain(ZoneNum).Device(IntGainsNum).CompObjectName == UpperCaseObjectName) {
                FoundDuplicate = true;
                break;
            }
        }
    }

    if (FoundDuplicate) {
        ShowSevereError("SetupZoneInternalGain: developer error, trapped duplicate internal gains sent to SetupZoneInternalGain");
        ShowContinueError("The duplicate object user name =" + cComponentName);
        ShowContinueError("The duplicate object type = " + cComponentObject);
        ShowContinueError("This internal gain will not be modeled, and the simulation continues");
        return;
    }

    if (ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
        ZoneIntGain(ZoneNum).Device.allocate(DeviceAllocInc);
        ZoneIntGain(ZoneNum).MaxNumberOfDevices = DeviceAllocInc;
    } else {
        if (ZoneIntGain(ZoneNum).NumberOfDevices + 1 > ZoneIntGain(ZoneNum).MaxNumberOfDevices) {
            ZoneIntGain(ZoneNum).Device.redimension(ZoneIntGain(ZoneNum).MaxNumberOfDevices += DeviceAllocInc);
        }
    }
    ++ZoneIntGain(ZoneNum).NumberOfDevices;

    ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).CompObjectType = UpperCaseObjectType;
    ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).CompObjectName = UpperCaseObjectName;
    ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).CompTypeOfNum = IntGainComp_TypeOfNum;

    // note pointer assignments in code below!
    if (present(ConvectionGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrConvectGainRate >>= ConvectionGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrConvectGainRate >>= ZeroPointerVal;
    }

    if (present(ReturnAirConvectionGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirConvGainRate >>= ReturnAirConvectionGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirConvGainRate >>= ZeroPointerVal;
    }

    if (present(ThermalRadiationGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrRadiantGainRate >>= ThermalRadiationGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrRadiantGainRate >>= ZeroPointerVal;
    }

    if (present(LatentGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrLatentGainRate >>= LatentGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrLatentGainRate >>= ZeroPointerVal;
    }

    if (present(ReturnAirLatentGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirLatentGainRate >>= ReturnAirLatentGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrReturnAirLatentGainRate >>= ZeroPointerVal;
    }

    if (present(CarbonDioxideGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrCarbonDioxideGainRate >>= CarbonDioxideGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrCarbonDioxideGainRate >>= ZeroPointerVal;
    }

    if (present(GenericContamGainRate)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrGenericContamGainRate >>= GenericContamGainRate;
    } else {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).PtrGenericContamGainRate >>= ZeroPointerVal;
    }

    if (present(RetNodeNum)) {
        ZoneIntGain(ZoneNum).Device(ZoneIntGain(ZoneNum).NumberOfDevices).ReturnAirNodeNum = RetNodeNum;
    }
}

} // namespace EnergyPlus
