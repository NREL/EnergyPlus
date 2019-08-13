// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <EMSManager.hh>
#include <OutputProcessor.hh>
#include <Scheduling/Base.hh>

namespace Scheduling {

bool ScheduleBase::validateTypeLimits() {
    // TODO: Add unit test for out of bounds, discrete, etc.
    Real64 const maxValue = *std::max_element(this->values.begin(), this->values.end());
    Real64 const minValue = *std::min_element(this->values.begin(), this->values.end());
    if (maxValue > this->typeLimits->maximum) {
        EnergyPlus::ShowSevereError("Value out of bounds");
        return false;
    } else if (minValue < this->typeLimits->minimum) {
        EnergyPlus::ShowSevereError("Value out of bounds");
        return false;
    }
    return true;
}

void ScheduleBase::updateValue(int const simTime) {
    if (this->emsActuatedOn) {
        this->value = this->emsActuatedValue;
    } else {
        // change search start to this->timeStamp.begin() + this->lastIndexUsed at some point, BUT, it needs to not only be reset on every
        // new environment, but also every warm-up day, etc.  Anytime that time goes backward.
        auto item = std::lower_bound(this->timeStamp.begin() + this->lastIndexUsed, this->timeStamp.end(), simTime);
        this->lastIndexUsed = item - this->timeStamp.begin();
        this->value = this->values[this->lastIndexUsed];
    }
}

DayType ScheduleBase::getDayTypeForDayOfWeek(int const dayOfWeek)
{
    switch (dayOfWeek) {
    case 1:
        return DayType::SUNDAY;
    case 2:
        return DayType::MONDAY;
    case 3:
        return DayType::TUESDAY;
    case 4:
        return DayType::WEDNESDAY;
    case 5:
        return DayType::THURSDAY;
    case 6:
        return DayType::FRIDAY;
    case 7:
        return DayType::SATURDAY;
    default:
        EnergyPlus::ShowFatalError("Invalid dayOfWeek passed to getDayTypeForDayOfWeek");
        return DayType::UNKNOWN; // hush up the compiler
    }
}

DayType ScheduleBase::mapWeatherManagerDayTypeToScheduleDayType(int const wmDayType)
{
    // Source indices, as defined in WeatherManager
    //        static Array1D_string const ValidNames(12, // don't forget Array1D was 1 based here
    //               {"SUNDAY",
    //                "MONDAY",
    //                "TUESDAY",
    //                "WEDNESDAY",
    //                "THURSDAY",
    //                "FRIDAY",
    //                "SATURDAY",
    //                "HOLIDAY",
    //                "SUMMERDESIGNDAY",
    //                "WINTERDESIGNDAY",
    //                "CUSTOMDAY1",
    //                "CUSTOMDAY2"});
    switch (wmDayType) {
    case 1:
        return DayType::SUNDAY;
    case 2:
        return DayType::MONDAY;
    case 3:
        return DayType::TUESDAY;
    case 4:
        return DayType::WEDNESDAY;
    case 5:
        return DayType::THURSDAY;
    case 6:
        return DayType::FRIDAY;
    case 7:
        return DayType::SATURDAY;
    case 8:
        return DayType::HOLIDAYS;
    case 9:
        return DayType::SUMMERDESIGNDAY;
    case 10:
        return DayType::WINTERDESIGNDAY;
    case 11:
        return DayType::CUSTOMDAY1;
    case 12:
        return DayType::CUSTOMDAY2;
    default:
        EnergyPlus::ShowFatalError("Bad WeatherManager DayType passed into mapWeatherManagerDayTypeToScheduleDayType");
        return DayType::UNKNOWN; // hush up compiler
    }
}

void ScheduleBase::setupOutputVariables()
{
    if (this->typeName == "Schedule:File:Shading") { // TODO: Convert over to regular Schedule Value once they aren't supported by the legacy ScheduleManager
        EnergyPlus::SetupOutputVariable("Schedule Value", EnergyPlus::OutputProcessor::Unit::None, this->value, "Zone", "Average", this->name);
    } else {
        EnergyPlus::SetupOutputVariable("NEW Schedule Value", EnergyPlus::OutputProcessor::Unit::None, this->value, "Zone", "Average", this->name);
    }
    EnergyPlus::SetupEMSActuator(this->typeName, this->name, "Schedule Value", "[ ]", this->emsActuatedOn, this->emsActuatedValue);
}

void ScheduleBase::resetTimeStartIndex()
{
    this->lastIndexUsed = 0;
    // this->lastLookupIndexUsed = 0;
}

Real64 ScheduleBase::lookupScheduleValue(int const hour, int const timeStep)
{
    // TODO: Keep a start index for this lookup, separate from the other start index
    int const simTime = Scheduling::Constants::secondsInDay * (EnergyPlus::DataEnvironment::DayOfYear - 1) + 3600 * ((hour - 1) + EnergyPlus::DataGlobals::TimeStepZone * timeStep);
    auto item = std::lower_bound(this->timeStamp.begin(), this->timeStamp.end(), simTime); // + this->lastLookupIndexUsed
    auto thisIndex = item - this->timeStamp.begin();
    return this->values[thisIndex];
}

}
