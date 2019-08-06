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
#include <EnergyPlus.hh>
#include <General.hh>
#include <EMSManager.hh>
#include <InputProcessing/InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Scheduling/Base.hh>
#include <Scheduling/Week.hh>
#include <Scheduling/YearWeekly.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>

namespace Scheduling {

std::vector<ScheduleYear> scheduleYears;

Real64 ScheduleYear::getCurrentValue()
{
    return this->value;
}

void ScheduleYear::processInput()
{
    std::string const thisObjectType = "Schedule:Year";
    auto const instances = EnergyPlus::inputProcessor->epJSON.find(thisObjectType);
    if (instances == EnergyPlus::inputProcessor->epJSON.end()) {
        return; // no constant schedules to process
    }
    auto &instancesValue = instances.value();
    for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
        auto const &fields = instance.value();
        auto const &thisObjectName = EnergyPlus::UtilityRoutines::MakeUPPERCase(instance.key());
        // do any pre-construction operations
        EnergyPlus::inputProcessor->markObjectAsUsed(thisObjectType, thisObjectName);
        if (std::find(Scheduling::allSchedNames.begin(), Scheduling::allSchedNames.end(), thisObjectName) != Scheduling::allSchedNames.end()) {
            EnergyPlus::ShowFatalError("Duplicate schedule name, all schedules, across all schedule types, must be uniquely named");
        }
        // then just add it to the vector via the constructor
        scheduleYears.emplace_back(thisObjectName, fields);
    }
}

void ScheduleYear::clear_state()
{
    scheduleYears.clear();
}

ScheduleYear::ScheduleYear(std::string const &objectName, nlohmann::json const &fields)
{
    // Schedule:Year,
    //       \min-fields 7
    //       \extensible:5
    //       \memo A Schedule:Year contains from 1 to 52 week schedules
    //  A1 , \field Name
    //       \required-field
    //       \type alpha
    //       \reference ScheduleNames
    //  A2 , \field Schedule Type Limits Name
    //       \type object-list
    //       \object-list ScheduleTypeLimitsNames
    //  A3 , \field Schedule:Week Name 1
    //       \begin-extensible
    //       \required-field
    //       \type object-list
    //       \object-list WeekScheduleNames
    //  N1 , \field Start Month 1
    //       \required-field
    //       \type integer
    //       \minimum 1
    //       \maximum 12
    //  N2 , \field Start Day 1
    //       \required-field
    //       \type integer
    //       \minimum 1
    //       \maximum 31
    //  N3 , \field End Month 1
    //       \required-field
    //       \type integer
    //       \minimum 1
    //       \maximum 12
    //  N4 , \field End Day 1
    //       \required-field
    //       \type integer
    //       \minimum 1
    //       \maximum 31
    this->name = objectName;
    // get a schedule type limits reference directly and store that
    if (fields.find("schedule_type_limits_name") != fields.end()) {
        this->typeLimits = ScheduleTypeData::factory(EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("schedule_type_limits_name")));
    }
    auto & weeksData = fields.at("schedule_weeks");
    for (auto const & weekData : weeksData) {
        int const startMonth = weekData.at("start_month");
        int const startDay = weekData.at("start_day");
        int const endMonth = weekData.at("end_month");
        int const endDay = weekData.at("end_day");
        auto scheduleInstance = ScheduleWeek::factory(EnergyPlus::UtilityRoutines::MakeUPPERCase(weekData.at("schedule_week_name")));
        this->weekScheduleRanges.emplace_back(startMonth, startDay, endMonth, endDay, scheduleInstance);
        if ((startMonth == 2 && startDay == 29) || (endMonth == 2 && endDay == 29)) {
            this->includesLeapYearData = true;
        }
    }
}

bool ScheduleYear::createTimeSeries()
{
    this->timeStamp.clear();
    this->values.clear();
    int endDayNum = 0;
    int currentDay = 0;
    int priorThroughTime = 0;
    int thisDayOfWeek = EnergyPlus::DataEnvironment::RunPeriodStartDayOfWeek - 1; // RunPeriodStartDayOfWeek should be 1-7, so this should be fine
    for (auto const & week : this->weekScheduleRanges) {
        int startDayNum = EnergyPlus::General::OrdinalDay(week.beginMonth, week.beginDay, 1);
        if (startDayNum != endDayNum + 1) {
            // non continuous, start day should be one more than the last end day
            EnergyPlus::ShowSevereError("Could not get full year continuity for Schedule:Year = " + this->name);
            return false;
        }
        endDayNum = EnergyPlus::General::OrdinalDay(week.endMonth, week.endDay, 1);
        int numDaysInThrough = endDayNum - startDayNum;
        for (int dayNum = 1; dayNum <= numDaysInThrough; dayNum++) {
            currentDay++;
            thisDayOfWeek++;
            if (thisDayOfWeek == 8) {
                thisDayOfWeek = 1;
            }
            Scheduling::DayType dt;
            auto const & thisEnvrnIndex = EnergyPlus::WeatherManager::Envrn;
            if (EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).KindOfEnvrn == EnergyPlus::DataGlobals::ksDesignDay) {
                dt = ScheduleBase::mapWeatherManagerDayTypeToScheduleDayType(
                    EnergyPlus::WeatherManager::DesDayInput(EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).DesignDayNum).DayType);
            } else {
                dt = ScheduleBase::getDayTypeForDayOfWeek(thisDayOfWeek); // TODO: Need to check for DD status, custom day, holiday
            }
            // call the current week schedule and ask for the day schedule for that day type
            // use the day schedule to build out the time series for that day
            auto const & thisDaySchedule = week.scheduleInstance->getScheduleDay(dt);
            for (auto const & thisUntil : thisDaySchedule->untils) {
                auto currentTimeStamp = priorThroughTime + thisUntil.timeInDay;
                this->timeStamp.push_back(currentTimeStamp);
                this->values.push_back(thisUntil.value);
            }
            priorThroughTime += 86400;
        }
    }
    if (this->typeLimits) {
        if (!this->validateTypeLimits()) {
            this->inputErrorOccurred = true;
        }
    }
    return true;
}

void ScheduleYear::updateValue(int simTime)
{
    if (this->emsActuatedOn) {
        this->value = this->emsActuatedValue;
    } else {
        // TODO: Change search to start with "this->timeStamp.begin() + this->lastIndexUsed - 1" once we can reset it
        auto item = std::lower_bound(this->timeStamp.begin(), this->timeStamp.end(), simTime);
        this->lastIndexUsed = item - this->timeStamp.begin();
        this->value = this->values[this->lastIndexUsed];
    }
}

void ScheduleYear::setupOutputVariables()
{
    for (auto &thisSchedule : scheduleYears) {
        // Set Up Reporting
        EnergyPlus::SetupOutputVariable(
            "NEW Schedule Value", EnergyPlus::OutputProcessor::Unit::None, thisSchedule.value, "Zone", "Average", thisSchedule.name);
        EnergyPlus::SetupEMSActuator("Schedule:Year", thisSchedule.name, "Schedule Value", "[ ]", thisSchedule.emsActuatedOn, thisSchedule.emsActuatedValue);
    }
}

bool ScheduleYear::validateTypeLimits()
{
//    if (this->typeLimits) {
//        if (this->value > this->typeLimits->maximum) {
//            // ShowSevereError("Value out of bounds")
//            return false;
//        } else if (this->value < this->typeLimits->minimum) {
//            // ShowSevereError("Value out of bounds")
//            return false;
//        }
//    }
    return true;
}

} // namespace Scheduling
