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

#include <EnergyPlus/Scheduling/Base.hh>
#include <EnergyPlus/Scheduling/Week.hh>
#include <EnergyPlus/Scheduling/Day.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>

#include <nlohmann/json.hpp>

namespace Scheduling {

bool getInputFlag = true;
std::vector<ScheduleWeekDaily> scheduleWeekDailies;
std::vector<ScheduleWeekCompact> scheduleWeekCompacts;

void ScheduleWeek::clear_state()
{
    getInputFlag = true;
    scheduleWeekDailies.clear();
    scheduleWeekCompacts.clear();
}

ScheduleWeek *ScheduleWeek::factory(const std::string& scheduleName)
{
    // get input if needed
    if (getInputFlag) {
        ScheduleWeek::processInput();
    }
    // then loop over supported vectors to try to find it
    for (auto & dailySchedule : scheduleWeekDailies) {
        if (dailySchedule.name == scheduleName) {
            return &dailySchedule;
        }
    }
    for (auto & compactSchedule : scheduleWeekCompacts) {
        if (compactSchedule.name == scheduleName) {
            return &compactSchedule;
        }
    }
    // error if not found
//    EnergyPlus::ShowFatalError("Could not find week schedule named \"" + scheduleName + "\"");
    return nullptr;
}
void ScheduleWeek::processInput()
{
    {
        std::string thisObjectType = "Schedule:Week:Daily";
        auto instances = EnergyPlus::inputProcessor->epJSON.find(thisObjectType);
        if (instances != EnergyPlus::inputProcessor->epJSON.end()) {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = EnergyPlus::UtilityRoutines::MakeUPPERCase(instance.key());
                // do any pre-construction operations
                EnergyPlus::inputProcessor->markObjectAsUsed(thisObjectType, thisObjectName);
                if (std::find(Scheduling::allSchedNames.begin(), Scheduling::allSchedNames.end(), thisObjectName) !=
                    Scheduling::allSchedNames.end()) {
                    EnergyPlus::ShowFatalError("Duplicate schedule name, all schedules, across all schedule types, must be uniquely named");
                }
                // then just add it to the vector via the constructor
                scheduleWeekDailies.emplace_back(thisObjectName, fields);
            }
        }
    }
    {
        std::string thisObjectType = "Schedule:Week:Compact";
        auto instances = EnergyPlus::inputProcessor->epJSON.find(thisObjectType);
        if (instances != EnergyPlus::inputProcessor->epJSON.end()) {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = EnergyPlus::UtilityRoutines::MakeUPPERCase(instance.key());
                // do any pre-construction operations
                EnergyPlus::inputProcessor->markObjectAsUsed(thisObjectType, thisObjectName);
                if (std::find(Scheduling::allSchedNames.begin(), Scheduling::allSchedNames.end(), thisObjectName) !=
                    Scheduling::allSchedNames.end()) {
                    EnergyPlus::ShowFatalError("Duplicate schedule name, all schedules, across all schedule types, must be uniquely named");
                }
                // then just add it to the vector via the constructor
                scheduleWeekCompacts.emplace_back(thisObjectName, fields);
            }
        }
    }
    // get schedule:week:daily and schedule:week:compact, fill appropriate vectors
    getInputFlag = false;
}

ScheduleWeekDaily::ScheduleWeekDaily(std::string const &objectName, nlohmann::json const &fields)
{
    // Schedule:Week:Daily,
    //  A1 , \field Name
    //  A2 , \field Sunday Schedule:Day Name
    //  A3 , \field Monday Schedule:Day Name
    //  A4 , \field Tuesday Schedule:Day Name
    //  A5 , \field Wednesday Schedule:Day Name
    //  A6 , \field Thursday Schedule:Day Name
    //  A7 , \field Friday Schedule:Day Name
    //  A8 , \field Saturday Schedule:Day Name
    //  A9 , \field Holiday Schedule:Day Name
    //  A10, \field SummerDesignDay Schedule:Day Name
    //  A11, \field WinterDesignDay Schedule:Day Name
    //  A12, \field CustomDay1 Schedule:Day Name
    //  A13; \field CustomDay2 Schedule:Day Name
    auto & uc = EnergyPlus::UtilityRoutines::MakeUPPERCase;
    this->name = EnergyPlus::UtilityRoutines::MakeUPPERCase(objectName);
    this->days[Scheduling::DayType::SUNDAY] = ScheduleDay::factory(uc(fields.at("sunday_schedule_day_name")));
    this->days[Scheduling::DayType::MONDAY] = ScheduleDay::factory(uc(fields.at("monday_schedule_day_name")));
    this->days[Scheduling::DayType::TUESDAY] = ScheduleDay::factory(uc(fields.at("tuesday_schedule_day_name")));
    this->days[Scheduling::DayType::WEDNESDAY] = ScheduleDay::factory(uc(fields.at("wednesday_schedule_day_name")));
    this->days[Scheduling::DayType::THURSDAY] = ScheduleDay::factory(uc(fields.at("thursday_schedule_day_name")));
    this->days[Scheduling::DayType::FRIDAY] = ScheduleDay::factory(uc(fields.at("friday_schedule_day_name")));
    this->days[Scheduling::DayType::SATURDAY] = ScheduleDay::factory(uc(fields.at("saturday_schedule_day_name")));
    this->days[Scheduling::DayType::HOLIDAYS] = ScheduleDay::factory(uc(fields.at("holiday_schedule_day_name")));
    this->days[Scheduling::DayType::SUMMERDESIGNDAY] = ScheduleDay::factory(uc(fields.at("summerdesignday_schedule_day_name")));
    this->days[Scheduling::DayType::WINTERDESIGNDAY] = ScheduleDay::factory(uc(fields.at("winterdesignday_schedule_day_name")));
    this->days[Scheduling::DayType::CUSTOMDAY1] = ScheduleDay::factory(uc(fields.at("customday1_schedule_day_name")));
    this->days[Scheduling::DayType::CUSTOMDAY2] = ScheduleDay::factory(uc(fields.at("customday2_schedule_day_name")));
}

ScheduleDay *ScheduleWeekDaily::getScheduleDay(Scheduling::DayType dt)
{
    return this->days[dt];
}

ScheduleWeekCompact::ScheduleWeekCompact(std::string const &objectName, nlohmann::json const &fields)
{
    // Schedule:Week:Compact,
    //  \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
    //  \memo Compact definition for Schedule:Day:List
    //  \min-fields 3
    //  A1 , \field Name
    //  A2 , \field DayType List 1
    //       \begin-extensible
    //       \note Choices can be combined on single line
    //       \note if separated by spaces. i.e. "Holiday Weekends"
    //       \required-field
    //       \type choice
    //       \key AllDays
    //       \key AllOtherDays
    //       \key Weekdays
    //       \key Weekends
    //       \key Sunday
    //       \key Monday
    //       \key Tuesday
    //       \key Wednesday
    //       \key Thursday
    //       \key Friday
    //       \key Saturday
    //       \key Holiday
    //       \key SummerDesignDay
    //       \key WinterDesignDay
    //       \key CustomDay1
    //       \key CustomDay2
    //  A3 , \field Schedule:Day Name 1
    //       \required-field
    //       \type object-list
    //       \object-list DayScheduleNames
    //  A4 , \field DayType List 2
    this->name = EnergyPlus::UtilityRoutines::MakeUPPERCase(objectName);
    auto & daysData = fields.at("data");
    for (auto const & dayData : daysData) {
        std::string dayType = EnergyPlus::UtilityRoutines::MakeUPPERCase(dayData.at("daytype_list"));
        // OK, processing this kinda stinks,
        // the string will be trimmed of surrounding whitespace initially, so first check for an optional "For" at the beginning, and remove
        if (dayType.substr(0, 3) == "FOR") {
            dayType.erase(0, 3);
        }
        // then we need to trim off any whitespace in case of "For : SUNDAY" types
        dayType = EnergyPlus::UtilityRoutines::epTrim(dayType); // TODO: maybe epTrim should just operate on the string passed in
        // now we could have ": SUNDAY" left, so trim off an optional colon
        if (dayType.substr(0, 1) == ":") {
            dayType.erase(0, 1);
        }
        // then we could still have " SUNDAY" left, so trim off whitespace again
        dayType = EnergyPlus::UtilityRoutines::epTrim(dayType);
        // now we can process it into a proper type
        ScheduleDay *schedule = ScheduleDay::factory(EnergyPlus::UtilityRoutines::MakeUPPERCase(dayData.at("schedule_day_name")));
        if (dayType == "SUNDAY") {
            this->days[Scheduling::DayType::SUNDAY] = schedule;
        } else if (dayType == "MONDAY") {
            this->days[Scheduling::DayType::MONDAY] = schedule;
        } else if (dayType == "TUESDAY") {
            this->days[Scheduling::DayType::TUESDAY] = schedule;
        } else if (dayType == "WEDNESDAY") {
            this->days[Scheduling::DayType::WEDNESDAY] = schedule;
        } else if (dayType == "THURSDAY") {
            this->days[Scheduling::DayType::THURSDAY] = schedule;
        } else if (dayType == "FRIDAY") {
            this->days[Scheduling::DayType::FRIDAY] = schedule;
        } else if (dayType == "SATURDAY") {
            this->days[Scheduling::DayType::SATURDAY] = schedule;
        } else if (dayType == "HOLIDAY") {
            this->days[Scheduling::DayType::HOLIDAYS] = schedule;
        } else if (dayType == "SUMMERDESIGNDAY") {
            this->days[Scheduling::DayType::SUMMERDESIGNDAY] = schedule;
        } else if (dayType == "WINTERDESIGNDAY") {
            this->days[Scheduling::DayType::WINTERDESIGNDAY] = schedule;
        } else if (dayType == "CUSTOMDAY1") {
            this->days[Scheduling::DayType::CUSTOMDAY1] = schedule;
        } else if (dayType == "CUSTOMDAY2") {
            this->days[Scheduling::DayType::CUSTOMDAY2] = schedule;
        } else if (dayType == "WEEKDAYS") {
            this->days[Scheduling::DayType::MONDAY] = schedule;
            this->days[Scheduling::DayType::TUESDAY] = schedule;
            this->days[Scheduling::DayType::WEDNESDAY] = schedule;
            this->days[Scheduling::DayType::THURSDAY] = schedule;
            this->days[Scheduling::DayType::FRIDAY] = schedule;
        } else if (dayType == "WEEKENDS") {
            this->days[Scheduling::DayType::SATURDAY] = schedule;
            this->days[Scheduling::DayType::SUNDAY] = schedule;
        } else if (dayType == "ALLDAYS") {
            this->days[Scheduling::DayType::MONDAY] = schedule;
            this->days[Scheduling::DayType::TUESDAY] = schedule;
            this->days[Scheduling::DayType::WEDNESDAY] = schedule;
            this->days[Scheduling::DayType::THURSDAY] = schedule;
            this->days[Scheduling::DayType::FRIDAY] = schedule;
            this->days[Scheduling::DayType::SATURDAY] = schedule;
            this->days[Scheduling::DayType::SUNDAY] = schedule;
            this->days[Scheduling::DayType::HOLIDAYS] = schedule;
            this->days[Scheduling::DayType::SUMMERDESIGNDAY] = schedule;
            this->days[Scheduling::DayType::WINTERDESIGNDAY] = schedule;
            this->days[Scheduling::DayType::CUSTOMDAY1] = schedule;
            this->days[Scheduling::DayType::CUSTOMDAY2] = schedule;
        } else if (dayType == "ALLOTHERDAYS") {
            this->days[Scheduling::DayType::ALLOTHERDAYS] = schedule;
            this->hasAllOtherDays = true;
        }
    }
}

ScheduleDay *ScheduleWeekCompact::getScheduleDay(Scheduling::DayType dt)
{
    if (this->days.find(dt) != this->days.end()) {
        return this->days[dt];
    }
    if (this->hasAllOtherDays) {
        return this->days[Scheduling::DayType::ALLOTHERDAYS];
    }
    // TODO: Do we just return a nullptr to allow the schedule to default to zero?
    EnergyPlus::ShowFatalError("Could not find day schedule for day type SSSSS for Schedule:Week:Daily");
    return nullptr; // hush up the compiler
}

} // namespace Scheduling
