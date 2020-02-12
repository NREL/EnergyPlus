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

#include <algorithm>
#include <regex>
#include <bitset>

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Scheduling/Base.hh>
#include <EnergyPlus/Scheduling/YearCompact.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace Scheduling {

std::vector<ScheduleCompact> scheduleCompacts;

void ScheduleCompact::processInput()
{
    // Now we'll go through normal processing operations
    std::string const thisObjectType = "Schedule:Compact";
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
        scheduleCompacts.emplace_back(thisObjectName, fields);
    }
}

void ScheduleCompact::clear_state()
{
    scheduleCompacts.clear();
}

ScheduleCompact::ScheduleCompact(std::string const &objectName, nlohmann::json const &fields)
{
    // Schedule:Compact,
    //   \extensible:1 - repeat last field, remembering to remove ; from "inner" fields.
    //   \min-fields 5
    //   \memo Irregular object.  Does not follow the usual definition for fields.  Fields A3... are:
    //   \memo Through: Date
    //   \memo For: Applicable days (ref: Schedule:Week:Compact)
    //   \memo Interpolate: Average/Linear/No (ref: Schedule:Day:Interval) -- optional, if not used will be "No"
    //   \memo Until: <Time> (ref: Schedule:Day:Interval)
    //   \memo <numeric value>
    //   \memo words "Through","For","Interpolate","Until" must be included.
    //   \format compactSchedule
    //  A1 , \field Name
    //       \required-field
    //       \type alpha
    //       \reference ScheduleNames
    //  A2 , \field Schedule Type Limits Name
    //       \type object-list
    //       \object-list ScheduleTypeLimitsNames
    //  A3 , \field Field 1
    //       \begin-extensible
    //  A4 , \field Field 2
    //  A5 , \field Field 3
    //  A6 , \field Field 4
    this->name = objectName;
    this->typeName = "Schedule:Compact";
    // get a schedule type limits reference directly and store that
    if (fields.find("schedule_type_limits_name") != fields.end()) {
        this->typeLimits = ScheduleTypeData::factory(EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("schedule_type_limits_name")));
    }
    auto fieldWiseData = fields.at("data");
    this->processFields(fieldWiseData);
}

bool ScheduleCompact::validateContinuity()
{
    int lastThroughTime = -1;
    for (auto const &thisThrough : this->throughs) {
        if (thisThrough.timeStamp <= lastThroughTime) {
            EnergyPlus::ShowFatalError("Invalid sequence of Through timestamps for Schedule:Compact = " + this->name);
        }
        lastThroughTime = thisThrough.timeStamp;
        if (thisThrough.fors.front().hasAllOtherDays) {
            EnergyPlus::ShowFatalError("AllOtherDays used in first For: field for Schedule:Compact = " + this->name);
        }
        std::bitset<Scheduling::NumDayTypeBits> runningTally;
        for (auto const &thisFor : thisThrough.fors) {
            if ((runningTally & thisFor.days).any()) {
                EnergyPlus::ShowFatalError("Duplicate days found in For: field for Schedule:Compact = " + this->name);
            }
            if (thisFor.hasAllOtherDays) {
                runningTally.set();
            } else {
                runningTally |= thisFor.days;
            }
            int lastUntilTime = -1;
            for (auto const &thisUntil : thisFor.untils) {
                if (thisUntil.time <= lastUntilTime) {
                    EnergyPlus::ShowFatalError("Invalid sequence of Until timestamps for Schedule:Compact = " + this->name);
                }
                lastUntilTime = thisUntil.time;
            }
            if (thisFor.untils.back().time != Scheduling::Constants::secondsInDay) {
                EnergyPlus::ShowFatalError("Invalid Until timestamps, they do not reach the end of the day for Schedule:Compact = " + this->name);
            }
        }
        if (!runningTally.all()) {
            EnergyPlus::ShowFatalError("Insufficient day coverage in For: fields for Schedule:Compact = " + this->name);
        }
    }
    // check final through to make sure we hit the end of the year
    int midnightMorningLastDayOfYear = 31449600;
    if (this->includesLeapYearData) {
        midnightMorningLastDayOfYear += Scheduling::Constants::secondsInDay;
    }
    if (this->throughs.back().timeStamp != midnightMorningLastDayOfYear) {
        EnergyPlus::ShowFatalError("Invalid Through timestamps, they do not reach the end of the year for Schedule:Compact = " + this->name);
    }
    return true;
}

void ScheduleCompact::createTimeSeries() {
    // TODO: Handle multiyear
    // TODO: Handle leap year
    // TODO: Handle daylight savings time
    int priorThroughTime = 0;
    int currentDay = 0;
    int thisDayOfWeek = EnergyPlus::DataEnvironment::RunPeriodStartDayOfWeek - 1; // RunPeriodStartDayOfWeek should be 1-7, so this should be fine
    for (auto const & thisThrough : this->throughs) {
        int numDaysInThrough = ((thisThrough.timeStamp - priorThroughTime) / Scheduling::Constants::secondsInDay) + 1; // TODO: double check this + 1 on the end
        for (int dayNum = 1; dayNum <= numDaysInThrough; dayNum++) {
            currentDay++;
            thisDayOfWeek++;
            if (thisDayOfWeek == 0) {
                // weird situation with a weather environment that is run period but weather file simulation is turned off
                // presumably because we've overridden the run with the -w flag?
                // what about the doingsizing portion?
                thisDayOfWeek = 1;
            } else if (thisDayOfWeek == 8) {
                thisDayOfWeek = 1;
            }
            Scheduling::DayType dt;
            auto const & thisEnvrnIndex = EnergyPlus::WeatherManager::Envrn;
            if (thisEnvrnIndex == 0) {
                dt = DayType::MONDAY;
            } else if (EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).KindOfEnvrn == EnergyPlus::DataGlobals::ksDesignDay) {
                dt = ScheduleBase::mapWeatherManagerDayTypeToScheduleDayType(
                    EnergyPlus::WeatherManager::DesDayInput(EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).DesignDayNum).DayType);
            } else if (EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).KindOfEnvrn == EnergyPlus::DataGlobals::ksRunPeriodDesign) {
                dt = ScheduleBase::mapWeatherManagerDayTypeToScheduleDayType(static_cast<int>(
                    EnergyPlus::WeatherManager::RunPeriodDesignInput(EnergyPlus::WeatherManager::Environment(thisEnvrnIndex).RunPeriodDesignNum)
                        .startWeekDay));
            } else {
                dt = ScheduleBase::getDayTypeForDayOfWeek(thisDayOfWeek); // TODO: Need to check for DD status, custom day, holiday
            }
            std::bitset<Scheduling::NumDayTypeBits> bs;
            bs.set((int)dt);
            bool found = false;
            for (auto const & thisFor : thisThrough.fors) {
                if (thisFor.hasAllOtherDays || (thisFor.days & bs).any()) {
                    found = true;
                    for (auto const & thisUntil : thisFor.untils) {
                        auto currentTimeStamp = priorThroughTime + ((dayNum - 1) * Scheduling::Constants::secondsInDay) + thisUntil.time;
                        this->timeStamp.push_back(currentTimeStamp);
                        this->values.push_back(thisUntil.value);
                    }
                    break;
                }
            }
            if (!found) {
                EnergyPlus::ShowFatalError("Could not achieve continuity, something went wrong, schedule = " + this->name);
            }
        }
        priorThroughTime = thisThrough.timeStamp + Scheduling::Constants::secondsInDay; // TODO: Make sure this should include the last day's 86400
    }
}

void ScheduleCompact::processFields(nlohmann::json const &fieldWiseData)
{
    std::map<FieldType, std::vector<FieldType>> validNextFieldTypes = {
        {FieldType::THROUGH, {FieldType::FOR}},
        {FieldType::FOR, {FieldType::UNTIL, FieldType::INTERPOLATE}},
        {FieldType::INTERPOLATE, {FieldType::UNTIL}},
        {FieldType::UNTIL, {FieldType::VALUE}},
        {FieldType::VALUE, {FieldType::UNTIL, FieldType::FOR, FieldType::THROUGH}},
    };
    std::vector<FieldType> validFieldTypes{FieldType::THROUGH}; // start with through, that should always be first
    for (auto const &datum : fieldWiseData) {
        FieldType lastFieldType = FieldType::UNKNOWN;
        lastFieldType = this->processSingleField(lastFieldType, datum, validFieldTypes);
        if (lastFieldType == FieldType::UNKNOWN) {
            EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE; COULD NOT PROCESS FIELD");
        }
        validFieldTypes = validNextFieldTypes[lastFieldType];
    }
    if (!this->validateContinuity()) {
        EnergyPlus::ShowFatalError("Preceding Schedule:Compact issues cause program termination");
    }
}

FieldType ScheduleCompact::processSingleField(FieldType lastFieldType, nlohmann::json datum, std::vector<FieldType> const &validFieldTypes)
{
    try {
        std::string possibleString = datum.at("field");
        // parse the string for type and value
        possibleString = EnergyPlus::UtilityRoutines::MakeUPPERCase(possibleString);
        if (possibleString.compare(0, 7, "THROUGH") == 0) {
            lastFieldType = FieldType::THROUGH;
            if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED THROUGH WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
            }
            this->throughs.emplace_back();
            auto date = ScheduleCompact::trimCompactFieldValue(possibleString.substr(7));
            auto newTimeStamp = this->processThroughFieldValue(date);
            this->throughs.back().timeStamp = newTimeStamp;
        } else if (possibleString.compare(0, 3, "FOR") == 0) {
            lastFieldType = FieldType::FOR;
            if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED FOR WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
            }
            this->throughs.back().fors.emplace_back();
            auto theseDayTypes = ScheduleCompact::trimCompactFieldValue(possibleString.substr(3));
            std::istringstream iss(theseDayTypes);
            std::vector<std::string> results(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());
            this->processForFieldValue(results, this->throughs.back().fors.back());
        } else if (possibleString.compare(0, 11, "INTERPOLATE") == 0) {
            lastFieldType = FieldType::INTERPOLATE;
            if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED INTERPOLATE WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
            }
            std::string sInterpolate = ScheduleCompact::trimCompactFieldValue(possibleString.substr(11));
            if (sInterpolate == "LINEAR") {
                this->throughs.back().fors.back().interpolate = Interpolation::LINEAR;
            } else if (sInterpolate == "AVERAGE") {
                this->throughs.back().fors.back().interpolate = Interpolation::AVERAGE;
            }
        } else if (possibleString.compare(0, 5, "UNTIL") == 0) {
            lastFieldType = FieldType::UNTIL;
            if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED UNTIL WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
            }
            this->throughs.back().fors.back().untils.emplace_back();
            auto sTime = ScheduleCompact::trimCompactFieldValue(possibleString.substr(5));
            this->throughs.back().fors.back().untils.back().time = this->processUntilFieldValue(sTime);
        }
    } catch (nlohmann::detail::type_error &error) {
        lastFieldType = FieldType::VALUE;
        // if it wasn't a string, it should've been a schedule value (float)
        if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
            EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED VALUE WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
        }
        Real64 possibleFloat = datum.at("field");
        this->throughs.back().fors.back().untils.back().value = possibleFloat;
    }
    return lastFieldType;
}

std::string ScheduleCompact::trimCompactFieldValue(std::string const &s)
{
    std::string newString = s;
    // first trim off any space preceding an optional colon
    newString = EnergyPlus::UtilityRoutines::epTrim(newString);
    // then trim off a colon if it exists
    if (newString[0] == ':') {
        newString.erase(0, 1);
    }
    // then continue trimming after the colon is gone
    newString = EnergyPlus::UtilityRoutines::epTrim(newString);
    return newString;
}

int ScheduleCompact::processThroughFieldValue(std::string const &s)
{
    // check for missing value
    if (s.empty()) {
        EnergyPlus::ShowFatalError("Blank value on Through: field for Schedule:Compact named " + this->name);
    }
    // check that it matches the right pattern
    std::regex re("[0-9]?[0-9]\\/[0-9][0-9]?");
    std::smatch m;
    if (!std::regex_match(s, m, re)) { // I expect one single match, and needs to apply to whole string, so just regex_match, not regex_search
        EnergyPlus::ShowFatalError("Invalid value on Through: field for Schedule:Compact named " + this->name + "; invalid input value: " + s);
    }
    // then process out the month and day, two possibilities: M/DD and MM/DD, I'm just checking the position of the slash
    std::string sMonth, sDay;
    if (s.size() == 3u) {
        // 9/1
        sMonth = s.substr(0, 1);
        sDay = s.substr(2, 1);
    } else if (s.size() == 4u) {
        if (s[1] == '/') {
            // 9/01
            sMonth = s.substr(0, 1);
            sDay = s.substr(2, 2);
        } else {
            // 09/1
            sMonth = s.substr(0, 2);
            sDay = s.substr(3, 1);
        }
    } else { // must be MM/DD if it passed the regex above
        // 09/01
        sMonth = s.substr(0, 2);
        sDay = s.substr(3, s.size() - 3);
    }
    int month = std::stoi(sMonth);
    int day = std::stoi(sDay);
    // validate the days and months
    if (month < 1 || month > 12) {
        EnergyPlus::ShowFatalError("Out of range month (" + sMonth + ") for Schedule:Compact " + this->name);
    }
    switch (month) {
    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
        if (day < 1 || day > 31) {
            EnergyPlus::ShowFatalError("Out of range day (" + sDay + ") for month " + sMonth + " for Schedule:Compact " + this->name);
        }
        break;
    case 4:
    case 6:
    case 9:
    case 11:
        if (day < 1 || day > 30) {
            EnergyPlus::ShowFatalError("Out of range day (" + sDay + ") for month " + sMonth + " for Schedule:Compact " + this->name);
        }
        break;
    default:                       // will be month 2
        if (day == 29) {
            this->includesLeapYearData = true;
        } else if (day < 1 || day > 29) {
            EnergyPlus::ShowFatalError("Out of range day (" + sDay + ") for month " + sMonth + " for Schedule:Compact " + this->name);
        }
        break;
    }
    return Scheduling::getScheduleTime(1, month, day, 0, 0, 0, this->includesLeapYearData); // TODO: Handle multiple years
}

int ScheduleCompact::processUntilFieldValue(std::string const &s)
{
    // check for missing value
    if (s.empty()) {
        EnergyPlus::ShowFatalError("Blank value on Until: field for Schedule:Compact named " + this->name);
    }
    // check that it matches the right pattern
    std::regex re("[0-9]?[0-9]:[0-9][0-9]");
    std::smatch m;
    if (!std::regex_match(s, m, re)) { // I expect one single match, and needs to apply to whole string, so just regex_match, not regex_search
        EnergyPlus::ShowFatalError("Invalid value on Until: field for Schedule:Compact named " + this->name + "; invalid input value: " + s);
    }
    // then process out the month and day, two possibilities: M/DD and MM/DD, I'm just checking the position of the slash
    std::string sHour, sMinute;
    if (s[1] == ':') {
        sHour = s.substr(0, 1);
        sMinute = s.substr(2, 2);
    } else { // must be HH:MM if it passed the regex above
        sHour = s.substr(0, 2);
        sMinute = s.substr(3, 2);
    }
    int hour = std::stoi(sHour);
    int minute = std::stoi(sMinute);
    // validate the hours and minutes
    if (hour < 0 || hour > 24) { // TODO: Do we handle subhourly right now, like 00:15?
        EnergyPlus::ShowFatalError("Out of range hour (" + sHour + ") for Schedule:Compact " + this->name);
    }
    if (minute < 0 || minute > 59) {
        EnergyPlus::ShowFatalError("Out of range minute (" + sMinute + ") for Schedule:Compact " + this->name);
    }
    return Scheduling::getScheduleTime(1, 1, 1, hour, minute, 0, this->includesLeapYearData); // TODO: Make a more efficient routine for time-only
}

DayType ScheduleCompact::getDayTypeFromString(const std::string &s)
{
    if (s == "WEEKDAY" || s == "WEEKDAYS") {
        return DayType::WEEKDAYS;
    } else if (s == "WEEKEND" || s == "WEEKENDS") {
        return DayType::WEEKENDS;
    } else if (s == "HOLIDAY" || s == "HOLIDAYS") {
        return DayType::HOLIDAYS;
    } else if (s == "ALLDAY" || s == "ALLDAYS") {
        return DayType::ALLDAYS;
    } else if (s == "SUMMERDESIGNDAY" || s == "SUMMERDESIGNDAYS") {
        return DayType::SUMMERDESIGNDAY;
    } else if (s == "WINTERDESIGNDAY" || s == "WINTERDESIGNDAYS") {
        return DayType::WINTERDESIGNDAY;
    } else if (s == "SUNDAY" || s == "SUNDAYS") {
        return DayType::SUNDAY;
    } else if (s == "MONDAY" || s == "MONDAYS") {
        return DayType::MONDAY;
    } else if (s == "TUESDAY" || s == "TUESDAYS") {
        return DayType::TUESDAY;
    } else if (s == "WEDNESDAY" || s == "WEDNESDAYS") {
        return DayType::WEDNESDAY;
    } else if (s == "THURSDAY" || s == "THURSDAYS") {
        return DayType::THURSDAY;
    } else if (s == "FRIDAY" || s == "FRIDAYS") {
        return DayType::FRIDAY;
    } else if (s == "SATURDAY" || s == "SATURDAYS") {
        return DayType::SATURDAY;
    } else if (s == "CUSTOMDAY1") {
        return DayType::CUSTOMDAY1;
    } else if (s == "CUSTOMDAY2") {
        return DayType::CUSTOMDAY2;
    } else if (s == "ALLOTHERDAY" || s == "ALLOTHERDAYS") {
        return DayType::ALLOTHERDAYS;
    } else {
        return DayType::UNKNOWN;
    }
}

void ScheduleCompact::processForFieldValue(const std::vector<std::string> &keys, Scheduling::For &thisFor)
{
    auto &bits = thisFor.days;
    for (auto const &key : keys) {
        auto dayType = ScheduleCompact::getDayTypeFromString(key);
        switch (dayType) {
        case DayType::WEEKDAYS:
            bits.set((int)DayType::MONDAY);
            bits.set((int)DayType::TUESDAY);
            bits.set((int)DayType::WEDNESDAY);
            bits.set((int)DayType::THURSDAY);
            bits.set((int)DayType::FRIDAY);
            break;
        case DayType::WEEKENDS:
            bits.set((int)DayType::SUNDAY);
            bits.set((int)DayType::SATURDAY);
            break;
        case DayType::SUMMERDESIGNDAY:
        case DayType::WINTERDESIGNDAY:
        case DayType::HOLIDAYS:
        case DayType::SUNDAY:
        case DayType::MONDAY:
        case DayType::TUESDAY:
        case DayType::WEDNESDAY:
        case DayType::THURSDAY:
        case DayType::FRIDAY:
        case DayType::SATURDAY:
        case DayType::CUSTOMDAY1:
        case DayType::CUSTOMDAY2:
            bits.set((int)dayType);
            break;
        case DayType::ALLDAYS:
            bits.set();
            break;
        case DayType::ALLOTHERDAYS:
            thisFor.hasAllOtherDays = true;
            break;
        case DayType::UNKNOWN:
            EnergyPlus::ShowFatalError("Bad key type in For field of Schedule:Compact = " + this->name);
        }
    }
}

void ScheduleCompact::prepareForNewEnvironment()
{
    this->timeStamp.clear();
    this->values.clear();
    this->lastIndexUsed = 0;
    this->createTimeSeries();
    // TODO: do we need to validate continuity?
    if (this->typeLimits) {
        this->validateTypeLimits();
    }
    // TODO: Check for error flag?
}

} // namespace Scheduling
