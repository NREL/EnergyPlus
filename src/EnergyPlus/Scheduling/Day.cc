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

#include <InputProcessing/InputProcessor.hh>
#include <Scheduling/Base.hh>
#include <Scheduling/Day.hh>
#include <Scheduling/TypeLimits.hh>
#include <UtilityRoutines.hh>

namespace Scheduling {

    bool dayScheduleGetInputFlag = true;
    std::vector<ScheduleDayHourly> scheduleDayHourlys;
    std::vector<ScheduleDayInterval> scheduleDayIntervals;

    ScheduleDay *Scheduling::ScheduleDay::factory(const std::string &scheduleName) {
        if (dayScheduleGetInputFlag) {
            ScheduleDay::processInput();
        }
        for (auto &hourlyDay : scheduleDayHourlys) {
            if (hourlyDay.name == scheduleName) {
                return &hourlyDay;
            }
        }
        for (auto &intervalDay : scheduleDayIntervals) {
            if (intervalDay.name == scheduleName) {
                return &intervalDay;
            }
        }
        EnergyPlus::ShowFatalError("Could not find day schedule with name = \"" + scheduleName + "\"");
        return nullptr;
    }

    void Scheduling::ScheduleDay::processInput() {
        {
            std::string thisObjectType = "Schedule:Day:Hourly";
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
                    scheduleDayHourlys.emplace_back(thisObjectName, fields);
                }
            }
        }
        {
            std::string thisObjectType = "Schedule:Day:Interval";
            auto instances = EnergyPlus::inputProcessor->epJSON.find(thisObjectType);
            if (instances != EnergyPlus::inputProcessor->epJSON.end()) {
                auto &instancesValue = instances.value();
                for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                    auto const &fields = instance.value();
                    auto const &thisObjectName = instance.key();
                    // do any pre-construction checks
                    EnergyPlus::inputProcessor->markObjectAsUsed(thisObjectType, thisObjectName);
                    // then just add it to the vector via the constructor
                    scheduleDayIntervals.emplace_back(thisObjectName, fields);
                }
            }
        }
        dayScheduleGetInputFlag = false;
    }

    void Scheduling::ScheduleDay::clear_state() {
        dayScheduleGetInputFlag = true;
        scheduleDayHourlys.clear();
        scheduleDayIntervals.clear();
    }

    Scheduling::ScheduleDayHourly::ScheduleDayHourly(std::string const &objectName, nlohmann::json const &fields) {
        // Schedule:Day:Hourly,
        //       \min-fields 26
        //       \memo A Schedule:Day:Hourly contains 24 values for each hour of the day.
        //  A1 , \field Name
        //       \required-field
        //       \type alpha
        //       \reference DayScheduleNames
        //  A2 , \field Schedule Type Limits Name
        //       \type object-list
        //       \object-list ScheduleTypeLimitsNames
        //  N1 , \field Hour 1
        //       \type real
        //       \default 0
        //  N2 , \field Hour 2
        //       \type real
        //       \default 0
        //  ...
        //  N23, \field Hour 23
        //       \type real
        //       \default 0
        //  N24; \field Hour 24
        //       \type real
        //       \default 0
        this->name = EnergyPlus::UtilityRoutines::MakeUPPERCase(objectName);
        if (fields.find("schedule_type_limits_name") != fields.end()) {
            this->typeLimits = ScheduleTypeData::factory(fields.at("schedule_type_limits_name"));
        }
        for (int i = 1; i <= 24; i++) {
            this->hourlyValues.push_back(fields.at("hour_" + std::to_string(i)));
        }
    }

    Scheduling::ScheduleDayInterval::ScheduleDayInterval(std::string const &objectName, nlohmann::json const &fields) {
        //Schedule:Day:Interval,
        //       \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
        //       \memo A Schedule:Day:Interval contains a full day of values with specified end times for each value
        //       \memo Currently, is set up to allow for 10 minute intervals for an entire day.
        //       \min-fields 5
        //  A1 , \field Name
        //       \required-field
        //       \type alpha
        //       \reference DayScheduleNames
        //  A2 , \field Schedule Type Limits Name
        //       \type object-list
        //       \object-list ScheduleTypeLimitsNames
        //  A3 , \field Interpolate to Timestep
        //       \note when the interval does not match the user specified timestep a Average choice will average between the intervals request (to
        //       \note timestep resolution. A No choice will use the interval value at the simulation timestep without regard to if it matches
        //       \note the boundary or not. A Linear choice will interpolate linearly between successive values.
        //       \type choice
        //       \key Average
        //       \key Linear
        //       \key No
        //       \default No
        // A4  , \field Time 1
        //       \begin-extensible
        //       \note "until" includes the time entered.
        //       \units hh:mm
        // N1  , \field Value Until Time 1
        // A5  , \field Time 2
        //       \note "until" includes the time entered.
        //       \units hh:mm
        // N2  , \field Value Until Time 2
        // A6  , \field Time 3
        //       \note "until" includes the time entered.
        //       \units hh:mm
        // N3  , \field Value Until Time 3
        // A7  , \field Time 4
        //       \note "until" includes the time entered.
        //       \units hh:mm
        this->name = EnergyPlus::UtilityRoutines::MakeUPPERCase(objectName);
        this->name = EnergyPlus::UtilityRoutines::MakeUPPERCase(objectName);
        if (fields.find("schedule_type_limits_name") != fields.end()) {
            this->typeLimits = ScheduleTypeData::factory(fields.at("schedule_type_limits_name"));
        }
        if (fields.find("interpolate_to_timestep") != fields.end()) {
            auto fieldValue = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("interpolate_to_timestep"));
            if (fieldValue == "LINEAR") {
                this->interpolateToTimestep = Interpolation::LINEAR;
            } else if (fieldValue == "AVERAGE") {
                this->interpolateToTimestep = Interpolation::AVERAGE;
            }
        }
        auto & intervalsData = fields.at("data");
        for (auto const & intervalData : intervalsData) {
            this->untilTimes.push_back(intervalData.at("time"));
            this->valuesUntilTimes.push_back(intervalData.at("value_until_time"));
        }
    }

}