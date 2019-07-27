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

#include <exception>

#include <EnergyPlus.hh>
#include <EMSManager.hh>
#include <InputProcessing/InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Scheduling/Base.hh>
#include <Scheduling/YearCompact.hh>
#include <UtilityRoutines.hh>

namespace Scheduling {

std::vector<ScheduleCompact> scheduleCompacts;

Real64 ScheduleCompact::getCurrentValue()
{
    return this->value;
}

void ScheduleCompact::processInput()
{
    ScheduleCompact c;
    c.value = 0;
    c.name = "";
    scheduleCompacts.push_back(c);
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
    // get a schedule type limits reference directly and store that
    if (fields.find("schedule_type_limits_name") != fields.end()) {
        this->typeLimits = ScheduleTypeData::factory(fields.at("schedule_type_limits_name"));
    }
    std::map<CompactField::FieldType, std::vector<CompactField::FieldType>> validNextFieldTypes =
        {
            {CompactField::FieldType::THROUGH, {CompactField::FieldType::FOR}},
            {CompactField::FieldType::FOR, {CompactField::FieldType::UNTIL, CompactField::FieldType::INTERPOLATE}},
            {CompactField::FieldType::INTERPOLATE, {CompactField::FieldType::UNTIL}},
            {CompactField::FieldType::UNTIL, {CompactField::FieldType::VALUE}},
            {CompactField::FieldType::VALUE, {CompactField::FieldType::UNTIL, CompactField::FieldType::FOR, CompactField::FieldType::THROUGH}},
        };
    std::vector<CompactField::FieldType> validFieldTypes{CompactField::FieldType::THROUGH}; // start with through, that should always be first
    auto fieldWiseData = fields.at("data");
    for (auto const & datum : fieldWiseData) {
        CompactField::FieldType lastFieldType = CompactField::FieldType::UNKNOWN;
        try {
            std::string possibleString = datum.at("field");
            // parse the string for type and value
            possibleString = EnergyPlus::UtilityRoutines::MakeUPPERCase(possibleString);
            if (possibleString.compare(0, 7, "THROUGH") == 0) {
                lastFieldType = CompactField::FieldType::THROUGH;
                if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                    EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED THROUGH WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
                }
                this->throughs.emplace_back();
                this->throughs.back().date = possibleString.substr(8);
            } else if (possibleString.compare(0, 3, "FOR") == 0) {
                lastFieldType = CompactField::FieldType::FOR;
                if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                    EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED FOR WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
                }
                this->throughs.back().fors.emplace_back();
                std::istringstream iss(possibleString.substr(4));
                std::vector<std::string> results(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());
                this->throughs.back().fors.back().days = results;
            } else if (possibleString.compare(0, 11, "INTERPOLATE") == 0) {
                lastFieldType = CompactField::FieldType::INTERPOLATE;
                if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                    EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED INTERPOLATE WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
                }
                this->throughs.back().fors.back().sInterpolate = possibleString.substr(12);
            } else if (possibleString.compare(0, 5, "UNTIL") == 0) {
                lastFieldType = CompactField::FieldType::UNTIL;
                if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                    EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED UNTIL WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
                }
                this->throughs.back().fors.back().untils.emplace_back();
                this->throughs.back().fors.back().untils.back().sTime = possibleString.substr(6);
            }
        } catch (nlohmann::detail::type_error & error) {
            lastFieldType = CompactField::FieldType::VALUE;
            // if it wasn't a string, it should've been a schedule value (float)
            if (std::find(validFieldTypes.begin(), validFieldTypes.end(), lastFieldType) == validFieldTypes.end()) {
                EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE, ENCOUNTERED VALUE WHEN IT WAS NOT VALID, ALLOWED TYPES = whatever");
            }
            Real64 possibleFloat = datum.at("field");
            this->throughs.back().fors.back().untils.back().value = possibleFloat;
        }
        if (lastFieldType == CompactField::FieldType::UNKNOWN) {
            EnergyPlus::ShowFatalError("BAD INPUT ON COMPACT SCHEDULE; COULD NOT PROCESS FIELD");
        }
        validFieldTypes = validNextFieldTypes[lastFieldType];
    }
    if (this->typeLimits && !this->valuesInBounds()) {
        EnergyPlus::ShowFatalError("Schedule bounds error causes program termination");
    }
}

void ScheduleCompact::updateValue(int EP_UNUSED(simTime))
{
    if (this->emsActuatedOn) {
        this->value = this->emsActuatedValue;
    } else {
        // this->value = this->value;
    }
}

void ScheduleCompact::setupOutputVariables()
{
    for (auto &thisSchedule : scheduleCompacts) {
        // Set Up Reporting
        EnergyPlus::SetupOutputVariable(
            "NEW Schedule Value", EnergyPlus::OutputProcessor::Unit::None, thisSchedule.value, "Zone", "Average", thisSchedule.name);
        EnergyPlus::SetupEMSActuator("Schedule:Compact", thisSchedule.name, "Schedule Value", "[ ]", thisSchedule.emsActuatedOn, thisSchedule.emsActuatedValue);
    }
}

bool ScheduleCompact::valuesInBounds()
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
