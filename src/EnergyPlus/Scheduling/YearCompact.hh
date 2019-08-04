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

#ifndef SRC_ENERGYPLUS_SCHEDULING_YEARCOMPACT_HH
#define SRC_ENERGYPLUS_SCHEDULING_YEARCOMPACT_HH

#include <bitset>
#include <vector>

#include <EnergyPlus.hh>
#include <Scheduling/Base.hh>
#include <nlohmann/json.hpp>

namespace Scheduling {

struct Until {
    int time;
    Real64 value;
};

enum class DayType {
    // These first 12 are plain single day IDs and make up the bitset length
    SUMMERDESIGNDAY = 0,
    WINTERDESIGNDAY = 1,
    HOLIDAYS = 2,
    SUNDAY = 3,
    MONDAY = 4,
    TUESDAY = 5,
    WEDNESDAY = 6,
    THURSDAY = 7,
    FRIDAY = 8,
    SATURDAY = 9,
    CUSTOMDAY1 = 10,
    CUSTOMDAY2 = 11,
    // These two don't count for the bitset length
    WEEKDAYS = 12,
    WEEKENDS = 13,
    ALLDAYS = 14,
    ALLOTHERDAYS = 15,
    // And this is just a dummy value
    UNKNOWN = 16
};
int const NumDayTypeBits = 12;

struct For {
    Scheduling::Interpolation interpolate = Scheduling::Interpolation::NONE;
    std::bitset<NumDayTypeBits> days;
    bool hasAllOtherDays = false;
    std::vector<Until> untils;
};

struct Through {
    int timeStamp;
    std::vector<For> fors;
};

enum class FieldType {
    THROUGH,
    FOR,
    UNTIL,
    INTERPOLATE,
    VALUE,
    UNKNOWN
};

struct ScheduleCompact : ScheduleBase
{
    // constructors/destructors
    ScheduleCompact() = default;
    ScheduleCompact(std::string const &objectName, nlohmann::json const &fields);
    ~ScheduleCompact() = default;

    // overridden base class methods
    Real64 getCurrentValue() override;
    bool validateTypeLimits() override;
    void updateValue(int simTime) override;

    // static functions related to the state of all compact schedules
    static void processInput();
    static void clear_state();
    static void setupOutputVariables();

    // instance methods for this class
    FieldType processSingleField(FieldType lastFieldType, nlohmann::json datum, std::vector<FieldType> const &validFieldTypes);
    void processFields(nlohmann::json const &fieldWiseData);
    static std::string trimCompactFieldValue(std::string const &);
    int processThroughFieldValue(std::string const &);
    void processForFieldValue(const std::vector<std::string>& keys, Scheduling::For &);
    int processUntilFieldValue(std::string const &);
    bool validateContinuity();
    static DayType getDayTypeFromString(const std::string& s);
    void createTimeSeries();

    // member variables
    std::vector<Through> throughs;
    bool includesLeapYearData = false;

    // member variables related to final data storage and lookup
    std::vector<Real64> timeStamp;
    std::vector<Real64> values;
    int lastIndexUsed = 0;
};

extern std::vector<ScheduleCompact> scheduleCompacts;

}

#endif //SRC_ENERGYPLUS_SCHEDULING_YEARCOMPACT_HH
