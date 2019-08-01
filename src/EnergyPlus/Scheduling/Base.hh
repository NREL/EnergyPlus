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

#ifndef SRC_ENERGYPLUS_SCHEDULING_BASE_HH
#define SRC_ENERGYPLUS_SCHEDULING_BASE_HH

#include <EnergyPlus.hh>
#include <Scheduling/TypeLimits.hh>
#include <string>

namespace Scheduling {

struct ScheduleBase
{
    // members common to all schedules
    std::string name = "";
    Real64 value = 0.0;
    ScheduleTypeData *typeLimits = nullptr;
    bool emsActuatedOn = false;
    Real64 emsActuatedValue = 0.0;
    bool inputErrorOccurred = false;

    // abstract functions that must be implemented by derived classes
    virtual Real64 getCurrentValue() = 0;
    virtual bool validateTypeLimits() = 0;
    virtual void updateValue(int simTime) = 0; // expecting simTime to be seconds since 1/1 00:00:00 of the first simulation year
};

static std::vector<std::string> allSchedNames;

enum class ScheduleType
{
    CONSTANT,
    COMPACT,
    YEAR,
    FILE,
    UNKNOWN
};

enum class Interpolation
{
    NONE,
    AVERAGE,
    LINEAR
};

std::vector<std::string> const allValidDayTypes({"Sunday",
                                                 "Monday",
                                                 "Tuesday",
                                                 "Wednesday",
                                                 "Thursday",
                                                 "Friday",
                                                 "Saturday",
                                                 "Holiday",
                                                 "SummerDesignDay",
                                                 "WinterDesignDay",
                                                 "CustomDay1",
                                                 "CustomDay2"});

std::vector<std::string> const typeLimitUnitTypes({"Dimensionless",
                                                   "Temperature",
                                                   "DeltaTemperature",
                                                   "PrecipitationRate",
                                                   "Angle",
                                                   "ConvectionCoefficient",
                                                   "ActivityLevel",
                                                   "Velocity",
                                                   "Capacity",
                                                   "Power",
                                                   "Availability",
                                                   "Percent",
                                                   "Control",
                                                   "Mode"});

inline int getScheduleTime(int yearNum, int monthNum, int dayNum, int hourNum, int minuteNum, int secondNum, bool const leapYearDataFound)
{
    // This function is a bit naive with leap years - if you end a non-leap-year but give it the date 2/29, it will still calculate the
    // end of january plus 29 days worth of time.  This is for efficiency, trying to eliminate as much logic as possible
    // TODO: Handle daylight savings?
    std::vector<int> cumulativeSecondsInMonthsNonLeap{
        0,        // end of month 0
        2678400,  // end of jan
        5097600,  // end of feb
        7776000,  // end of mar
        10368000, // end of apr
        13046400, // end of may
        15638400, // end of june
        18316800, // end of july
        20995200, // end of aug
        23587200, // end of sep
        26265600, // end of oct
        28857600, // end of nov
        31536000  // end of dec
    };
    if (leapYearDataFound) {
        for (int m = 2; m <= monthNum; m++) { // no need to overwrite all the months, just how far along we are in the year
            cumulativeSecondsInMonthsNonLeap[m] += 86400;
        }
    }
    int const fromYears = 31536000 * (yearNum - 1);
    int const fromMonths = cumulativeSecondsInMonthsNonLeap[monthNum - 1];
    int const fromDays = 86400 * (dayNum - 1);
    int const fromHours = 3600 * hourNum;
    int const fromMinutes = 60 * minuteNum;
    int const fromSeconds = secondNum;
    return fromYears + fromMonths + fromDays + fromHours + fromMinutes + fromSeconds;
}

} // namespace Scheduling
#endif // SRC_ENERGYPLUS_SCHEDULING_BASE_HH
