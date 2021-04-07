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

#ifndef ScheduleManager_hh_INCLUDED
#define ScheduleManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ScheduleManager {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS

    int const MaxDayTypes(12);

    enum class SchedType : int
    {
        Unassigned = 0,
        ScheduleInput_year = 1,
        ScheduleInput_compact = 2,
        ScheduleInput_file = 3,
        ScheduleInput_constant = 4,
        ScheduleInput_external = 5
    };

    // DERIVED TYPE DEFINITIONS

    // INTERFACE BLOCK SPECIFICATIONS

    // MODULE VARIABLE DECLARATIONS:

    enum class ScheduleInterpolation
    {
        No,      // no interpolation
        Average, // interpolation only to resolve time intervals not matching timestep lengths (this was previously interpolate:yes)
        Linear   // linear interpolation from the previous time to the current time for the entire schedule
    };

    // Derived Types Variables

    // Types

    struct ScheduleTypeData
    {
        // Members
        std::string Name; // Schedule Type Name
        bool Limited;     // True if this Schedule Type has limits
        Real64 Minimum;   // Minimum for limited schedule
        Real64 Maximum;   // Maximum for limited schedule
        bool IsReal;      // True if this is a "real" schedule, false if integer
        int UnitType;     // reference ScheduleTypeLimit table

        // Default Constructor
        ScheduleTypeData() : Limited(false), Minimum(0.0), Maximum(0.0), IsReal(true), UnitType(0)
        {
        }
    };

    struct DayScheduleData
    {
        // Members
        std::string Name;                           // Day Schedule Name
        int ScheduleTypePtr;                        // Index of Schedule Type
        ScheduleInterpolation IntervalInterpolated; // Indicator for interval interpolation. If not "interpolated", False.  Else True
        bool Used;                                  // Indicator for this schedule being "used".
        Array2D<Real64> TSValue;                    // Value array by simulation timestep
        Real64 TSValMax;                            // maximum of all TSValue's
        Real64 TSValMin;                            // minimum of all TSValue's

        // Default Constructor
        DayScheduleData() : ScheduleTypePtr(0), IntervalInterpolated(ScheduleInterpolation::No), Used(false), TSValMax(0.0), TSValMin(0.0)
        {
        }
    };

    struct WeekScheduleData
    {
        // Members
        std::string Name;               // Week Schedule Name
        bool Used;                      // Indicator for this schedule being "used".
        Array1D_int DaySchedulePointer; // Index of Day Schedule

        // Default Constructor
        WeekScheduleData() : Used(false), DaySchedulePointer(MaxDayTypes, 0)
        {
        }
    };

    struct ScheduleData
    {
        // Members
        std::string Name;                          // Schedule Name
        int ScheduleTypePtr;                       // Index of Schedule Type
        Array1D_int WeekSchedulePointer;           // one created for each day of possible simulation
        SchedType SchType = SchedType::Unassigned; // what kind of object has been input.
        bool Used;                                 // Indicator for this schedule being "used".
        bool MaxMinSet;                            // Max/min values have been stored for this schedule
        Real64 MaxValue;                           // Maximum value for this schedule
        Real64 MinValue;                           // Minimum value for this schedule
        Real64 CurrentValue;                       // For Reporting
        bool EMSActuatedOn;                        // indicates if EMS computed
        Real64 EMSValue;

        // Default Constructor
        ScheduleData()
            : ScheduleTypePtr(0), WeekSchedulePointer(366, 0), Used(false), MaxMinSet(false), MaxValue(0.0), MinValue(0.0), CurrentValue(0.0),
              EMSActuatedOn(false), EMSValue(0.0)
        {
        }
    };

    // Functions

    void ProcessScheduleInput(EnergyPlusData &state);

    void ReportScheduleDetails(EnergyPlusData &state, int const LevelOfDetail); // =1: hourly; =2: timestep; = 3: make IDF excerpt

    // Returns the CurrentScheduleValue
    Real64 GetCurrentScheduleValue(EnergyPlusData &state, int const ScheduleIndex);

    // Updates each schedule value to the current timestep
    // Uses EMS value if actuated, otherwise calls LookUpScheduleValue with ThisHour=DataGlobals::HourOfDay, ThisTimeStep=DataGlobals::TimeStep
    void UpdateScheduleValues(EnergyPlusData &state);

    // Looks up a given Schedule value for an hour & timestep, minding whether DST is enabled or not
    Real64 LookUpScheduleValue(EnergyPlusData &state,
                               int const ScheduleIndex,
                               int const ThisHour,
                               int const ThisTimeStep = -1 // Negative => unspecified, will use NumOfTimeStepInHour
    );

    int GetScheduleIndex(EnergyPlusData &state, std::string const &ScheduleName);

    std::string GetScheduleType(EnergyPlusData &state, int const ScheduleIndex);

    int GetDayScheduleIndex(EnergyPlusData &state, std::string &ScheduleName);

    void GetScheduleValuesForDay(
        EnergyPlusData &state, int const ScheduleIndex, Array2S<Real64> DayValues, Optional_int_const JDay = _, Optional_int_const CurDayofWeek = _);

    void GetSingleDayScheduleValues(EnergyPlusData &state,
                                    int const DayScheduleIndex, // Index of the DaySchedule for values
                                    Array2S<Real64> DayValues   // Returned set of values
    );

    void ExternalInterfaceSetSchedule(EnergyPlusData &state,
                                      int &ScheduleIndex,
                                      Real64 &Value // The new value for the schedule
    );

    void ProcessIntervalFields(EnergyPlusData &state,
                               Array1S_string const Untils,
                               Array1S<Real64> const Numbers,
                               int const NumUntils,
                               int const NumNumbers,
                               Array2A<Real64> MinuteValue,
                               Array2A_bool SetMinuteValue,
                               bool &ErrorsFound,
                               std::string const &DayScheduleName,     // Name (used for errors)
                               std::string const &ErrContext,          // Context (used for errors)
                               ScheduleInterpolation interpolationKind // enumeration on how to interpolate values in schedule
    );

    void DecodeHHMMField(EnergyPlusData &state,
                         std::string const &FieldValue,          // Input field value
                         int &RetHH,                             // Returned "hour"
                         int &RetMM,                             // Returned "minute"
                         bool &ErrorsFound,                      // True if errors found in this field
                         std::string const &DayScheduleName,     // originating day schedule name
                         std::string const &FullFieldValue,      // Full Input field value
                         ScheduleInterpolation interpolationKind // enumeration on how to interpolate values in schedule
    );

    bool isMinuteMultipleOfTimestep(int minute, int numMinutesPerTimestep);

    void ProcessForDayTypes(EnergyPlusData &state,
                            std::string const &ForDayField, // Field containing the "FOR:..."
                            Array1D_bool &TheseDays,        // Array to contain returned "true" days
                            Array1D_bool &AlReady,          // Array of days already done
                            bool &ErrorsFound               // Will be true if error found.
    );

    bool CheckScheduleValueMinMax(EnergyPlusData &state,
                                  int const ScheduleIndex,      // Which Schedule being tested
                                  std::string const &MinString, // Minimum indicator ('>', '>=')
                                  Real64 const Minimum          // Minimum desired value
    );

    bool CheckScheduleValueMinMax(EnergyPlusData &state,
                                  int const ScheduleIndex,      // Which Schedule being tested
                                  std::string const &MinString, // Minimum indicator ('>', '>=')
                                  Real64 const Minimum,         // Minimum desired value
                                  std::string const &MaxString, // Maximum indicator ('<', ',=')
                                  Real64 const Maximum          // Maximum desired value
    );

    bool CheckScheduleValueMinMax(EnergyPlusData &state,
                                  int const ScheduleIndex,      // Which Schedule being tested
                                  std::string const &MinString, // Minimum indicator ('>', '>=')
                                  Real32 const Minimum          // Minimum desired value
    );

    bool CheckScheduleValueMinMax(EnergyPlusData &state,
                                  int const ScheduleIndex,      // Which Schedule being tested
                                  std::string const &MinString, // Minimum indicator ('>', '>=')
                                  Real32 const Minimum,         // Minimum desired value
                                  std::string const &MaxString, // Maximum indicator ('<', ',=')
                                  Real32 const Maximum          // Maximum desired value
    );

    bool CheckScheduleValue(EnergyPlusData &state,
                            int const ScheduleIndex, // Which Schedule being tested
                            Real64 const Value       // Actual desired value
    );

    bool CheckScheduleValue(EnergyPlusData &state,
                            int const ScheduleIndex, // Which Schedule being tested
                            int const Value          // Actual desired value
    );

    bool CheckDayScheduleValueMinMax(EnergyPlusData &state,
                                     int const ScheduleIndex,            // Which Day Schedule being tested
                                     Real64 const Minimum,               // Minimum desired value
                                     std::string const &MinString,       // Minimum indicator ('>', '>=')
                                     Optional<Real64 const> Maximum = _, // Maximum desired value
                                     Optional_string_const MaxString = _ // Maximum indicator ('<', ',=')
    );

    bool CheckDayScheduleValueMinMax(EnergyPlusData &state,
                                     int const ScheduleIndex,            // Which Day Schedule being tested
                                     Real32 const Minimum,               // Minimum desired value
                                     std::string const &MinString,       // Minimum indicator ('>', '>=')
                                     Optional<Real32 const> Maximum = _, // Maximum desired value
                                     Optional_string_const MaxString = _ // Maximum indicator ('<', ',=')
    );

    bool HasFractionalScheduleValue(EnergyPlusData &state, int const ScheduleIndex); // Which Schedule being tested

    Real64 GetScheduleMinValue(EnergyPlusData &state, int const ScheduleIndex); // Which Schedule being tested

    Real64 GetScheduleMaxValue(EnergyPlusData &state, int const ScheduleIndex); // Which Schedule being tested

    std::string GetScheduleName(EnergyPlusData &state, int const ScheduleIndex);

    void ReportScheduleValues(EnergyPlusData &state);

    void ReportOrphanSchedules(EnergyPlusData &state);

    Real64 ScheduleAnnualFullLoadHours(EnergyPlusData &state,
                                       int const ScheduleIndex,  // Which Schedule being tested
                                       int const StartDayOfWeek, // Day of week for start of year
                                       bool const isItLeapYear   // true if it is a leap year containing February 29
    );

    Real64 ScheduleAverageHoursPerWeek(EnergyPlusData &state,
                                       int const ScheduleIndex,  // Which Schedule being tested
                                       int const StartDayOfWeek, // Day of week for start of year
                                       bool const isItLeapYear   // true if it is a leap year containing February 29
    );

    Real64 ScheduleHoursGT1perc(EnergyPlusData &state,
                                int const ScheduleIndex,  // Which Schedule being tested
                                int const StartDayOfWeek, // Day of week for start of year
                                bool const isItLeapYear   // true if it is a leap year containing February 29
    );

    int GetNumberOfSchedules(EnergyPlusData &state);

} // namespace ScheduleManager

struct ScheduleManagerData : BaseGlobalStruct
{
    bool CheckScheduleValueMinMaxRunOnceOnly = true;
    bool DoScheduleReportingSetup = true;
    std::unordered_map<std::string, std::string> UniqueDayScheduleNames;
    std::unordered_map<std::string, std::string> UniqueWeekScheduleNames;
    std::unordered_map<std::string, std::string> UniqueScheduleNames;

    // Integer Variables for the Module
    int NumScheduleTypes = 0;
    int NumDaySchedules = 0;
    int NumWeekSchedules = 0;
    int NumSchedules = 0;

    // Logical Variables for Module
    bool ScheduleInputProcessed = false; // This is false until the Schedule Input has been processed.
    bool ScheduleDSTSFileWarningIssued = false;
    bool ScheduleFileShadingProcessed = false; // This is false unless there is a Schedule:File:Shading object.

    // Object Data
    Array1D<ScheduleManager::ScheduleTypeData> ScheduleType; // Allowed Schedule Types
    Array1D<ScheduleManager::DayScheduleData> DaySchedule;   // Day Schedule Storage
    Array1D<ScheduleManager::WeekScheduleData> WeekSchedule; // Week Schedule Storage
    Array1D<ScheduleManager::ScheduleData> Schedule;         // Schedule Storage

    void clear_state() override
    {
        CheckScheduleValueMinMaxRunOnceOnly = true;
        UniqueDayScheduleNames.clear();
        UniqueWeekScheduleNames.clear();
        UniqueScheduleNames.clear();
        DoScheduleReportingSetup = true;

        NumScheduleTypes = 0;
        NumDaySchedules = 0;
        NumWeekSchedules = 0;
        NumSchedules = 0;

        ScheduleInputProcessed = false;
        ScheduleDSTSFileWarningIssued = false;
        ScheduleFileShadingProcessed = false;

        ScheduleType.clear(); // Allowed Schedule Types
        DaySchedule.clear();  // Day Schedule Storage
        WeekSchedule.clear(); // Week Schedule Storage
        Schedule.clear();     // Schedule Storage
    }
};

} // namespace EnergyPlus

#endif
