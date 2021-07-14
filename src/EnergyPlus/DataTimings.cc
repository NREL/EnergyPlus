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

// ObjexxFCL Headers
#include <ObjexxFCL/time.hh>

// EnergyPlus Headers
#include <EnergyPlus/CommandLineInterface.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataTimings.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

#ifdef EP_NO_Timings
#undef EP_Timings
#endif

namespace DataTimings {

    // MODULE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   January 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This data-only module is a repository for data and routines for timing within EnergyPlus.

    // Functions

    void epStartTime(
#ifdef EP_NO_Timings
        [[maybe_unused]] std::string const &ctimingElementstring
#endif
#ifdef EP_Timings
            std::string const &ctimingElementstring
#endif
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Implement a timing scheme using start-stop (ref: epStopTime) that will help
        // developers pinpoint problems.

        // METHODOLOGY EMPLOYED:
        // structure similar to recurring error structure.

        // Object Data
        Array1D<timings> tempTiming; // used for reallocate.

#ifdef EP_NO_Timings
        return;
#endif
#ifdef EP_Timings
        int loop;  // testing if already in structure
        int found; // indicator for element
        if (state.dataTimingsData->NumTimingElements == 0) {
            state.dataTimingsData->MaxTimingElements = 250;
            Timing.allocate(state.dataTimingsData->MaxTimingElements);
        } else if (state.dataTimingsData->NumTimingElements == state.dataTimingsData->MaxTimingElements) {
            tempTiming.allocate(state.dataTimingsData->MaxTimingElements + 250);
            tempTiming({1, state.dataTimingsData->MaxTimingElements}) = Timing({1, state.dataTimingsData->MaxTimingElements});
            Timing.deallocate();
            state.dataTimingsData->MaxTimingElements += 250;
            Timing.allocate(state.dataTimingsData->MaxTimingElements);
            Timing({1, state.dataTimingsData->MaxTimingElements}) = tempTiming({1, state.dataTimingsData->MaxTimingElements});
            tempTiming.deallocate();
        }

        found = 0;
        for (loop = 1; loop <= state.dataTimingsData->NumTimingElements; ++loop) {
            if (Timing(loop).Element != ctimingElementstring) continue;
            found = loop;
        }

        if (found == 0) {
            ++state.dataTimingsData->NumTimingElements;
            Timing(state.dataTimingsData->NumTimingElements).Element = ctimingElementstring;
            found = state.dataTimingsData->NumTimingElements;
        }

        TSTART(Timing(found).rstartTime);
        ++Timing(found).calls;
#endif
    }

    void epStopTime(
#ifdef EP_NO_Timings
        [[maybe_unused]] std::string const &ctimingElementstring,
        [[maybe_unused]] Optional_bool_const printit, // true if it should be printed here.
        [[maybe_unused]] Optional_string_const wprint // only needed (and assumed, if printit is true)
#endif
#ifdef EP_Timings
            std::string const &ctimingElementstring,
        Optional_bool_const printit, // true if it should be printed here.
        Optional_string_const wprint // only needed (and assumed, if printit is true)
#endif
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Implement a timing scheme using start-stop (ref: epStartTime) that will help
        // developers pinpoint problems.

        // METHODOLOGY EMPLOYED:
        // structure similar to recurring error structure.

#ifdef EP_NO_Timings
        return;
#endif
#ifdef EP_Timings
        int loop;  // testing if already in structure
        int found; // indicator for element
        Real64 stoptime;
        found = 0;
        for (loop = 1; loop <= state.dataTimingsData->NumTimingElements; ++loop) {
            if (Timing(loop).Element != ctimingElementstring) continue;
            found = loop;
        }

        if (found == 0) {
            ShowFatalError(state, "epStopTime: No element=" + ctimingElementstring);
        }

        TSTOP(stoptime);
        Timing(found).currentTimeSum += (stoptime - Timing(found).rstartTime);

        if (present(printit)) {
            if (printit) {
                {
                    auto const SELECT_CASE_var(wprint);
                    if (SELECT_CASE_var == "PRINT_TIME0") {
                        fmt::print("{:80}{16.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME1") {
                        fmt::print("{:70}{16.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME2") {
                        fmt::print("{:60}{10.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME2i") {
                        fmt::print("{:56}{:4}{10.4F}\n", ctimingElementstring, Timing(found).calls, Timing(found).currentTimeSum);
                    } else if (SELECT_CASE_var == "PRINT_TIME3") {
                        fmt::print("{:50}{:10.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME3i") {
                        fmt::print("{:46}{:4}{:10.4F}\n", ctimingElementstring, Timing(found).calls, Timing(found).currentTimeSum);
                    } else if (SELECT_CASE_var == "PRINT_TIME4") {
                        fmt::print("{:40}{:10.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIMEX") {
                        fmt::print("{:100}{:16.6F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINTES") {
                        fmt::print("{:80}{:22.15}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME_AF") {
                        fmt::print("{:55}          {:16.4F}\n", ctimingElementstring, stoptime - Timing(found).rstartTime);
                    } else if (SELECT_CASE_var == "PRINT_TIME_AIF") {
                        fmt::print("{:55}{:10}{:16.4F}\n", ctimingElementstring, Timing(found).calls, Timing(found).currentTimeSum);
                    } else {
                        fmt::print("{}{}", ctimingElementstring, Timing(found).currentTimeSum);
                    }
                }
            }
            // could not cover:
            //#define PRINT_TIME_AIIF(a, i1, i2, t) write(*,'(a55,i10,i10,f16.4)') a, i1, i2, t
            //#define PRINT_TIME_AIIIF(a, i1, i2, i3, t) write(*,'(a55,i10,i10,i10,f16.55)') a, i1, i2, i3, t
        }
#endif
    }

    void epSummaryTimes(
#ifdef EP_NO_Timings
        InputOutputFile &,
        [[maybe_unused]] Real64 &TimeUsed_CPUTime
#endif
#ifdef EP_Timings
            InputOutputFile &auditFile,
        Real64 &TimeUsed_CPUTime
#endif
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Print summary of timings from timing scheme using start-stop (ref: epStartTime, epStopTime) that will help
        // developers pinpoint problems.

        // METHODOLOGY EMPLOYED:
        // structure similar to recurring error structure.

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

#ifdef EP_NO_Timings
        return;
#endif

#ifdef EP_Timings
        int loop;

        print(auditFile, "Timing Element{}# calls{}Time {{s}}{}Time {{s}} (per call)\n", tabchar, tabchar, tabchar);

        for (loop = 1; loop <= state.dataTimingsData->NumTimingElements; ++loop) {
            if (Timing(loop).calls > 0) {
                print(auditFile,
                      "{}{}{}{}{:.3R}{}{:.3R}\n",
                      Timing(loop).Element,
                      tabchar,
                      Timing(loop).calls,
                      tabchar,
                      Timing(loop).currentTimeSum,
                      tabchar,
                      Timing(loop).currentTimeSum / double(Timing(loop).calls));
            } else {
                print(auditFile,
                      "{}{}{}{}{:.3R}{}{:.3R}\n",
                      Timing(loop).Element,
                      tabchar,
                      Timing(loop).calls,
                      tabchar,
                      Timing(loop).currentTimeSum,
                      tabchar,
                      -999.0);
            }
        }
        print(auditFile, "Time from CPU_Time{}{:.3R}\n", tabchar, TimeUsed_CPUTime);
#endif
    }

    Real64 epGetTimeUsed(EnergyPlusData &state, std::string const &ctimingElementstring)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provides outside function to getting time used on a particular element

        // Return value
        Real64 totalTimeUsed;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int loop;  // testing if already in structure
        int found; // indicator for element

        found = 0;
        for (loop = 1; loop <= state.dataTimingsData->NumTimingElements; ++loop) {
            if (state.dataTimingsData->Timing(loop).Element != ctimingElementstring) continue;
            found = loop;
        }

        if (found == 0 && !state.dataErrTracking->AbortProcessing) {
            ShowFatalError(state, "epGetTimeUsed: No element=" + ctimingElementstring);
        } else {
            ShowSevereError(state, "epGetTimeUsed: No element=" + ctimingElementstring);
        }

        totalTimeUsed = state.dataTimingsData->Timing(found).currentTimeSum;

        return totalTimeUsed;
    }

    Real64 epGetTimeUsedperCall(EnergyPlusData &state, std::string const &ctimingElementstring)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provides outside function to getting time used on a particular element
        // per Call.

        // Return value
        Real64 averageTimeUsed;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int loop;  // testing if already in structure
        int found; // indicator for element

        found = 0;
        for (loop = 1; loop <= state.dataTimingsData->NumTimingElements; ++loop) {
            if (state.dataTimingsData->Timing(loop).Element != ctimingElementstring) continue;
            found = loop;
        }

        if (found == 0) {
            ShowFatalError(state, "epGetTimeUsedperCall: No element=" + ctimingElementstring);
        } else {
            ShowSevereError(state, "epGetTimeUsedperCall: No element=" + ctimingElementstring);
        }

        if (state.dataTimingsData->Timing(found).calls > 0) {
            averageTimeUsed = state.dataTimingsData->Timing(found).currentTimeSum / double(state.dataTimingsData->Timing(found).calls);
        } else {
            averageTimeUsed = -999.0;
        }

        return averageTimeUsed;
    }

    Real64 eptime(EnergyPlusData &state)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // An alternative method for timing (to CPU_TIME) is to call the standard
        // System_Clock routine.  This is a standard alternative to CPU_TIME.
        // According to Intel documentation, the "count_rate" may differ depending on
        // the size of the integer to receive the output.

        // Return value
        Real64 calctime; // calculated time based on "count" and "count_rate"

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Int32 icount;

        SYSTEM_CLOCK(icount);

        calctime = double(icount) / state.dataTimingsData->clockrate; // clockrate is set by main program.

        return calctime;
    }

    Real64 epElapsedTime()
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // An alternative method for timing elapsed times is to call the standard
        // Date_And_Time routine and set the "time".

        // Return value
        Real64 calctime; // calculated time based on hrs, minutes, seconds, milliseconds

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Array1D<Int32> clockvalues(8);
        // value(1)   Current year
        // value(2)   Current month
        // value(3)   Current day
        // value(4)   Time difference with respect to UTC in minutes (0-59)
        // value(5)   Hour of the day (0-23)
        // value(6)   Minutes (0-59)
        // value(7)   Seconds (0-59)
        // value(8)   Milliseconds (0-999)

        date_and_time(_, _, _, clockvalues);
        calctime = clockvalues(5) * 3600.0 + clockvalues(6) * 60.0 + clockvalues(7) + clockvalues(8) / 1000.0;

        return calctime;
    }

} // namespace DataTimings

} // namespace EnergyPlus
