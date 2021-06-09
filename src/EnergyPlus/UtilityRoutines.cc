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

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
}

// C++ Headers
#include <cstdlib>
#include <exception>
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataTimings.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/ExternalInterface.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputReports.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/StringUtilities.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace UtilityRoutines {

    Real64 ProcessNumber(std::string const &String, bool &ErrorFlag)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function processes a string that should be numeric and
        // returns the real value of the string.

        // METHODOLOGY EMPLOYED:
        // FUNCTION ProcessNumber translates the argument (a string)
        // into a real number.  The string should consist of all
        // numeric characters (except a decimal point).  Numerics
        // with exponentiation (i.e. 1.2345E+03) are allowed but if
        // it is not a valid number an error message along with the
        // string causing the error is printed out and 0.0 is returned
        // as the value.

        // REFERENCES:
        // List directed Fortran input/output.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const ValidNumerics("0123456789.+-EeDd");

        Real64 rProcessNumber = 0.0;
        //  Make sure the string has all what we think numerics should have
        std::string PString(stripped(String));
        std::string::size_type const StringLen(PString.length());
        ErrorFlag = false;
        if (StringLen == 0) return rProcessNumber;
        bool parseFailed = false;
        if (PString.find_first_not_of(ValidNumerics) == std::string::npos) {
            // make FORTRAN floating point number (containing 'd' or 'D')
            // standardized by replacing 'd' or 'D' with 'e'
            std::replace_if(
                std::begin(PString), std::end(PString), [](const char c) { return c == 'D' || c == 'd'; }, 'e');
            // then parse as a normal floating point value
            parseFailed = !readItem(PString, rProcessNumber);
            ErrorFlag = false;
        } else {
            rProcessNumber = 0.0;
            ErrorFlag = true;
        }
        if (parseFailed) {
            rProcessNumber = 0.0;
            ErrorFlag = true;
        }

        return rProcessNumber;
    }

    int FindItemInList(std::string const &String, Array1_string const &ListOfItems, int const NumItems)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is not case insensitive and doesn't need
        // for most inputs -- they are automatically turned to UPPERCASE.
        // If you need case insensitivity use FindItem.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (String == ListOfItems(Count)) return Count;
        }
        return 0; // Not found
    }

    int FindItemInList(std::string const &String, Array1S_string const ListOfItems, int const NumItems)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is not case insensitive and doesn't need
        // for most inputs -- they are automatically turned to UPPERCASE.
        // If you need case insensitivity use FindItem.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (String == ListOfItems(Count)) return Count;
        }
        return 0; // Not found
    }

    int FindItemInSortedList(std::string const &String, Array1S_string const ListOfItems, int const NumItems)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int Probe(0);
        int LBnd(0);
        int UBnd(NumItems + 1);
        bool Found(false);
        while ((!Found) || (Probe != 0)) {
            Probe = (UBnd - LBnd) / 2;
            if (Probe == 0) break;
            Probe += LBnd;
            if (equali(String, ListOfItems(Probe))) {
                Found = true;
                break;
            } else if (lessthani(String, ListOfItems(Probe))) {
                UBnd = Probe;
            } else {
                LBnd = Probe;
            }
        }
        return Probe;
    }

    int FindItem(std::string const &String, Array1D_string const &ListOfItems, int const NumItems)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int FindItem = UtilityRoutines::FindItemInList(String, ListOfItems, NumItems);
        if (FindItem != 0) return FindItem;

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (equali(String, ListOfItems(Count))) return Count;
        }
        return 0; // Not found
    }

    int FindItem(std::string const &String, Array1S_string const ListOfItems, int const NumItems)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int FindItem = UtilityRoutines::FindItemInList(String, ListOfItems, NumItems);
        if (FindItem != 0) return FindItem;

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (equali(String, ListOfItems(Count))) return Count;
        }
        return 0; // Not found
    }

    std::string MakeUPPERCase(std::string const &InputString)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This function returns the Upper Case representation of the InputString.

        // METHODOLOGY EMPLOYED:
        // Uses the Intrinsic SCAN function to scan the lowercase representation of
        // characters (DataStringGlobals) for each character in the given string.

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        std::string ResultString(InputString);

        for (std::string::size_type i = 0, e = len(InputString); i < e; ++i) {
            int const curCharVal = int(InputString[i]);
            if ((97 <= curCharVal && curCharVal <= 122) || (224 <= curCharVal && curCharVal <= 255)) { // lowercase ASCII and accented characters
                ResultString[i] = char(curCharVal - 32);
            }
        }

        return ResultString;
    }

    void VerifyName(EnergyPlusData &state,
                    std::string const &NameToVerify,
                    Array1D_string const &NamesList,
                    int const NumOfNames,
                    bool &ErrorFound,
                    bool &IsBlank,
                    std::string const &StringToDisplay)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine verifys that a new name can be added to the
        // list of names for this item (i.e., that there isn't one of that
        // name already and that this name is not blank).

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found;

        ErrorFound = false;
        if (NumOfNames > 0) {
            Found = FindItem(NameToVerify, NamesList, NumOfNames);
            if (Found != 0) {
                ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    void VerifyName(EnergyPlusData &state,
                    std::string const &NameToVerify,
                    Array1S_string const NamesList,
                    int const NumOfNames,
                    bool &ErrorFound,
                    bool &IsBlank,
                    std::string const &StringToDisplay)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine verifys that a new name can be added to the
        // list of names for this item (i.e., that there isn't one of that
        // name already and that this name is not blank).

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found;

        ErrorFound = false;
        if (NumOfNames > 0) {
            Found = FindItem(NameToVerify, NamesList, NumOfNames);
            if (Found != 0) {
                ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    bool IsNameEmpty(EnergyPlusData &state, std::string &NameToVerify, std::string const &StringToDisplay, bool &ErrorFound)
    {
        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + " Name, cannot be blank");
            ErrorFound = true;
            NameToVerify = "xxxxx";
            return true;
        }
        return false;
    }

    size_t case_insensitive_hasher::operator()(const std::string &key) const noexcept
    {
        std::string keyCopy = MakeUPPERCase(key);
        return std::hash<std::string>()(keyCopy);
    }

    bool case_insensitive_comparator::operator()(const std::string &a, const std::string &b) const noexcept
    {
        return SameString(a, b);
    }

    void appendPerfLog(EnergyPlusData &state, std::string const &colHeader, std::string const &colValue, bool finalColumn)
    // Add column to the performance log file (comma separated) which is appended to existing log.
    // The finalColumn (an optional argument) being true triggers the actual file to be written or appended.
    // J.Glazer February 2020
    {
        // the following was added for unit testing to clear the static strings
        if (colHeader == "RESET" && colValue == "RESET") {
            state.dataUtilityRoutines->appendPerfLog_headerRow = "";
            state.dataUtilityRoutines->appendPerfLog_valuesRow = "";
            return;
        }

        // accumuate the row until ready to be written to the file.
        state.dataUtilityRoutines->appendPerfLog_headerRow = state.dataUtilityRoutines->appendPerfLog_headerRow + colHeader + ",";
        state.dataUtilityRoutines->appendPerfLog_valuesRow = state.dataUtilityRoutines->appendPerfLog_valuesRow + colValue + ",";

        if (finalColumn) {
            std::fstream fsPerfLog;
            if (!FileSystem::fileExists(state.dataStrGlobals->outputPerfLogFilePath)) {
                if (state.files.outputControl.perflog) {
                    fsPerfLog.open(state.dataStrGlobals->outputPerfLogFilePath, std::fstream::out); // open file normally
                    if (!fsPerfLog) {
                        ShowFatalError(state,
                                       "appendPerfLog: Could not open file \"" + state.dataStrGlobals->outputPerfLogFilePath.string() +
                                           "\" for output (write).");
                    }
                    fsPerfLog << state.dataUtilityRoutines->appendPerfLog_headerRow << std::endl;
                    fsPerfLog << state.dataUtilityRoutines->appendPerfLog_valuesRow << std::endl;
                }
            } else {
                if (state.files.outputControl.perflog) {
                    fsPerfLog.open(state.dataStrGlobals->outputPerfLogFilePath, std::fstream::app); // append to already existing file
                    if (!fsPerfLog) {
                        ShowFatalError(state,
                                       "appendPerfLog: Could not open file \"" + state.dataStrGlobals->outputPerfLogFilePath.string() +
                                           "\" for output (append).");
                    }
                    fsPerfLog << state.dataUtilityRoutines->appendPerfLog_valuesRow << std::endl;
                }
            }
            fsPerfLog.close();
        }
    }

    bool ValidateFuelType([[maybe_unused]] EnergyPlusData &state,
                          std::string const &FuelTypeInput,
                          std::string &FuelTypeOutput,
                          bool &FuelTypeErrorsFound,
                          bool const &AllowSteamAndDistrict)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Dareum Nam
        //       DATE WRITTEN   May 2020

        // PURPOSE OF THIS FUNCTION:
        // Validates fuel types and sets output strings

        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(FuelTypeInput));

        if (SELECT_CASE_var == "ELECTRICITY") {
            FuelTypeOutput = "Electricity";

        } else if (SELECT_CASE_var == "NATURALGAS") {
            FuelTypeOutput = "NaturalGas";

        } else if (SELECT_CASE_var == "DIESEL") {
            FuelTypeOutput = "Diesel";

        } else if (SELECT_CASE_var == "GASOLINE") {
            FuelTypeOutput = "Gasoline";

        } else if (SELECT_CASE_var == "COAL") {
            FuelTypeOutput = "Coal";

        } else if (SELECT_CASE_var == "FUELOILNO1") {
            FuelTypeOutput = "FuelOilNo1";

        } else if (SELECT_CASE_var == "FUELOILNO2") {
            FuelTypeOutput = "FuelOilNo2";

        } else if (SELECT_CASE_var == "PROPANE") {
            FuelTypeOutput = "Propane";

        } else if (SELECT_CASE_var == "OTHERFUEL1") {
            FuelTypeOutput = "OtherFuel1";

        } else if (SELECT_CASE_var == "OTHERFUEL2") {
            FuelTypeOutput = "OtherFuel2";

        } else {
            if (AllowSteamAndDistrict) {
                if (SELECT_CASE_var == "STEAM") {
                    FuelTypeOutput = "Steam";
                } else if (SELECT_CASE_var == "DISTRICTHEATING") {
                    FuelTypeOutput = "DistrictHeating";
                } else if (SELECT_CASE_var == "DISTRICTCOOLING") {
                    FuelTypeOutput = "DistrictCooling";
                } else {
                    FuelTypeErrorsFound = true;
                }
            } else {
                FuelTypeErrorsFound = true;
            }
        }

        return FuelTypeErrorsFound;
    }

    bool ValidateFuelTypeWithAssignResourceTypeNum(std::string const &FuelTypeInput,
                                                   std::string &FuelTypeOutput,
                                                   DataGlobalConstants::ResourceType &FuelTypeNum,
                                                   bool &FuelTypeErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Dareum Nam
        //       DATE WRITTEN   May 2020

        // PURPOSE OF THIS FUNCTION:
        // Validates fuel types and sets output strings with DataGlobalConstants::AssignResourceTypeNum() (Boilers.cc and boilerSteam.cc)

        auto const SELECT_CASE_var(FuelTypeInput);

        if (SELECT_CASE_var == "ELECTRICITY") {
            FuelTypeOutput = "Electricity";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("ELECTRICITY");

        } else if (SELECT_CASE_var == "NATURALGAS") {
            FuelTypeOutput = "NaturalGas";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("NATURALGAS");

        } else if (SELECT_CASE_var == "DIESEL") {
            FuelTypeOutput = "Diesel";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("DIESEL");

        } else if (SELECT_CASE_var == "GASOLINE") {
            FuelTypeOutput = "Gasoline";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("GASOLINE");

        } else if (SELECT_CASE_var == "COAL") {
            FuelTypeOutput = "Coal";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("COAL");

        } else if (SELECT_CASE_var == "FUELOILNO1") {
            FuelTypeOutput = "FuelOilNo1";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("FUELOILNO1");

        } else if (SELECT_CASE_var == "FUELOILNO2") {
            FuelTypeOutput = "FuelOilNo2";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("FUELOILNO2");

        } else if (SELECT_CASE_var == "PROPANE") {
            FuelTypeOutput = "Propane";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("PROPANE");

        } else if (SELECT_CASE_var == "OTHERFUEL1") {
            FuelTypeOutput = "OtherFuel1";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("OTHERFUEL1");

        } else if (SELECT_CASE_var == "OTHERFUEL2") {
            FuelTypeOutput = "OtherFuel2";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("OTHERFUEL2");

        } else {
            FuelTypeErrorsFound = true;
        }

        return FuelTypeErrorsFound;
    }

} // namespace UtilityRoutines

int AbortEnergyPlus(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine causes the program to halt due to a fatal error.

    // METHODOLOGY EMPLOYED:
    // Puts a message on output files.
    // Closes files.
    // Stops the program.

    // Using/Aliasing
    using namespace DataSystemVariables;
    using namespace DataTimings;
    using namespace DataErrorTracking;
    using BranchInputManager::TestBranchIntegrity;
    using BranchNodeConnections::CheckNodeConnections;
    using BranchNodeConnections::TestCompSetInletOutletNodes;
    using ExternalInterface::CloseSocket;

    using NodeInputManager::CheckMarkedNodes;
    using NodeInputManager::SetupNodeVarsForReporting;
    using PlantManager::CheckPlantOnAbort;
    using SimulationManager::ReportLoopConnections;
    using SolarShading::ReportSurfaceErrors;
    using SystemReports::ReportAirLoopConnections;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    std::string NumWarnings;
    std::string NumSevere;
    std::string NumWarningsDuringWarmup;
    std::string NumSevereDuringWarmup;
    std::string NumWarningsDuringSizing;
    std::string NumSevereDuringSizing;
    int Hours;      // Elapsed Time Hour Reporting
    int Minutes;    // Elapsed Time Minute Reporting
    Real64 Seconds; // Elapsed Time Second Reporting
    bool ErrFound;
    bool TerminalError;

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->updateSQLiteSimulationRecord(true, false);
    }

    state.dataErrTracking->AbortProcessing = true;
    if (state.dataErrTracking->AskForConnectionsReport) {
        state.dataErrTracking->AskForConnectionsReport = false; // Set false here in case any further fatal errors in below processing...

        ShowMessage(state, "Fatal error -- final processing.  More error messages may appear.");
        SetupNodeVarsForReporting(state);

        ErrFound = false;
        TerminalError = false;
        TestBranchIntegrity(state, ErrFound);
        if (ErrFound) TerminalError = true;
        TestAirPathIntegrity(state, ErrFound);
        if (ErrFound) TerminalError = true;
        CheckMarkedNodes(state, ErrFound);
        if (ErrFound) TerminalError = true;
        CheckNodeConnections(state, ErrFound);
        if (ErrFound) TerminalError = true;
        TestCompSetInletOutletNodes(state, ErrFound);
        if (ErrFound) TerminalError = true;

        if (!TerminalError) {
            ReportAirLoopConnections(state);
            ReportLoopConnections(state);
        }

    } else if (!state.dataErrTracking->ExitDuringSimulations) {
        ShowMessage(state, "Warning:  Node connection errors not checked - most system input has not been read (see previous warning).");
        ShowMessage(state, "Fatal error -- final processing.  Program exited before simulations began.  See previous error messages.");
    }

    if (state.dataErrTracking->AskForSurfacesReport) {
        ReportSurfaces(state);
    }

    ReportSurfaceErrors(state);
    CheckPlantOnAbort(state);
    ShowRecurringErrors(state);
    SummarizeErrors(state);
    CloseMiscOpenFiles(state);
    NumWarnings = fmt::to_string(state.dataErrTracking->TotalWarningErrors);
    NumSevere = fmt::to_string(state.dataErrTracking->TotalSevereErrors);
    NumWarningsDuringWarmup = fmt::to_string(state.dataErrTracking->TotalWarningErrorsDuringWarmup);
    NumSevereDuringWarmup = fmt::to_string(state.dataErrTracking->TotalSevereErrorsDuringWarmup);
    NumWarningsDuringSizing = fmt::to_string(state.dataErrTracking->TotalWarningErrorsDuringSizing);
    NumSevereDuringSizing = fmt::to_string(state.dataErrTracking->TotalSevereErrorsDuringSizing);

    // catch up with timings if in middle
    state.dataSysVars->Time_Finish = epElapsedTime();
    if (state.dataSysVars->Time_Finish < state.dataSysVars->Time_Start) state.dataSysVars->Time_Finish += 24.0 * 3600.0;
    state.dataSysVars->Elapsed_Time = state.dataSysVars->Time_Finish - state.dataSysVars->Time_Start;
#ifdef EP_Detailed_Timings
    epStopTime("EntireRun=");
#endif
    if (state.dataSysVars->Elapsed_Time < 0.0) state.dataSysVars->Elapsed_Time = 0.0;
    Hours = state.dataSysVars->Elapsed_Time / 3600.0;
    state.dataSysVars->Elapsed_Time -= Hours * 3600.0;
    Minutes = state.dataSysVars->Elapsed_Time / 60.0;
    state.dataSysVars->Elapsed_Time -= Minutes * 60.0;
    Seconds = state.dataSysVars->Elapsed_Time;
    if (Seconds < 0.0) Seconds = 0.0;
    const auto Elapsed = format("{:02}hr {:02}min {:5.2F}sec", Hours, Minutes, Seconds);

    state.dataResultsFramework->resultsFramework->SimulationInformation.setRunTime(Elapsed);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsWarmup(NumWarningsDuringWarmup, NumSevereDuringWarmup);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSizing(NumWarningsDuringSizing, NumSevereDuringSizing);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSummary(NumWarnings, NumSevere);

    ShowMessage(state,
                "EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup +
                    " Severe Errors.");
    ShowMessage(state,
                "EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing +
                    " Severe Errors.");
    ShowMessage(state,
                "EnergyPlus Terminated--Fatal Error Detected. " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed);
    DisplayString(state, "EnergyPlus Run Time=" + Elapsed);

    {
        auto tempfl = state.files.endFile.try_open(state.files.outputControl.end);

        if (!tempfl.good()) {
            DisplayString(state, "AbortEnergyPlus: Could not open file " + tempfl.filePath.string() + " for output (write).");
        }
        print(
            tempfl, "EnergyPlus Terminated--Fatal Error Detected. {} Warning; {} Severe Errors; Elapsed Time={}\n", NumWarnings, NumSevere, Elapsed);
    }

    // Output detailed ZONE time series data
    SimulationManager::OpenOutputJsonFiles(state, state.files.json);

    state.dataResultsFramework->resultsFramework->writeOutputs(state);

#ifdef EP_Detailed_Timings
    epSummaryTimes(state.files.audit, Time_Finish - Time_Start);
#endif
    std::cerr << "Program terminated: "
              << "EnergyPlus Terminated--Error(s) Detected." << std::endl;
    // Close the socket used by ExternalInterface. This call also sends the flag "-1" to the ExternalInterface,
    // indicating that E+ terminated with an error.
    if (state.dataExternalInterface->NumExternalInterfaces > 0) CloseSocket(state, -1);

    if (state.dataGlobal->eplusRunningViaAPI) {
        state.files.flushAll();
    }

    return EXIT_FAILURE;
}

void CloseMiscOpenFiles(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine scans potential unit numbers and closes
    // any that are still open.

    // METHODOLOGY EMPLOYED:
    // Use INQUIRE to determine if file is open.

    // Using/Aliasing
    using DaylightingManager::CloseDFSFile;
    using DaylightingManager::CloseReportIllumMaps;

    CloseReportIllumMaps(state);
    CloseDFSFile(state);

    if (state.dataReportFlag->DebugOutput || (state.files.debug.good() && state.files.debug.position() > 0)) {
        state.files.debug.close();
    } else {
        state.files.debug.del();
    }
}

int EndEnergyPlus(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine causes the program to terminate when complete (no errors).

    // METHODOLOGY EMPLOYED:
    // Puts a message on output files.
    // Closes files.
    // Stops the program.

    using namespace DataSystemVariables;
    using namespace DataTimings;
    using namespace DataErrorTracking;
    using ExternalInterface::CloseSocket;

    using SolarShading::ReportSurfaceErrors;

    std::string NumWarnings;
    std::string NumSevere;
    std::string NumWarningsDuringWarmup;
    std::string NumSevereDuringWarmup;
    std::string NumWarningsDuringSizing;
    std::string NumSevereDuringSizing;
    int Hours;      // Elapsed Time Hour Reporting
    int Minutes;    // Elapsed Time Minute Reporting
    Real64 Seconds; // Elapsed Time Second Reporting

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->updateSQLiteSimulationRecord(true, true);
    }

    ReportSurfaceErrors(state);
    ShowRecurringErrors(state);
    SummarizeErrors(state);
    CloseMiscOpenFiles(state);
    NumWarnings = fmt::to_string(state.dataErrTracking->TotalWarningErrors);
    strip(NumWarnings);
    NumSevere = fmt::to_string(state.dataErrTracking->TotalSevereErrors);
    strip(NumSevere);
    NumWarningsDuringWarmup = fmt::to_string(state.dataErrTracking->TotalWarningErrorsDuringWarmup);
    strip(NumWarningsDuringWarmup);
    NumSevereDuringWarmup = fmt::to_string(state.dataErrTracking->TotalSevereErrorsDuringWarmup);
    strip(NumSevereDuringWarmup);
    NumWarningsDuringSizing = fmt::to_string(state.dataErrTracking->TotalWarningErrorsDuringSizing);
    strip(NumWarningsDuringSizing);
    NumSevereDuringSizing = fmt::to_string(state.dataErrTracking->TotalSevereErrorsDuringSizing);
    strip(NumSevereDuringSizing);

    state.dataSysVars->Time_Finish = epElapsedTime();
    if (state.dataSysVars->Time_Finish < state.dataSysVars->Time_Start) state.dataSysVars->Time_Finish += 24.0 * 3600.0;
    state.dataSysVars->Elapsed_Time = state.dataSysVars->Time_Finish - state.dataSysVars->Time_Start;
    if (state.dataGlobal->createPerfLog) {
        UtilityRoutines::appendPerfLog(state, "Run Time [seconds]", format("{:.2R}", state.dataSysVars->Elapsed_Time));
    }
#ifdef EP_Detailed_Timings
    epStopTime("EntireRun=");
#endif
    Hours = state.dataSysVars->Elapsed_Time / 3600.0;
    state.dataSysVars->Elapsed_Time -= Hours * 3600.0;
    Minutes = state.dataSysVars->Elapsed_Time / 60.0;
    state.dataSysVars->Elapsed_Time -= Minutes * 60.0;
    Seconds = state.dataSysVars->Elapsed_Time;
    if (Seconds < 0.0) Seconds = 0.0;
    const auto Elapsed = format("{:02}hr {:02}min {:5.2F}sec", Hours, Minutes, Seconds);

    state.dataResultsFramework->resultsFramework->SimulationInformation.setRunTime(Elapsed);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsWarmup(NumWarningsDuringWarmup, NumSevereDuringWarmup);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSizing(NumWarningsDuringSizing, NumSevereDuringSizing);
    state.dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSummary(NumWarnings, NumSevere);

    if (state.dataGlobal->createPerfLog) {
        UtilityRoutines::appendPerfLog(state, "Run Time [string]", Elapsed);
        UtilityRoutines::appendPerfLog(state, "Number of Warnings", NumWarnings);
        UtilityRoutines::appendPerfLog(state, "Number of Severe", NumSevere, true); // last item so write the perfLog file
    }
    ShowMessage(state,
                "EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup +
                    " Severe Errors.");
    ShowMessage(state,
                "EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing +
                    " Severe Errors.");
    ShowMessage(state, "EnergyPlus Completed Successfully-- " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed);
    DisplayString(state, "EnergyPlus Run Time=" + Elapsed);

    {
        auto tempfl = state.files.endFile.try_open(state.files.outputControl.end);
        if (!tempfl.good()) {
            DisplayString(state, "EndEnergyPlus: Could not open file " + tempfl.filePath.string() + " for output (write).");
        }
        print(tempfl, "EnergyPlus Completed Successfully-- {} Warning; {} Severe Errors; Elapsed Time={}\n", NumWarnings, NumSevere, Elapsed);
    }

    // Output detailed ZONE time series data
    SimulationManager::OpenOutputJsonFiles(state, state.files.json);

    state.dataResultsFramework->resultsFramework->writeOutputs(state);

#ifdef EP_Detailed_Timings
    epSummaryTimes(Time_Finish - Time_Start);
#endif
    if (state.dataGlobal->printConsoleOutput) std::cerr << "EnergyPlus Completed Successfully." << std::endl;
    // Close the ExternalInterface socket. This call also sends the flag "1" to the ExternalInterface,
    // indicating that E+ finished its simulation
    if ((state.dataExternalInterface->NumExternalInterfaces > 0) && state.dataExternalInterface->haveExternalInterfaceBCVTB) CloseSocket(state, 1);

    if (state.dataGlobal->eplusRunningViaAPI) {
        state.files.flushAll();
    }

    return EXIT_SUCCESS;
}

void ConvertCaseToUpper(std::string const &InputString, // Input string
                        std::string &OutputString       // Output string (in UpperCase)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Convert a string to upper case

    // METHODOLOGY EMPLOYED:
    // This routine is not dependant upon the ASCII
    // code.  It works by storing the upper and lower case alphabet.  It
    // scans the whole input string.  If it finds a character in the lower
    // case alphabet, it makes an appropriate substitution.

    // Using/Aliasing
    static std::string const UpperCase("ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ");
    static std::string const LowerCase("abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüý");

    OutputString = InputString;

    for (std::string::size_type A = 0; A < len(InputString); ++A) {
        std::string::size_type const B = index(LowerCase, InputString[A]);
        if (B != std::string::npos) {
            OutputString[A] = UpperCase[B];
        }
    }
}

void ConvertCaseToLower(std::string const &InputString, // Input string
                        std::string &OutputString       // Output string (in LowerCase)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Convert a string to lower case

    // METHODOLOGY EMPLOYED:
    // This routine is not dependant upon the ASCII
    // code.  It works by storing the upper and lower case alphabet.  It
    // scans the whole input string.  If it finds a character in the lower
    // case alphabet, it makes an appropriate substitution.

    // Using/Aliasing
    static std::string const UpperCase("ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ");
    static std::string const LowerCase("abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüý");

    OutputString = InputString;

    for (std::string::size_type A = 0; A < len(InputString); ++A) {
        std::string::size_type const B = index(UpperCase, InputString[A]);
        if (B != std::string::npos) {
            OutputString[A] = LowerCase[B];
        }
    }
}

std::string::size_type FindNonSpace(std::string const &String) // String to be scanned
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function finds the first non-space character in the passed string
    // and returns that position as the result to the calling program.

    // METHODOLOGY EMPLOYED:
    // Scan string for character not equal to blank.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    return String.find_first_not_of(' ');
}

bool env_var_on(std::string const &env_var_str)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Stuart G. Mentzer
    //       DATE WRITTEN   April 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Test if a boolean environment variable value is "on" (has value starting with Y or T)

    return ((!env_var_str.empty()) && is_any_of(env_var_str[0], "YyTt"));
}

void ShowFatalError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       Kyle Benne
    //                      August 2010
    //                      Added sqlite output
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts ErrorMessage with a Fatal designation on
    // designated output files.  Then, the program is aborted.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.
    // Calls AbortEnergyPlus

    using namespace DataErrorTracking;

    ShowErrorMessage(state, " **  Fatal  ** " + ErrorMessage, OutUnit1, OutUnit2);
    DisplayString(state, "**FATAL:" + ErrorMessage);

    ShowErrorMessage(state, " ...Summary of Errors that led to program termination:", OutUnit1, OutUnit2);
    ShowErrorMessage(state, format(" ..... Reference severe error count={}", state.dataErrTracking->TotalSevereErrors), OutUnit1, OutUnit2);
    ShowErrorMessage(state, " ..... Last severe error=" + state.dataErrTracking->LastSevereError, OutUnit1, OutUnit2);
    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 2, ErrorMessage, 1);
        if (state.dataSQLiteProcedures->sqlite->sqliteWithinTransaction()) state.dataSQLiteProcedures->sqlite->sqliteCommit();
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Fatal, ErrorMessage);
    }
    throw FatalError(ErrorMessage);
}

void ShowSevereError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts ErrorMessage with a Severe designation on
    // designated output files.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    using namespace DataStringGlobals;
    using namespace DataErrorTracking;
    int Loop;

    for (Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(ErrorMessage, MessageSearch[Loop])) ++state.dataErrTracking->MatchCounts(Loop);
    }

    ++state.dataErrTracking->TotalSevereErrors;
    if (state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation &&
        !state.dataErrTracking->AbortProcessing)
        ++state.dataErrTracking->TotalSevereErrorsDuringWarmup;
    if (state.dataGlobal->DoingSizing) ++state.dataErrTracking->TotalSevereErrorsDuringSizing;
    ShowErrorMessage(state, " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2);
    state.dataErrTracking->LastSevereError = ErrorMessage;

    //  Could set a variable here that gets checked at some point?

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 1, ErrorMessage, 1);
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Severe, ErrorMessage);
    }
}

void ShowSevereMessage(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts ErrorMessage with a Severe designation on
    // designated output files.
    // But does not bump the error count so can be used in conjunction with recurring
    // error calls.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    using namespace DataStringGlobals;
    using namespace DataErrorTracking;

    int Loop;

    for (Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(ErrorMessage, MessageSearch[Loop])) ++state.dataErrTracking->MatchCounts(Loop);
    }

    ShowErrorMessage(state, " ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2);
    state.dataErrTracking->LastSevereError = ErrorMessage;

    //  Could set a variable here that gets checked at some point?

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 1, ErrorMessage, 0);
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Severe, ErrorMessage);
    }
}

void ShowContinueError(EnergyPlusData &state, std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   October 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine displays a 'continued error' message on designated output files.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    ShowErrorMessage(state, " **   ~~~   ** " + Message, OutUnit1, OutUnit2);
    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->updateSQLiteErrorRecord(Message);
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Continue, Message);
    }
}

void ShowContinueErrorTimeStamp(EnergyPlusData &state, std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   February 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine displays a 'continued error' timestamp message on designated output files.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    // Using/Aliasing
    using General::CreateSysTimeIntervalString;

    std::string cEnvHeader;

    if (state.dataGlobal->WarmupFlag) {
        if (!state.dataGlobal->DoingSizing) {
            cEnvHeader = " During Warmup, Environment=";
        } else {
            cEnvHeader = " During Warmup & Sizing, Environment=";
        }
    } else {
        if (!state.dataGlobal->DoingSizing) {
            cEnvHeader = " Environment=";
        } else {
            cEnvHeader = " During Sizing, Environment=";
        }
    }

    if (len(Message) < 50) {
        const auto m = Message + cEnvHeader + state.dataEnvrn->EnvironmentName + ", at Simulation time=" + state.dataEnvrn->CurMnDy + ' ' +
                       CreateSysTimeIntervalString(state);
        ShowErrorMessage(state, " **   ~~~   ** " + m, OutUnit1, OutUnit2);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->updateSQLiteErrorRecord(m);
        }
        if (state.dataGlobal->errorCallback) {
            state.dataGlobal->errorCallback(Error::Continue, m);
        }
    } else {
        const auto m = " **   ~~~   ** " + Message;
        const auto postfix = " **   ~~~   ** " + cEnvHeader + state.dataEnvrn->EnvironmentName + ", at Simulation time=" + state.dataEnvrn->CurMnDy +
                             ' ' + CreateSysTimeIntervalString(state);
        ShowErrorMessage(state, m);
        ShowErrorMessage(state, postfix, OutUnit1, OutUnit2);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->updateSQLiteErrorRecord(m);
        }
        if (state.dataGlobal->errorCallback) {
            state.dataGlobal->errorCallback(Error::Continue, m);
            state.dataGlobal->errorCallback(Error::Continue, postfix);
        }
    }
}

void ShowMessage(EnergyPlusData &state, std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine displays a simple message on designated output files.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    if (Message.empty()) {
        ShowErrorMessage(state, " *************", OutUnit1, OutUnit2);
    } else {
        ShowErrorMessage(state, " ************* " + Message, OutUnit1, OutUnit2);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, -1, Message, 0);
        }
        if (state.dataGlobal->errorCallback) {
            state.dataGlobal->errorCallback(Error::Info, Message);
        }
    }
}

void ShowWarningError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts ErrorMessage with a Warning designation on
    // designated output files.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    using namespace DataStringGlobals;
    using namespace DataErrorTracking;
    int Loop;

    for (Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(ErrorMessage, MessageSearch[Loop])) ++state.dataErrTracking->MatchCounts(Loop);
    }

    ++state.dataErrTracking->TotalWarningErrors;
    if (state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation &&
        !state.dataErrTracking->AbortProcessing)
        ++state.dataErrTracking->TotalWarningErrorsDuringWarmup;
    if (state.dataGlobal->DoingSizing) ++state.dataErrTracking->TotalWarningErrorsDuringSizing;
    ShowErrorMessage(state, " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2);

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 0, ErrorMessage, 1);
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Warning, ErrorMessage);
    }
}

void ShowWarningMessage(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 2009

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts ErrorMessage with a Warning designation on
    // designated output files.
    // But does not bump the error count so can be used in conjunction with recurring
    // error calls.

    // METHODOLOGY EMPLOYED:
    // Calls ShowErrorMessage utility routine.

    // Using/Aliasing
    using namespace DataStringGlobals;
    using namespace DataErrorTracking;

    for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(ErrorMessage, MessageSearch[Loop])) ++state.dataErrTracking->MatchCounts(Loop);
    }

    ShowErrorMessage(state, " ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2);
    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 0, ErrorMessage, 0);
    }
    if (state.dataGlobal->errorCallback) {
        state.dataGlobal->errorCallback(Error::Warning, ErrorMessage);
    }
}

void ShowRecurringSevereErrorAtEnd(EnergyPlusData &state,
                                   std::string const &Message,         // Message automatically written to "error file" at end of simulation
                                   int &MsgIndex,                      // Recurring message index, if zero, next available index is assigned
                                   Optional<Real64 const> ReportMaxOf, // Track and report the max of the values passed to this argument
                                   Optional<Real64 const> ReportMinOf, // Track and report the min of the values passed to this argument
                                   Optional<Real64 const> ReportSumOf, // Track and report the sum of the values passed to this argument
                                   std::string const &ReportMaxUnits,  // optional char string (<=15 length) of units for max value
                                   std::string const &ReportMinUnits,  // optional char string (<=15 length) of units for min value
                                   std::string const &ReportSumUnits   // optional char string (<=15 length) of units for sum value
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine stores a recurring ErrorMessage with a Severe designation
    // for output at the end of the simulation with automatic tracking of number
    // of occurrences and optional tracking of associated min, max, and sum values

    // METHODOLOGY EMPLOYED:
    // Calls StoreRecurringErrorMessage utility routine.

    // Using/Aliasing
    using namespace DataStringGlobals;
    using namespace DataErrorTracking;

    // INTERFACE BLOCK SPECIFICATIONS
    //  Use for recurring "severe" error messages shown once at end of simulation
    //  with count of occurrences and optional max, min, sum

    for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(Message, MessageSearch[Loop])) {
            ++state.dataErrTracking->MatchCounts(Loop);
            break;
        }
    }
    bool bNewMessageFound = true;
    for (int Loop = 1; Loop <= state.dataErrTracking->NumRecurringErrors; ++Loop) {
        if (UtilityRoutines::SameString(state.dataErrTracking->RecurringErrors(Loop).Message, " ** Severe  ** " + Message)) {
            bNewMessageFound = false;
            MsgIndex = Loop;
            break;
        }
    }
    if (bNewMessageFound) {
        MsgIndex = 0;
    }

    ++state.dataErrTracking->TotalSevereErrors;
    StoreRecurringErrorMessage(
        state, " ** Severe  ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
}

void ShowRecurringWarningErrorAtEnd(EnergyPlusData &state,
                                    std::string const &Message,         // Message automatically written to "error file" at end of simulation
                                    int &MsgIndex,                      // Recurring message index, if zero, next available index is assigned
                                    Optional<Real64 const> ReportMaxOf, // Track and report the max of the values passed to this argument
                                    Optional<Real64 const> ReportMinOf, // Track and report the min of the values passed to this argument
                                    Optional<Real64 const> ReportSumOf, // Track and report the sum of the values passed to this argument
                                    std::string const &ReportMaxUnits,  // optional char string (<=15 length) of units for max value
                                    std::string const &ReportMinUnits,  // optional char string (<=15 length) of units for min value
                                    std::string const &ReportSumUnits   // optional char string (<=15 length) of units for sum value
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine stores a recurring ErrorMessage with a Warning designation
    // for output at the end of the simulation with automatic tracking of number
    // of occurrences and optional tracking of associated min, max, and sum values

    // METHODOLOGY EMPLOYED:
    // Calls StoreRecurringErrorMessage utility routine.

    // Using/Aliasing
    using namespace DataStringGlobals;
    using namespace DataErrorTracking;

    // INTERFACE BLOCK SPECIFICATIONS
    //  Use for recurring "warning" error messages shown once at end of simulation
    //  with count of occurrences and optional max, min, sum

    for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(Message, MessageSearch[Loop])) {
            ++state.dataErrTracking->MatchCounts(Loop);
            break;
        }
    }
    bool bNewMessageFound = true;
    for (int Loop = 1; Loop <= state.dataErrTracking->NumRecurringErrors; ++Loop) {
        if (UtilityRoutines::SameString(state.dataErrTracking->RecurringErrors(Loop).Message, " ** Warning ** " + Message)) {
            bNewMessageFound = false;
            MsgIndex = Loop;
            break;
        }
    }
    if (bNewMessageFound) {
        MsgIndex = 0;
    }

    ++state.dataErrTracking->TotalWarningErrors;
    StoreRecurringErrorMessage(
        state, " ** Warning ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
}

void ShowRecurringContinueErrorAtEnd(EnergyPlusData &state,
                                     std::string const &Message,         // Message automatically written to "error file" at end of simulation
                                     int &MsgIndex,                      // Recurring message index, if zero, next available index is assigned
                                     Optional<Real64 const> ReportMaxOf, // Track and report the max of the values passed to this argument
                                     Optional<Real64 const> ReportMinOf, // Track and report the min of the values passed to this argument
                                     Optional<Real64 const> ReportSumOf, // Track and report the sum of the values passed to this argument
                                     std::string const &ReportMaxUnits,  // optional char string (<=15 length) of units for max value
                                     std::string const &ReportMinUnits,  // optional char string (<=15 length) of units for min value
                                     std::string const &ReportSumUnits   // optional char string (<=15 length) of units for sum value
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine stores a recurring ErrorMessage with a continue designation
    // for output at the end of the simulation with automatic tracking of number
    // of occurrences and optional tracking of associated min, max, and sum values

    // METHODOLOGY EMPLOYED:
    // Calls StoreRecurringErrorMessage utility routine.

    // Using/Aliasing
    using namespace DataStringGlobals;
    using namespace DataErrorTracking;

    // INTERFACE BLOCK SPECIFICATIONS
    //  Use for recurring "continue" error messages shown once at end of simulation
    //  with count of occurrences and optional max, min, sum

    for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
        if (has(Message, MessageSearch[Loop])) {
            ++state.dataErrTracking->MatchCounts(Loop);
            break;
        }
    }
    bool bNewMessageFound = true;
    for (int Loop = 1; Loop <= state.dataErrTracking->NumRecurringErrors; ++Loop) {
        if (UtilityRoutines::SameString(state.dataErrTracking->RecurringErrors(Loop).Message, " **   ~~~   ** " + Message)) {
            bNewMessageFound = false;
            MsgIndex = Loop;
            break;
        }
    }
    if (bNewMessageFound) {
        MsgIndex = 0;
    }

    StoreRecurringErrorMessage(
        state, " **   ~~~   ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
}

void StoreRecurringErrorMessage(EnergyPlusData &state,
                                std::string const &ErrorMessage,         // Message automatically written to "error file" at end of simulation
                                int &ErrorMsgIndex,                      // Recurring message index, if zero, next available index is assigned
                                Optional<Real64 const> ErrorReportMaxOf, // Track and report the max of the values passed to this argument
                                Optional<Real64 const> ErrorReportMinOf, // Track and report the min of the values passed to this argument
                                Optional<Real64 const> ErrorReportSumOf, // Track and report the sum of the values passed to this argument
                                std::string const &ErrorReportMaxUnits,  // Units for "max" reporting
                                std::string const &ErrorReportMinUnits,  // Units for "min" reporting
                                std::string const &ErrorReportSumUnits   // Units for "sum" reporting
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2004
    //       MODIFIED       September 2005;LKL;Added Units

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine stores a recurring ErrorMessage with
    // for output at the end of the simulation with automatic tracking of number
    // of occurrences and optional tracking of associated min, max, and sum values

    // Using/Aliasing
    using namespace DataStringGlobals;
    using namespace DataErrorTracking;
    // If Index is zero, then assign next available index and reallocate array
    if (ErrorMsgIndex == 0) {
        state.dataErrTracking->RecurringErrors.redimension(++state.dataErrTracking->NumRecurringErrors);
        ErrorMsgIndex = state.dataErrTracking->NumRecurringErrors;
        // The message string only needs to be stored once when a new recurring message is created
        state.dataErrTracking->RecurringErrors(ErrorMsgIndex).Message = ErrorMessage;
        state.dataErrTracking->RecurringErrors(ErrorMsgIndex).Count = 1;
        if (state.dataGlobal->WarmupFlag) state.dataErrTracking->RecurringErrors(ErrorMsgIndex).WarmupCount = 1;
        if (state.dataGlobal->DoingSizing) state.dataErrTracking->RecurringErrors(ErrorMsgIndex).SizingCount = 1;

        // For max, min, and sum values, store the current value when a new recurring message is created
        if (present(ErrorReportMaxOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MaxValue = ErrorReportMaxOf;
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportMax = true;
            if (!ErrorReportMaxUnits.empty()) {
                state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MaxUnits = ErrorReportMaxUnits;
            }
        }
        if (present(ErrorReportMinOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MinValue = ErrorReportMinOf;
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportMin = true;
            if (!ErrorReportMinUnits.empty()) {
                state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MinUnits = ErrorReportMinUnits;
            }
        }
        if (present(ErrorReportSumOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).SumValue = ErrorReportSumOf;
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportSum = true;
            if (!ErrorReportSumUnits.empty()) {
                state.dataErrTracking->RecurringErrors(ErrorMsgIndex).SumUnits = ErrorReportSumUnits;
            }
        }

    } else if (ErrorMsgIndex > 0) {
        // Do stats and store
        ++state.dataErrTracking->RecurringErrors(ErrorMsgIndex).Count;
        if (state.dataGlobal->WarmupFlag) ++state.dataErrTracking->RecurringErrors(ErrorMsgIndex).WarmupCount;
        if (state.dataGlobal->DoingSizing) ++state.dataErrTracking->RecurringErrors(ErrorMsgIndex).SizingCount;

        if (present(ErrorReportMaxOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MaxValue =
                max(ErrorReportMaxOf, state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MaxValue);
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportMax = true;
        }
        if (present(ErrorReportMinOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MinValue =
                min(ErrorReportMinOf, state.dataErrTracking->RecurringErrors(ErrorMsgIndex).MinValue);
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportMin = true;
        }
        if (present(ErrorReportSumOf)) {
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).SumValue += ErrorReportSumOf;
            state.dataErrTracking->RecurringErrors(ErrorMsgIndex).ReportSum = true;
        }
    } else {
        // If ErrorMsgIndex < 0, then do nothing
    }
}

void ShowErrorMessage(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine displays the error messages on the indicated
    // file unit numbers, in addition to the "standard error output"
    // unit.

    // METHODOLOGY EMPLOYED:
    // If arguments OutUnit1 and/or OutUnit2 are present the
    // error message is written to these as well and the standard one.

    auto *err_stream = state.files.err_stream.get();

    if (state.dataUtilityRoutines->outputErrorHeader && err_stream) {
        *err_stream << "Program Version," << state.dataStrGlobals->VerStringVar << ',' << state.dataStrGlobals->IDDVerString << '\n';
        state.dataUtilityRoutines->outputErrorHeader = false;
    }

    if (!state.dataGlobal->DoingInputProcessing) {
        if (err_stream) *err_stream << "  " << ErrorMessage << '\n';
    } else {
        // CacheIPErrorFile is never opened or closed
        // so this output would just go to stdout
        // ObjexxFCL::gio::write(CacheIPErrorFile, fmtA) << ErrorMessage;
        if (state.dataGlobal->printConsoleOutput) std::cout << ErrorMessage << '\n';
    }
    if (present(OutUnit1)) {
        print(OutUnit1(), "  {}", ErrorMessage);
    }
    if (present(OutUnit2)) {
        print(OutUnit2(), "  {}", ErrorMessage);
    }
    // std::string tmp = "  " + ErrorMessage + '\n';
    // if (errorCallback) DataGlobals::errorCallback(tmp.c_str());
}

void SummarizeErrors(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   March 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine provides a summary of certain errors that might
    // otherwise get lost in the shuffle of many similar messages.

    using namespace DataErrorTracking;

    std::string::size_type StartC;
    std::string::size_type EndC;

    if (any_gt(state.dataErrTracking->MatchCounts, 0)) {
        ShowMessage(state, "");
        ShowMessage(state, "===== Final Error Summary =====");
        ShowMessage(state, "The following error categories occurred.  Consider correcting or noting.");
        for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (state.dataErrTracking->MatchCounts(Loop) > 0) {
                ShowMessage(state, Summaries[Loop]);
                std::string thisMoreDetails = MoreDetails[Loop];
                if (!thisMoreDetails.empty()) {
                    StartC = 0;
                    EndC = len(thisMoreDetails) - 1;
                    while (EndC != std::string::npos) {
                        EndC = index(thisMoreDetails.substr(StartC), "<CR");
                        ShowMessage(state, ".." + thisMoreDetails.substr(StartC, EndC));
                        if (thisMoreDetails.substr(StartC + EndC, 5) == "<CRE>") break;
                        StartC += EndC + 4;
                        EndC = len(thisMoreDetails.substr(StartC)) - 1;
                    }
                }
            }
        }
        ShowMessage(state, "");
    }
}

void ShowRecurringErrors(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   March 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine provides a summary of certain errors that might
    // otherwise get lost in the shuffle of many similar messages.

    // Using/Aliasing
    using namespace DataErrorTracking;

    using General::strip_trailing_zeros;

    static std::string const StatMessageStart(" **   ~~~   ** ");

    int Loop;
    std::string StatMessage;
    std::string MaxOut;
    std::string MinOut;
    std::string SumOut;

    if (state.dataErrTracking->NumRecurringErrors > 0) {
        ShowMessage(state, "");
        ShowMessage(state, "===== Recurring Error Summary =====");
        ShowMessage(state, "The following recurring error messages occurred.");
        for (Loop = 1; Loop <= state.dataErrTracking->NumRecurringErrors; ++Loop) {
            auto const &error(state.dataErrTracking->RecurringErrors(Loop));
            // Suppress reporting the count if it is a continue error
            if (has_prefix(error.Message, " **   ~~~   ** ")) {
                ShowMessage(state, error.Message);
                if (state.dataSQLiteProcedures->sqlite) {
                    state.dataSQLiteProcedures->sqlite->updateSQLiteErrorRecord(error.Message);
                }
                if (state.dataGlobal->errorCallback) {
                    state.dataGlobal->errorCallback(Error::Continue, error.Message);
                }
            } else {
                const auto warning = has_prefix(error.Message, " ** Warning ** ");
                const auto severe = has_prefix(error.Message, " ** Severe  ** ");

                ShowMessage(state, "");
                ShowMessage(state, error.Message);
                ShowMessage(state, format("{}  This error occurred {} total times;", StatMessageStart, error.Count));
                ShowMessage(state, format("{}  during Warmup {} times;", StatMessageStart, error.WarmupCount));
                ShowMessage(state, format("{}  during Sizing {} times.", StatMessageStart, error.SizingCount));
                if (state.dataSQLiteProcedures->sqlite) {
                    if (warning) {
                        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 0, error.Message.substr(15), error.Count);
                    } else if (severe) {
                        state.dataSQLiteProcedures->sqlite->createSQLiteErrorRecord(1, 1, error.Message.substr(15), error.Count);
                    }
                }
                if (state.dataGlobal->errorCallback) {
                    Error level = Error::Warning;
                    if (severe) {
                        level = Error::Severe;
                    }
                    state.dataGlobal->errorCallback(level, error.Message);
                    state.dataGlobal->errorCallback(Error::Continue, "");
                }
            }
            StatMessage = "";
            if (error.ReportMax) {
                MaxOut = format("{:.6R}", error.MaxValue);
                strip_trailing_zeros(MaxOut);
                StatMessage += "  Max=" + MaxOut;
                if (!error.MaxUnits.empty()) StatMessage += ' ' + error.MaxUnits;
            }
            if (error.ReportMin) {
                MinOut = format("{:.6R}", error.MinValue);
                strip_trailing_zeros(MinOut);
                StatMessage += "  Min=" + MinOut;
                if (!error.MinUnits.empty()) StatMessage += ' ' + error.MinUnits;
            }
            if (error.ReportSum) {
                SumOut = format("{:.6R}", error.SumValue);
                strip_trailing_zeros(SumOut);
                StatMessage += "  Sum=" + SumOut;
                if (!error.SumUnits.empty()) StatMessage += ' ' + error.SumUnits;
            }
            if (error.ReportMax || error.ReportMin || error.ReportSum) {
                ShowMessage(state, StatMessageStart + StatMessage);
            }
        }
        ShowMessage(state, "");
    }
}

} // namespace EnergyPlus
