// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <sys/stat.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/char.functions.hh>
// #include <ObjexxFCL/Array1.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include "IOFiles.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CommandLineInterface.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataTimings.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/ExternalInterface.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputReports.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/ResultsSchema.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/Timer.h>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace UtilityRoutines {
    bool outputErrorHeader(true);
    ObjexxFCL::gio::Fmt fmtLD("*");
    std::string appendPerfLog_headerRow("");
    std::string appendPerfLog_valuesRow("");

    void clear_state()
    {
        appendPerfLog_headerRow = "";
        appendPerfLog_valuesRow = "";
    }

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
        std::string const PString(stripped(String));
        std::string::size_type const StringLen(PString.length());
        ErrorFlag = false;
        if (StringLen == 0) return rProcessNumber;
        int IoStatus(0);
        if (PString.find_first_not_of(ValidNumerics) == std::string::npos) {
            {
                IOFlags flags;
                ObjexxFCL::gio::read(PString, fmtLD, flags) >> rProcessNumber;
                IoStatus = flags.ios();
            }
            ErrorFlag = false;
        } else {
            rProcessNumber = 0.0;
            ErrorFlag = true;
        }
        if (IoStatus != 0) {
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

    void VerifyName(std::string const &NameToVerify,
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
                ShowSevereError(StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    void VerifyName(std::string const &NameToVerify,
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
                ShowSevereError(StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    bool IsNameEmpty(std::string &NameToVerify, std::string const &StringToDisplay, bool &ErrorFound)
    {
        if (NameToVerify.empty()) {
            ShowSevereError(StringToDisplay + " Name, cannot be blank");
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

    void appendPerfLog(std::string const &colHeader, std::string const &colValue, bool finalColumn)
    // Add column to the performance log file (comma separated) which is appended to existing log.
    // The finalColumn (an optional argument) being true triggers the actual file to be written or appended.
    // J.Glazer February 2020
    {
        // the following was added for unit testing to clear the static strings
        if (colHeader == "RESET" && colValue == "RESET") {
            appendPerfLog_headerRow = "";
            appendPerfLog_valuesRow = "";
            return;
        }

        // accumuate the row until ready to be written to the file.
        appendPerfLog_headerRow = appendPerfLog_headerRow + colHeader + ",";
        appendPerfLog_valuesRow = appendPerfLog_valuesRow + colValue + ",";

        if (finalColumn) {
            std::fstream fsPerfLog;
            if (!exists(DataStringGlobals::outputPerfLogFileName)) {
                fsPerfLog.open(DataStringGlobals::outputPerfLogFileName, std::fstream::out); // open file normally
                if (!fsPerfLog.fail()) {
                    fsPerfLog << appendPerfLog_headerRow << std::endl;
                    fsPerfLog << appendPerfLog_valuesRow << std::endl;
                }
            } else {
                fsPerfLog.open(DataStringGlobals::outputPerfLogFileName, std::fstream::app); // append to already existing file
                if (!fsPerfLog.fail()) {
                    fsPerfLog << appendPerfLog_valuesRow << std::endl;
                }
            }
            fsPerfLog.close();
        }
    }

    inline bool exists(const std::string &filename)
    {
        // https://stackoverflow.com/questions/25225948/how-to-check-if-a-file-exists-in-c-with-fstreamopen/51300933
        struct stat buffer;
        return (stat(filename.c_str(), &buffer) == 0);
    }

    bool ValidateFuelType(std::string const &FuelTypeInput,
                          std::string &FuelTypeOutput,
                          bool &FuelTypeErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Dareum Nam
        //       DATE WRITTEN   May 2020

        // PURPOSE OF THIS FUNCTION:
        // Validates fuel types and sets output strings

        auto const SELECT_CASE_var(FuelTypeInput);

        if (SELECT_CASE_var == "ELECTRICITY") {
            FuelTypeOutput = "Electric";

        } else if (SELECT_CASE_var == "NATURALGAS") {
            FuelTypeOutput = "Gas";

        } else if (SELECT_CASE_var == "DIESEL") {
            FuelTypeOutput = "Diesel";

        } else if (SELECT_CASE_var == "GASOLINE") {
            FuelTypeOutput = "Gasoline";

        } else if (SELECT_CASE_var == "COAL") {
            FuelTypeOutput = "Coal";

        } else if (SELECT_CASE_var == "FUELOILNO1") {
            FuelTypeOutput = "FuelOil#1";

        } else if (SELECT_CASE_var == "FUELOILNO2") {
            FuelTypeOutput = "FuelOil#2";

        } else if (SELECT_CASE_var == "PROPANE") {
            FuelTypeOutput = "Propane";

        } else if (SELECT_CASE_var == "OTHERFUEL1") {
            FuelTypeOutput = "OtherFuel1";

        } else if (SELECT_CASE_var == "OTHERFUEL2") {
            FuelTypeOutput = "OtherFuel2";

        } else {
            FuelTypeErrorsFound = true;
        }

        return FuelTypeErrorsFound;
    }

    bool ValidateFuelTypeWithFuelTypeNum(std::string const &FuelTypeInput,
                                         int &FuelTypeNum,
                                         bool &FuelTypeErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Dareum Nam
        //       DATE WRITTEN   May 2020

        // PURPOSE OF THIS FUNCTION:
        // Validates fuel types and sets output strings with fuel type number (DXCoils.cc and HVACVariableRefrigerantFlow.cc)

        if (SameString(FuelTypeInput, "Electricity")) {
            FuelTypeNum = 1; // FuelTypeElectricity
        } else if (SameString(FuelTypeInput, "NaturalGas")) {
            FuelTypeNum = 2; // FuelTypeNaturalGas
        } else if (SameString(FuelTypeInput, "Propane")) {
            FuelTypeNum = 3; // FuelTypePropaneGas
        } else if (SameString(FuelTypeInput, "Diesel")) {
            FuelTypeNum = 4; // FuelTypeDiesel
        } else if (SameString(FuelTypeInput, "Gasoline")) {
            FuelTypeNum = 5; // FuelTypeGasoline
        } else if (SameString(FuelTypeInput, "FuelOilNo1")) {
            FuelTypeNum = 6; // FuelTypeFuelOil1
        } else if (SameString(FuelTypeInput, "FuelOilNo2")) {
            FuelTypeNum = 7; // FuelTypeFuelOil2
        } else if (SameString(FuelTypeInput, "OtherFuel1")) {
            FuelTypeNum = 8; // FuelTypeOtherFuel1
        } else if (SameString(FuelTypeInput, "OtherFuel2")) {
            FuelTypeNum = 9; // FuelTypeOtherFuel2
        } else {
            FuelTypeErrorsFound = true;
        }

        return FuelTypeErrorsFound;
    }

    bool ValidateFuelTypeWithAssignResourceTypeNum(std::string const &FuelTypeInput,
                                                   std::string &FuelTypeOutput,
                                                   int &FuelTypeNum,
                                                   bool &FuelTypeErrorsFound)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Dareum Nam
        //       DATE WRITTEN   May 2020

        // PURPOSE OF THIS FUNCTION:
        // Validates fuel types and sets output strings with DataGlobalConstants::AssignResourceTypeNum() (Boilers.cc and boilerSteam.cc)

        auto const SELECT_CASE_var(FuelTypeInput);

        if (SELECT_CASE_var == "ELECTRICITY") {
            FuelTypeOutput = "Electric";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("ELECTRICITY");

        } else if (SELECT_CASE_var == "NATURALGAS") {
            FuelTypeOutput = "Gas";
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
            FuelTypeOutput = "FuelOil#1";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("FUELOIL#1");

        } else if (SELECT_CASE_var == "FUELOILNO2") {
            FuelTypeOutput = "FuelOil#2";
            FuelTypeNum = DataGlobalConstants::AssignResourceTypeNum("FUELOIL#2");

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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataPrecisionGlobals;
        using namespace DataSystemVariables;
        using namespace DataTimings;
        using namespace DataErrorTracking;
        using BranchInputManager::TestBranchIntegrity;
        using BranchNodeConnections::CheckNodeConnections;
        using BranchNodeConnections::TestCompSetInletOutletNodes;
        using ExternalInterface::CloseSocket;
        using ExternalInterface::NumExternalInterfaces;
        using General::RoundSigDigits;
        using NodeInputManager::CheckMarkedNodes;
        using NodeInputManager::SetupNodeVarsForReporting;
        using PlantManager::CheckPlantOnAbort;
        using SimulationManager::ReportLoopConnections;
        using SolarShading::ReportSurfaceErrors;
        using SystemReports::ReportAirLoopConnections;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt OutFmt("('Press ENTER to continue after reading above message>')");

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

        if (sqlite) {
            sqlite->updateSQLiteSimulationRecord(true, false);
        }

        AbortProcessing = true;
        if (AskForConnectionsReport) {
            AskForConnectionsReport = false; // Set false here in case any further fatal errors in below processing...

            ShowMessage("Fatal error -- final processing.  More error messages may appear.");
            SetupNodeVarsForReporting(state.files);

            ErrFound = false;
            TerminalError = false;
            TestBranchIntegrity(state.dataBranchInputManager, state.files, ErrFound);
            if (ErrFound) TerminalError = true;
            TestAirPathIntegrity(state, state.files, ErrFound);
            if (ErrFound) TerminalError = true;
            CheckMarkedNodes(ErrFound);
            if (ErrFound) TerminalError = true;
            CheckNodeConnections(ErrFound);
            if (ErrFound) TerminalError = true;
            TestCompSetInletOutletNodes(ErrFound);
            if (ErrFound) TerminalError = true;

            if (!TerminalError) {
                ReportAirLoopConnections(state.files);
                ReportLoopConnections(state.files);
            }

        } else if (!ExitDuringSimulations) {
            ShowMessage("Warning:  Node connection errors not checked - most system input has not been read (see previous warning).");
            ShowMessage("Fatal error -- final processing.  Program exited before simulations began.  See previous error messages.");
        }

        if (AskForSurfacesReport) {
            ReportSurfaces(state.files);
        }

        ReportSurfaceErrors();
        CheckPlantOnAbort();
        ShowRecurringErrors();
        SummarizeErrors();
        CloseMiscOpenFiles(state.files);
        NumWarnings = fmt::to_string(TotalWarningErrors);
        NumSevere = fmt::to_string(TotalSevereErrors);
        NumWarningsDuringWarmup = fmt::to_string(TotalWarningErrorsDuringWarmup);
        NumSevereDuringWarmup = fmt::to_string(TotalSevereErrorsDuringWarmup);
        NumWarningsDuringSizing = fmt::to_string(TotalWarningErrorsDuringSizing);
        NumSevereDuringSizing = fmt::to_string(TotalSevereErrorsDuringSizing);

        // catch up with timings if in middle
        Time_Finish = epElapsedTime();
        if (Time_Finish < Time_Start) Time_Finish += 24.0 * 3600.0;
        Elapsed_Time = Time_Finish - Time_Start;
#ifdef EP_Detailed_Timings
        epStopTime("EntireRun=");
#endif
        if (Elapsed_Time < 0.0) Elapsed_Time = 0.0;
        Hours = Elapsed_Time / 3600.0;
        Elapsed_Time -= Hours * 3600.0;
        Minutes = Elapsed_Time / 60.0;
        Elapsed_Time -= Minutes * 60.0;
        Seconds = Elapsed_Time;
        if (Seconds < 0.0) Seconds = 0.0;
        const auto Elapsed = format("{:02}hr {:02}min {:5.2F}sec", Hours, Minutes, Seconds);

        ResultsFramework::OutputSchema->SimulationInformation.setRunTime(Elapsed);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsWarmup(NumWarningsDuringWarmup, NumSevereDuringWarmup);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsSizing(NumWarningsDuringSizing, NumSevereDuringSizing);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsSummary(NumWarnings, NumSevere);

        ShowMessage("EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup +
                    " Severe Errors.");
        ShowMessage("EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing +
                    " Severe Errors.");
        ShowMessage("EnergyPlus Terminated--Fatal Error Detected. " + NumWarnings + " Warning; " + NumSevere +
                    " Severe Errors; Elapsed Time=" + Elapsed);
        DisplayString("EnergyPlus Run Time=" + Elapsed);

        {
            auto tempfl = state.files.endFile.try_open();

            if (!tempfl.good()) {
                DisplayString("AbortEnergyPlus: Could not open file " + tempfl.fileName + " for output (write).");
            }
            print(tempfl,
                  "EnergyPlus Terminated--Fatal Error Detected. {} Warning; {} Severe Errors; Elapsed Time={}\n",
                  NumWarnings,
                  NumSevere,
                  Elapsed);
        }

        // Output detailed ZONE time series data
        SimulationManager::OpenOutputJsonFiles(state.files.json);

        if (ResultsFramework::OutputSchema->timeSeriesEnabled()) {
            ResultsFramework::OutputSchema->writeTimeSeriesReports(state.files.json);
        }

        if (ResultsFramework::OutputSchema->timeSeriesAndTabularEnabled()) {
            ResultsFramework::OutputSchema->WriteReport(state.files.json);
        }

#ifdef EP_Detailed_Timings
        epSummaryTimes(state.files.audit, Time_Finish - Time_Start);
#endif
        std::cerr << "Program terminated: "
                  << "EnergyPlus Terminated--Error(s) Detected." << std::endl;
        CloseOutOpenFiles();
        // Close the socket used by ExternalInterface. This call also sends the flag "-1" to the ExternalInterface,
        // indicating that E+ terminated with an error.
        if (NumExternalInterfaces > 0) CloseSocket(-1);
        return EXIT_FAILURE;
    }

    void CloseMiscOpenFiles(IOFiles &ioFiles)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataReportingFlags::DebugOutput;
        using DaylightingManager::CloseDFSFile;
        using DaylightingManager::CloseReportIllumMaps;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        //      LOGICAL :: exists, opened
        //      INTEGER :: UnitNumber
        //      INTEGER :: ios

        CloseReportIllumMaps(ioFiles);
        CloseDFSFile(ioFiles);

        if (DebugOutput || ioFiles.debug.position() > 0) {
            ioFiles.debug.close();
        } else {
            ioFiles.debug.del();
        }
    }

    void CloseOutOpenFiles()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine scans potential unit numbers and closes
        // any that are still open.

        // METHODOLOGY EMPLOYED:
        // Use INQUIRE to determine if file is open.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxUnitNumber(1000);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        bool exists;
        bool opened;
        std::string name;
        const std::string stdin_name("stdin");
        const std::string stdout_name("stdout");
        const std::string stderr_name("stderr");
        bool not_special(false);
        int UnitNumber;
        int ios;

        // TODO: This function is only used for eplusout.err
        // once that file is moved into the state object this function
        // can be removed. Then we can remove gio::inquire
        for (UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber) {
            {
                IOFlags flags;
                ObjexxFCL::gio::inquire(UnitNumber, flags);
                exists = flags.exists();
                opened = flags.open();
                ios = flags.ios();
                name = flags.name();
            }
            if (exists && opened && ios == 0) {
                not_special = name.compare(stdin_name) != 0;
                not_special = not_special && (name.compare(stdout_name) != 0);
                not_special = not_special && (name.compare(stderr_name) != 0);
                if (not_special) {
                    ObjexxFCL::gio::close(UnitNumber);
                }
            }
        }
    }

    int EndEnergyPlus(IOFiles &ioFiles)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataPrecisionGlobals;
        using namespace DataSystemVariables;
        using namespace DataTimings;
        using namespace DataErrorTracking;
        using ExternalInterface::CloseSocket;
        using ExternalInterface::haveExternalInterfaceBCVTB;
        using ExternalInterface::NumExternalInterfaces;
        using General::RoundSigDigits;
        using SolarShading::ReportSurfaceErrors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string NumWarnings;
        std::string NumSevere;
        std::string NumWarningsDuringWarmup;
        std::string NumSevereDuringWarmup;
        std::string NumWarningsDuringSizing;
        std::string NumSevereDuringSizing;
        int Hours;      // Elapsed Time Hour Reporting
        int Minutes;    // Elapsed Time Minute Reporting
        Real64 Seconds; // Elapsed Time Second Reporting

        if (sqlite) {
            sqlite->updateSQLiteSimulationRecord(true, true);
        }

        ReportSurfaceErrors();
        ShowRecurringErrors();
        SummarizeErrors();
        CloseMiscOpenFiles(ioFiles);
        NumWarnings = RoundSigDigits(TotalWarningErrors);
        strip(NumWarnings);
        NumSevere = RoundSigDigits(TotalSevereErrors);
        strip(NumSevere);
        NumWarningsDuringWarmup = RoundSigDigits(TotalWarningErrorsDuringWarmup);
        strip(NumWarningsDuringWarmup);
        NumSevereDuringWarmup = RoundSigDigits(TotalSevereErrorsDuringWarmup);
        strip(NumSevereDuringWarmup);
        NumWarningsDuringSizing = RoundSigDigits(TotalWarningErrorsDuringSizing);
        strip(NumWarningsDuringSizing);
        NumSevereDuringSizing = RoundSigDigits(TotalSevereErrorsDuringSizing);
        strip(NumSevereDuringSizing);

        Time_Finish = epElapsedTime();
        if (Time_Finish < Time_Start) Time_Finish += 24.0 * 3600.0;
        Elapsed_Time = Time_Finish - Time_Start;
        if (DataGlobals::createPerfLog) {
            UtilityRoutines::appendPerfLog("Run Time [seconds]", RoundSigDigits(Elapsed_Time, 2));
        }
#ifdef EP_Detailed_Timings
        epStopTime("EntireRun=");
#endif
        Hours = Elapsed_Time / 3600.0;
        Elapsed_Time -= Hours * 3600.0;
        Minutes = Elapsed_Time / 60.0;
        Elapsed_Time -= Minutes * 60.0;
        Seconds = Elapsed_Time;
        if (Seconds < 0.0) Seconds = 0.0;
        const auto Elapsed = format("{:02}hr {:02}min {:5.2F}sec", Hours, Minutes, Seconds);

        ResultsFramework::OutputSchema->SimulationInformation.setRunTime(Elapsed);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsWarmup(NumWarningsDuringWarmup, NumSevereDuringWarmup);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsSizing(NumWarningsDuringSizing, NumSevereDuringSizing);
        ResultsFramework::OutputSchema->SimulationInformation.setNumErrorsSummary(NumWarnings, NumSevere);

        if (DataGlobals::createPerfLog) {
            UtilityRoutines::appendPerfLog("Run Time [string]", Elapsed);
            UtilityRoutines::appendPerfLog("Number of Warnings", NumWarnings);
            UtilityRoutines::appendPerfLog("Number of Severe", NumSevere, true); // last item so write the perfLog file
        }
        ShowMessage("EnergyPlus Warmup Error Summary. During Warmup: " + NumWarningsDuringWarmup + " Warning; " + NumSevereDuringWarmup +
                    " Severe Errors.");
        ShowMessage("EnergyPlus Sizing Error Summary. During Sizing: " + NumWarningsDuringSizing + " Warning; " + NumSevereDuringSizing +
                    " Severe Errors.");
        ShowMessage("EnergyPlus Completed Successfully-- " + NumWarnings + " Warning; " + NumSevere + " Severe Errors; Elapsed Time=" + Elapsed);
        DisplayString("EnergyPlus Run Time=" + Elapsed);

        {
            auto tempfl = ioFiles.endFile.try_open();
            if (!tempfl.good()) {
                DisplayString("EndEnergyPlus: Could not open file " + tempfl.fileName + " for output (write).");
            }
            print(tempfl, "EnergyPlus Completed Successfully-- {} Warning; {} Severe Errors; Elapsed Time={}\n", NumWarnings, NumSevere, Elapsed);
        }

        // Output detailed ZONE time series data
        SimulationManager::OpenOutputJsonFiles(ioFiles.json);

        if (ResultsFramework::OutputSchema->timeSeriesEnabled()) {
            ResultsFramework::OutputSchema->writeTimeSeriesReports(ioFiles.json);
        }

        if (ResultsFramework::OutputSchema->timeSeriesAndTabularEnabled()) {
            ResultsFramework::OutputSchema->WriteReport(ioFiles.json);
        }

#ifdef EP_Detailed_Timings
        epSummaryTimes(Time_Finish - Time_Start);
#endif
        std::cerr << "EnergyPlus Completed Successfully." << std::endl;
        CloseOutOpenFiles();
        // Close the ExternalInterface socket. This call also sends the flag "1" to the ExternalInterface,
        // indicating that E+ finished its simulation
        if ((NumExternalInterfaces > 0) && haveExternalInterfaceBCVTB) CloseSocket(1);
        return EXIT_SUCCESS;
    }

    int GetNewUnitNumber()
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie, adapted from reference
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Returns a unit number of a unit that can exist and is not connected.  Note
        // this routine does not magically mark that unit number in use.  In order to
        // have the unit "used", the source code must OPEN the file.

        // METHODOLOGY EMPLOYED:
        // Use Inquire function to find out if proposed unit: exists or is opened.
        // If not, can be used for a new unit number.

        // REFERENCES:
        // Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
        // Developed at Unicomp, Inc.
        // Permission to use, copy, modify, and distribute this
        // software is freely granted, provided that this notice
        // is preserved.

        // USE STATEMENTS:
        // na

        //	// Return value
        //	int UnitNumber; // Result from scanning currently open files
        //
        //	// Locals
        //	// FUNCTION ARGUMENT DEFINITIONS:
        //
        //	// FUNCTION PARAMETER DEFINITIONS:
        //	//  IO Status Values:
        //
        //	int const END_OF_RECORD( -2 );
        //	int const END_OF_FILE( -1 );
        //
        //	//  Indicate default input and output units:
        //
        //	int const DEFAULT_INPUT_UNIT( 5 );
        //	int const DEFAULT_OUTPUT_UNIT( 6 );
        //
        //	//  Indicate number and value of preconnected units
        //
        //	int const NUMBER_OF_PRECONNECTED_UNITS( 2 );
        //	static Array1D_int const PRECONNECTED_UNITS( NUMBER_OF_PRECONNECTED_UNITS, { 5, 6 } );
        //
        //	//  Largest allowed unit number (or a large number, if none)
        //	int const MaxUnitNumber( 1000 );
        //
        //	// INTERFACE BLOCK SPECIFICATIONS
        //	// na
        //
        //	// DERIVED TYPE DEFINITIONS
        //	// na
        //
        //	// FUNCTION LOCAL VARIABLE DECLARATIONS:
        //	bool exists; // File exists
        //	bool opened; // Unit is open
        //	int ios; // return value from Inquire intrinsic
        //
        //	for ( UnitNumber = 1; UnitNumber <= MaxUnitNumber; ++UnitNumber ) {
        //		if ( UnitNumber == DEFAULT_INPUT_UNIT || UnitNumber == DEFAULT_OUTPUT_UNIT ) continue;
        //		if ( any_eq( UnitNumber, PRECONNECTED_UNITS ) ) continue;
        //		{ IOFlags flags; ObjexxFCL::gio::inquire( UnitNumber, flags ); exists = flags.exists(); opened = flags.open(); ios =
        //flags.ios(); } 		if ( exists && ! opened && ios == 0 ) return UnitNumber; // result is set in UnitNumber
        //	}
        //
        //	UnitNumber = -1;
        //
        //	return UnitNumber;

        return ObjexxFCL::gio::get_unit(); // Autodesk:Note ObjexxFCL::gio system provides this (and protects the F90+ preconnected units
                                           // {100,101,102})
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

    void ShowFatalError(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataErrorTracking;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        ShowErrorMessage(" **  Fatal  ** " + ErrorMessage, OutUnit1, OutUnit2);
        DisplayString("**FATAL:" + ErrorMessage);

        ShowErrorMessage(" ...Summary of Errors that led to program termination:", OutUnit1, OutUnit2);
        ShowErrorMessage(" ..... Reference severe error count=" + RoundSigDigits(TotalSevereErrors), OutUnit1, OutUnit2);
        ShowErrorMessage(" ..... Last severe error=" + LastSevereError, OutUnit1, OutUnit2);
        if (sqlite) {
            sqlite->createSQLiteErrorRecord(1, 2, ErrorMessage, 1);
            if (sqlite->sqliteWithinTransaction()) sqlite->sqliteCommit();
        }
        throw FatalError(ErrorMessage);
    }

    void ShowSevereError(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;
        using DataGlobals::DoingSizing;
        using DataGlobals::KickOffSimulation;
        using DataGlobals::WarmupFlag;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;

        for (Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(ErrorMessage, MessageSearch(Loop))) ++MatchCounts(Loop);
        }

        ++TotalSevereErrors;
        if (WarmupFlag && !DoingSizing && !KickOffSimulation && !AbortProcessing) ++TotalSevereErrorsDuringWarmup;
        if (DoingSizing) ++TotalSevereErrorsDuringSizing;
        ShowErrorMessage(" ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2);
        LastSevereError = ErrorMessage;

        //  Could set a variable here that gets checked at some point?

        if (sqlite) {
            sqlite->createSQLiteErrorRecord(1, 1, ErrorMessage, 1);
        }
    }

    void ShowSevereMessage(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;

        for (Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(ErrorMessage, MessageSearch(Loop))) ++MatchCounts(Loop);
        }

        ShowErrorMessage(" ** Severe  ** " + ErrorMessage, OutUnit1, OutUnit2);
        LastSevereError = ErrorMessage;

        //  Could set a variable here that gets checked at some point?

        if (sqlite) {
            sqlite->createSQLiteErrorRecord(1, 1, ErrorMessage, 0);
        }
    }

    void ShowContinueError(std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        ShowErrorMessage(" **   ~~~   ** " + Message, OutUnit1, OutUnit2);
        if (sqlite) {
            sqlite->updateSQLiteErrorRecord(Message);
        }
    }

    void ShowContinueErrorTimeStamp(std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataEnvironment::CurMnDy;
        using DataEnvironment::EnvironmentName;
        using DataGlobals::DoingSizing;
        using DataGlobals::WarmupFlag;
        using General::CreateSysTimeIntervalString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string cEnvHeader;

        if (WarmupFlag) {
            if (!DoingSizing) {
                cEnvHeader = " During Warmup, Environment=";
            } else {
                cEnvHeader = " During Warmup & Sizing, Environment=";
            }
        } else {
            if (!DoingSizing) {
                cEnvHeader = " Environment=";
            } else {
                cEnvHeader = " During Sizing, Environment=";
            }
        }

        if (len(Message) < 50) {
            ShowErrorMessage(" **   ~~~   ** " + Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' +
                                 CreateSysTimeIntervalString(),
                             OutUnit1,
                             OutUnit2);
            if (sqlite) {
                sqlite->updateSQLiteErrorRecord(Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' +
                                                CreateSysTimeIntervalString());
            }
        } else {
            ShowErrorMessage(" **   ~~~   ** " + Message);
            ShowErrorMessage(" **   ~~~   ** " + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' +
                                 CreateSysTimeIntervalString(),
                             OutUnit1,
                             OutUnit2);
            if (sqlite) {
                sqlite->updateSQLiteErrorRecord(Message + cEnvHeader + EnvironmentName + ", at Simulation time=" + CurMnDy + ' ' +
                                                CreateSysTimeIntervalString());
            }
        }
    }

    void ShowMessage(std::string const &Message, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (Message.empty()) {
            ShowErrorMessage(" *************", OutUnit1, OutUnit2);
        } else {
            ShowErrorMessage(" ************* " + Message, OutUnit1, OutUnit2);
            if (sqlite) {
                sqlite->createSQLiteErrorRecord(1, -1, Message, 0);
            }
        }
    }

    void ShowWarningError(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;
        using DataGlobals::DoingSizing;
        using DataGlobals::KickOffSimulation;
        using DataGlobals::WarmupFlag;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;

        for (Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(ErrorMessage, MessageSearch(Loop))) ++MatchCounts(Loop);
        }

        ++TotalWarningErrors;
        if (WarmupFlag && !DoingSizing && !KickOffSimulation && !AbortProcessing) ++TotalWarningErrorsDuringWarmup;
        if (DoingSizing) ++TotalWarningErrorsDuringSizing;
        ShowErrorMessage(" ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2);

        if (sqlite) {
            sqlite->createSQLiteErrorRecord(1, 0, ErrorMessage, 1);
        }
    }

    void ShowWarningMessage(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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
            if (has(ErrorMessage, MessageSearch(Loop))) ++MatchCounts(Loop);
        }

        ShowErrorMessage(" ** Warning ** " + ErrorMessage, OutUnit1, OutUnit2);
        if (sqlite) {
            sqlite->createSQLiteErrorRecord(1, 0, ErrorMessage, 0);
        }
    }

    void ShowRecurringSevereErrorAtEnd(std::string const &Message,         // Message automatically written to "error file" at end of simulation
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
        using namespace DataPrecisionGlobals;
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;

        // INTERFACE BLOCK SPECIFICATIONS
        //  Use for recurring "severe" error messages shown once at end of simulation
        //  with count of occurrences and optional max, min, sum

        for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(Message, MessageSearch(Loop))) {
                ++MatchCounts(Loop);
                break;
            }
        }
        bool bNewMessageFound = true;
        for (int Loop = 1; Loop <= NumRecurringErrors; ++Loop) {
            if (UtilityRoutines::SameString(RecurringErrors(Loop).Message, " ** Severe  ** " + Message)) {
                bNewMessageFound = false;
                MsgIndex = Loop;
                break;
            }
        }
        if (bNewMessageFound) {
            MsgIndex = 0;
        }

        ++TotalSevereErrors;
        StoreRecurringErrorMessage(
            " ** Severe  ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
    }

    void ShowRecurringWarningErrorAtEnd(std::string const &Message,         // Message automatically written to "error file" at end of simulation
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
        using namespace DataPrecisionGlobals;
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;

        // INTERFACE BLOCK SPECIFICATIONS
        //  Use for recurring "warning" error messages shown once at end of simulation
        //  with count of occurrences and optional max, min, sum

        for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(Message, MessageSearch(Loop))) {
                ++MatchCounts(Loop);
                break;
            }
        }
        bool bNewMessageFound = true;
        for (int Loop = 1; Loop <= NumRecurringErrors; ++Loop) {
            if (UtilityRoutines::SameString(RecurringErrors(Loop).Message, " ** Warning ** " + Message)) {
                bNewMessageFound = false;
                MsgIndex = Loop;
                break;
            }
        }
        if (bNewMessageFound) {
            MsgIndex = 0;
        }

        ++TotalWarningErrors;
        StoreRecurringErrorMessage(
            " ** Warning ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
    }

    void ShowRecurringContinueErrorAtEnd(std::string const &Message,         // Message automatically written to "error file" at end of simulation
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
        using namespace DataPrecisionGlobals;
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;

        // INTERFACE BLOCK SPECIFICATIONS
        //  Use for recurring "continue" error messages shown once at end of simulation
        //  with count of occurrences and optional max, min, sum

        for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
            if (has(Message, MessageSearch(Loop))) {
                ++MatchCounts(Loop);
                break;
            }
        }
        bool bNewMessageFound = true;
        for (int Loop = 1; Loop <= NumRecurringErrors; ++Loop) {
            if (UtilityRoutines::SameString(RecurringErrors(Loop).Message, " **   ~~~   ** " + Message)) {
                bNewMessageFound = false;
                MsgIndex = Loop;
                break;
            }
        }
        if (bNewMessageFound) {
            MsgIndex = 0;
        }

        StoreRecurringErrorMessage(
            " **   ~~~   ** " + Message, MsgIndex, ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits);
    }

    void StoreRecurringErrorMessage(std::string const &ErrorMessage,         // Message automatically written to "error file" at end of simulation
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
        using namespace DataPrecisionGlobals;
        using namespace DataStringGlobals;
        using namespace DataErrorTracking;
        using DataGlobals::DoingSizing;
        using DataGlobals::WarmupFlag;

        // If Index is zero, then assign next available index and reallocate array
        if (ErrorMsgIndex == 0) {
            RecurringErrors.redimension(++NumRecurringErrors);
            ErrorMsgIndex = NumRecurringErrors;
            // The message string only needs to be stored once when a new recurring message is created
            RecurringErrors(ErrorMsgIndex).Message = ErrorMessage;
            RecurringErrors(ErrorMsgIndex).Count = 1;
            if (WarmupFlag) RecurringErrors(ErrorMsgIndex).WarmupCount = 1;
            if (DoingSizing) RecurringErrors(ErrorMsgIndex).SizingCount = 1;

            // For max, min, and sum values, store the current value when a new recurring message is created
            if (present(ErrorReportMaxOf)) {
                RecurringErrors(ErrorMsgIndex).MaxValue = ErrorReportMaxOf;
                RecurringErrors(ErrorMsgIndex).ReportMax = true;
                if (!ErrorReportMaxUnits.empty()) {
                    RecurringErrors(ErrorMsgIndex).MaxUnits = ErrorReportMaxUnits;
                }
            }
            if (present(ErrorReportMinOf)) {
                RecurringErrors(ErrorMsgIndex).MinValue = ErrorReportMinOf;
                RecurringErrors(ErrorMsgIndex).ReportMin = true;
                if (!ErrorReportMinUnits.empty()) {
                    RecurringErrors(ErrorMsgIndex).MinUnits = ErrorReportMinUnits;
                }
            }
            if (present(ErrorReportSumOf)) {
                RecurringErrors(ErrorMsgIndex).SumValue = ErrorReportSumOf;
                RecurringErrors(ErrorMsgIndex).ReportSum = true;
                if (!ErrorReportSumUnits.empty()) {
                    RecurringErrors(ErrorMsgIndex).SumUnits = ErrorReportSumUnits;
                }
            }

        } else if (ErrorMsgIndex > 0) {
            // Do stats and store
            ++RecurringErrors(ErrorMsgIndex).Count;
            if (WarmupFlag) ++RecurringErrors(ErrorMsgIndex).WarmupCount;
            if (DoingSizing) ++RecurringErrors(ErrorMsgIndex).SizingCount;

            if (present(ErrorReportMaxOf)) {
                RecurringErrors(ErrorMsgIndex).MaxValue = max(ErrorReportMaxOf, RecurringErrors(ErrorMsgIndex).MaxValue);
                RecurringErrors(ErrorMsgIndex).ReportMax = true;
            }
            if (present(ErrorReportMinOf)) {
                RecurringErrors(ErrorMsgIndex).MinValue = min(ErrorReportMinOf, RecurringErrors(ErrorMsgIndex).MinValue);
                RecurringErrors(ErrorMsgIndex).ReportMin = true;
            }
            if (present(ErrorReportSumOf)) {
                RecurringErrors(ErrorMsgIndex).SumValue += ErrorReportSumOf;
                RecurringErrors(ErrorMsgIndex).ReportSum = true;
            }
        } else {
            // If ErrorMsgIndex < 0, then do nothing
        }
    }

    void ShowErrorMessage(std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1, OptionalOutputFileRef OutUnit2)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::DoingInputProcessing;
        using DataGlobals::err_stream;
        using DataStringGlobals::IDDVerString;
        using DataStringGlobals::VerString;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (UtilityRoutines::outputErrorHeader && err_stream) {
            *err_stream << "Program Version," + VerString + ',' + IDDVerString + DataStringGlobals::NL;
            UtilityRoutines::outputErrorHeader = false;
        }

        if (!DoingInputProcessing) {
            if (err_stream) *err_stream << "  " << ErrorMessage << DataStringGlobals::NL;
        } else {
            // CacheIPErrorFile is never opened or closed
            // so this output would just go to stdout
            // ObjexxFCL::gio::write(CacheIPErrorFile, fmtA) << ErrorMessage;
            std::cout << ErrorMessage << '\n';
        }
        if (present(OutUnit1)) {
            print(OutUnit1(), "  {}", ErrorMessage);
        }
        if (present(OutUnit2)) {
            print(OutUnit2(), "  {}", ErrorMessage);
        }
        std::string tmp = "  " + ErrorMessage + DataStringGlobals::NL;
        if (DataGlobals::errorCallback) DataGlobals::errorCallback(tmp.c_str());
    }

    void SummarizeErrors()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine provides a summary of certain errors that might
        // otherwise get lost in the shuffle of many similar messages.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataErrorTracking;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string::size_type StartC;
        std::string::size_type EndC;

        if (any_gt(MatchCounts, 0)) {
            ShowMessage("");
            ShowMessage("===== Final Error Summary =====");
            ShowMessage("The following error categories occurred.  Consider correcting or noting.");
            for (int Loop = 1; Loop <= SearchCounts; ++Loop) {
                if (MatchCounts(Loop) > 0) {
                    ShowMessage(Summaries(Loop));
                    if (MoreDetails(Loop) != "") {
                        StartC = 0;
                        EndC = len(MoreDetails(Loop)) - 1;
                        while (EndC != std::string::npos) {
                            EndC = index(MoreDetails(Loop).substr(StartC), "<CR");
                            ShowMessage(".." + MoreDetails(Loop).substr(StartC, EndC));
                            if (MoreDetails(Loop).substr(StartC + EndC, 5) == "<CRE>") break;
                            StartC += EndC + 4;
                            EndC = len(MoreDetails(Loop).substr(StartC)) - 1;
                        }
                    }
                }
            }
            ShowMessage("");
        }
    }

    void ShowRecurringErrors()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine provides a summary of certain errors that might
        // otherwise get lost in the shuffle of many similar messages.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataErrorTracking;
        using General::RoundSigDigits;
        using General::strip_trailing_zeros;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const StatMessageStart(" **   ~~~   ** ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        std::string StatMessage;
        std::string MaxOut;
        std::string MinOut;
        std::string SumOut;

        if (NumRecurringErrors > 0) {
            ShowMessage("");
            ShowMessage("===== Recurring Error Summary =====");
            ShowMessage("The following recurring error messages occurred.");
            for (Loop = 1; Loop <= NumRecurringErrors; ++Loop) {
                auto const &error(RecurringErrors(Loop));
                // Suppress reporting the count if it is a continue error
                if (has_prefix(error.Message, " **   ~~~   ** ")) {
                    ShowMessage(error.Message);
                    if (sqlite) {
                        sqlite->updateSQLiteErrorRecord(error.Message);
                    }
                } else {
                    ShowMessage("");
                    ShowMessage(error.Message);
                    ShowMessage(StatMessageStart + "  This error occurred " + RoundSigDigits(error.Count) + " total times;");
                    ShowMessage(StatMessageStart + "  during Warmup " + RoundSigDigits(error.WarmupCount) + " times;");
                    ShowMessage(StatMessageStart + "  during Sizing " + RoundSigDigits(error.SizingCount) + " times.");
                    if (sqlite) {
                        if (has_prefix(error.Message, " ** Warning ** ")) {
                            sqlite->createSQLiteErrorRecord(1, 0, error.Message.substr(15), error.Count);
                        } else if (has_prefix(error.Message, " ** Severe  ** ")) {
                            sqlite->createSQLiteErrorRecord(1, 1, error.Message.substr(15), error.Count);
                        }
                    }
                }
                StatMessage = "";
                if (error.ReportMax) {
                    MaxOut = RoundSigDigits(error.MaxValue, 6);
                    strip_trailing_zeros(MaxOut);
                    StatMessage += "  Max=" + MaxOut;
                    if (!error.MaxUnits.empty()) StatMessage += ' ' + error.MaxUnits;
                }
                if (error.ReportMin) {
                    MinOut = RoundSigDigits(error.MinValue, 6);
                    strip_trailing_zeros(MinOut);
                    StatMessage += "  Min=" + MinOut;
                    if (!error.MinUnits.empty()) StatMessage += ' ' + error.MinUnits;
                }
                if (error.ReportSum) {
                    SumOut = RoundSigDigits(error.SumValue, 6);
                    strip_trailing_zeros(SumOut);
                    StatMessage += "  Sum=" + SumOut;
                    if (!error.SumUnits.empty()) StatMessage += ' ' + error.SumUnits;
                }
                if (error.ReportMax || error.ReportMin || error.ReportSum) {
                    ShowMessage(StatMessageStart + StatMessage);
                }
            }
            ShowMessage("");
        }
    }

} // namespace UtilityRoutines
