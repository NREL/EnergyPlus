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

#include <fstream>

#include <EnergyPlus.hh>
#include <InputProcessing/InputProcessor.hh>
#include <Scheduling/Base.hh>
#include <Scheduling/YearFile.hh>
#include <UtilityRoutines.hh>

namespace Scheduling {

std::vector<ScheduleFile> scheduleFiles;
std::map<std::string, std::vector<std::vector<std::string>>> fileData;

void ScheduleFile::processInput()
{
    std::string const thisObjectType = "Schedule:File";
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
        scheduleFiles.emplace_back(thisObjectName, fields);
    }
}

void ScheduleFile::clear_state()
{
    scheduleFiles.clear();
    fileData.clear();
}

void ScheduleFile::createTimeSeries()
{
    // This function creates the time/value time-series for a specific subset of the master file-based schedule set
    // This function skips rows at the top, grabs the right column of data, and converts the values into numerics
    auto & dataSet = Scheduling::fileData[this->fileName];
    auto & thisColumnOfData = dataSet[this->columnNumber - 1];
    int rowNum = 0;
    int dataRowCount = 0;
    for (auto const & datum : thisColumnOfData) {
        rowNum++;
        if (rowNum > this->rowsToSkipAtTop) {
            dataRowCount++;
            this->timeStamp.push_back(dataRowCount * this->minutesPerItem * 60); // TODO: Test a non-hourly interval
            if (datum.empty()) {
                this->values.push_back(0);
            } else {
                try {
                    this->values.push_back(std::stod(datum));
                } catch (...) {
                    EnergyPlus::ShowFatalError("Failed to convert " + datum + " to numeric value");
                }
            }
        }
    }
    if (dataRowCount != this->expectedRowCount) {
        EnergyPlus::ShowFatalError("Malformed schedule file, invalid row count, expected to find this row count: " + std::to_string(this->expectedRowCount));
    }
}

std::vector<std::vector<std::string>> ScheduleFile::processCSVLines(std::vector<std::string> const & lines) {
    // This function takes a list of CSV lines and splits them into string tokens - note it does not parse numeric values yet
    // This essentially just creates a 2D table of data in a vector of vectors of strings, where the first index is the column
    // First we should find the number of columns in this file; we are going to base it on the number of tokens in line 1
    auto & line0 = lines[0];
    int maxExpectedColumnIndex = -1;
    std::stringstream ss2(line0);
    while( ss2.good() ) {
        maxExpectedColumnIndex++;
        std::string substr;
        getline(ss2, substr, this->columnDelimiter);  // TODO: Test all 4 delimiters
    }
    // then we'll actually get the data from the file, filling out to the number of expected columns based on the header (first) line
    std::vector<std::vector<std::string>> overallDataset;
    int lineCounter = -1;
    for (auto const & line : lines) {
        lineCounter++;
        std::stringstream ss(line);
        int columnIndex = -1;
        while( ss.good() ) {
            columnIndex++;
            std::string substr;
            getline(ss, substr, this->columnDelimiter);
            if (lineCounter == 0) {
                overallDataset.emplace_back();
            }
            overallDataset[columnIndex].push_back(substr);
        }
        if (columnIndex < maxExpectedColumnIndex) {
            for (int i = columnIndex + 1; i <= maxExpectedColumnIndex; i++) {
                overallDataset[i].push_back("");
            }
        }
    }
    return overallDataset;
}

void ScheduleFile::processCSVFile(const std::string& fileToOpen)
{
    // This function adds the entire file contents of the given file path to a "master" map called Scheduling::fileData
    // This file does not populate individual schedules, it simply adds it to the list for later processing
    // Multiple schedules could access different subsets of the same file, so we only want to process each file once
    std::ifstream fileInstance(fileToOpen);
    if (!fileInstance.is_open()) {
        EnergyPlus::ShowFatalError("Could not open schedule file for processing, check path: " + fileToOpen);
    }
    std::vector<std::string> lines;
    std::string line;
    while (getline(fileInstance, line)) {
        lines.push_back(line);
    }
    Scheduling::fileData[fileToOpen] = processCSVLines(lines);
}

ScheduleFile::ScheduleFile(std::string const &objectName, nlohmann::json const &fields)
{
    // Schedule:File,
    // \min-fields 5
    //       \memo A Schedule:File points to a text computer file that has 8760-8784 hours of data.
    //  A1 , \field Name
    //       \required-field
    //       \type alpha
    //       \reference ScheduleNames
    //  A2 , \field Schedule Type Limits Name
    //       \type object-list
    //       \object-list ScheduleTypeLimitsNames
    //  A3 , \field File Name
    //       \required-field
    //       \retaincase
    //  N1 , \field Column Number
    //       \required-field
    //       \type integer
    //       \minimum 1
    //  N2 , \field Rows to Skip at Top
    //       \required-field
    //       \type integer
    //       \minimum 0
    //  N3 , \field Number of Hours of Data
    //       \note 8760 hours does not account for leap years, 8784 does.
    //       \note should be either 8760 or 8784
    //       \default 8760
    //       \minimum 8760
    //       \maximum 8784
    //  A4 , \field Column Separator
    //       \type choice
    //       \key Comma
    //       \key Tab
    //       \key Space
    //       \key Semicolon
    //       \default Comma
    //  A5 , \field Interpolate to Timestep
    //       \note when the interval does not match the user specified timestep a "Yes" choice will average between the intervals request (to
    //       \note timestep resolution.  a "No" choice will use the interval value at the simulation timestep without regard to if it matches
    //       \note the boundary or not.
    //       \type choice
    //       \key Yes
    //       \key No
    //       \default No
    //  N4 ; \field Minutes per Item
    //       \note Must be evenly divisible into 60
    //       \type integer
    //       \minimum 1
    //       \maximum 60
    this->name = objectName;
    this->typeName = "Schedule:File";
    // these are required
    this->fileName = fields.at("file_name");
    this->columnNumber = fields.at("column_number");
    this->rowsToSkipAtTop = fields.at("rows_to_skip_at_top");
    // then there are optionals
    if (fields.find("schedule_type_limits_name") != fields.end()) {
        this->typeLimits = ScheduleTypeData::factory(EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("schedule_type_limits_name")));
    }
    if(fields.find("minutes_per_item") != fields.end()) {
        this->minutesPerItem = fields.at("minutes_per_item");
        switch (this->minutesPerItem) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 10:
        case 12:
        case 15:
        case 30:
        case 60:
            break;
        default:
            EnergyPlus::ShowFatalError("Invalid minutes per item, must be evenly divisible into an hour for Schedule:File " + this->name);
        }
    }
    if(fields.find("column_separator") != fields.end()) {
        std::string separatorUpperCase = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("column_separator"));
        if (separatorUpperCase == "COMMA") {
            this->columnDelimiter = ',';
        } else if (separatorUpperCase == "SEMICOLON") {
            this->columnDelimiter = ';';
        } else if (separatorUpperCase == "SPACE") {
            this->columnDelimiter = ' ';
        } else if (separatorUpperCase == "TAB") {
            this->columnDelimiter = '\t';
        } else {
            EnergyPlus::ShowFatalError("Schedule:File named \"" + this->name + "\": Bad column separator value: \"" + separatorUpperCase + "\"");
        }
    }
    if(fields.find("interpolate_to_timestep") != fields.end()) {
        try {
            // try direct casting, if not we'll revert to string parsing
            this->interpolateToTimeStep = fields.at("interpolate_to_timestep");
        } catch (nlohmann::json::type_error &) {
            if (EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("interpolate_to_timestep")) == "YES") {
                this->interpolateToTimeStep = true;
            }
        }
    }
    int numberOfHoursOfData = 8760;
    if(fields.find("number_of_hours_of_data") != fields.end()) {
        numberOfHoursOfData = fields.at("number_of_hours_of_data");
    }
    this->expectedRowCount = numberOfHoursOfData * 60 / this->minutesPerItem;
    // only process the file if it isn't already in the master list
    if (fileData.find(fields.at("file_name")) == fileData.end()) {
        this->processCSVFile(fields.at("file_name"));
    }
}

void ScheduleFile::prepareForNewEnvironment()
{
    this->timeStamp.clear();
    this->values.clear();
    this->lastIndexUsed = 0;
    this->createTimeSeries();
    if (this->typeLimits) {
        this->validateTypeLimits();
    }
    // TODO: Check for error flag?
}

} // namespace Scheduling
