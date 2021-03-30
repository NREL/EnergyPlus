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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <map>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/format.h>
#include <milo/dtoa.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Reference.fwd.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ResultsFramework {

    using namespace OutputProcessor;
    using OutputProcessor::RealVariables;
    using OutputProcessor::RealVariableType;

    // trim string
    std::string trim(std::string str)
    {
        str.erase(str.begin(), find_if(str.begin(), str.end(), [](char &ch) -> bool { return !isspace(ch); }));
        str.erase(find_if(str.rbegin(), str.rend(), [](char &ch) -> bool { return !isspace(ch); }).base(), str.end());
        return str;
    }

    // Class SimInfo
    void SimInfo::setProgramVersion(const std::string &programVersion)
    {
        ProgramVersion = programVersion;
    }

    std::string SimInfo::getProgramVersion()
    {
        return ProgramVersion;
    }

    void SimInfo::setSimulationEnvironment(const std::string &simulationEnvironment)
    {
        SimulationEnvironment = simulationEnvironment;
    }

    void SimInfo::setInputModelURI(const std::string &inputModelURI)
    {
        InputModelURI = inputModelURI;
    }

    void SimInfo::setStartDateTimeStamp(const std::string &startDateTimeStamp)
    {
        StartDateTimeStamp = startDateTimeStamp;
    }

    void SimInfo::setRunTime(const std::string &elapsedTime)
    {
        RunTime = elapsedTime;
    }

    void SimInfo::setNumErrorsWarmup(const std::string &numWarningsDuringWarmup, const std::string &numSevereDuringWarmup)
    {
        NumWarningsDuringWarmup = numWarningsDuringWarmup;
        NumSevereDuringWarmup = numSevereDuringWarmup;
    }

    void SimInfo::setNumErrorsSizing(const std::string &numWarningsDuringSizing, const std::string &numSevereDuringSizing)
    {
        NumWarningsDuringSizing = numWarningsDuringSizing;
        NumSevereDuringSizing = numSevereDuringSizing;
    }

    void SimInfo::setNumErrorsSummary(const std::string &numWarnings, const std::string &numSevere)
    {
        NumWarnings = numWarnings;
        NumSevere = numSevere;
    }

    json SimInfo::getJSON() const
    {
        json root = {{"ProgramVersion", ProgramVersion},
                     {"SimulationEnvironment", SimulationEnvironment},
                     {"InputModelURI", InputModelURI},
                     {"StartDateTimeStamp", StartDateTimeStamp},
                     {"RunTime", RunTime},
                     {"ErrorSummary", {{"NumWarnings", NumWarnings}, {"NumSevere", NumSevere}}},
                     {"ErrorSummaryWarmup", {{"NumWarnings", NumWarningsDuringWarmup}, {"NumSevere", NumSevereDuringWarmup}}},
                     {"ErrorSummarySizing", {{"NumWarnings", NumWarningsDuringSizing}, {"NumSevere", NumSevereDuringSizing}}}};
        return root;
    }

    // Class Variable
    Variable::Variable(const std::string &VarName,
                       const OutputProcessor::ReportingFrequency reportFrequency,
                       const OutputProcessor::TimeStepType timeStepType,
                       const int ReportID,
                       const OutputProcessor::Unit &units)
        : varName(VarName), m_timeStepType(timeStepType), rptID(ReportID), Units(units)
    {
        setReportFrequency(reportFrequency);
    }

    Variable::Variable(const std::string &VarName,
                       const OutputProcessor::ReportingFrequency reportFrequency,
                       const OutputProcessor::TimeStepType timeStepType,
                       const int ReportID,
                       const OutputProcessor::Unit &units,
                       const std::string &customUnits)
        : varName(VarName), m_timeStepType(timeStepType), rptID(ReportID), Units(units), m_customUnits(customUnits)
    {
        setReportFrequency(reportFrequency);
    }

    std::string Variable::variableName() const
    {
        return varName;
    }

    void Variable::setVariableName(const std::string &VarName)
    {
        varName = VarName;
    }

    std::string Variable::sReportFrequency() const
    {
        return sReportFreq;
    }

    OutputProcessor::ReportingFrequency Variable::iReportFrequency() const
    {
        return iReportFreq;
    }

    void Variable::setReportFrequency(const OutputProcessor::ReportingFrequency reportFrequency)
    {
        iReportFreq = reportFrequency;
        switch (iReportFreq) {
        case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
            if (m_timeStepType == OutputProcessor::TimeStepType::TimeStepZone) sReportFreq = "Detailed - Zone";
            if (m_timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) sReportFreq = "Detailed - HVAC";
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
            sReportFreq = "TimeStep";
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at 'EndHourFlag'
            sReportFreq = "Hourly";
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at 'EndDayFlag'
            sReportFreq = "Daily";
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at end of month
            sReportFreq = "Monthly";
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // once per environment 'EndEnvrnFlag'
            sReportFreq = "RunPeriod";
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // once per environment 'EndEnvrnFlag'
            sReportFreq = "Yearly";
            break;
        }
    }

    OutputProcessor::TimeStepType Variable::timeStepType() const
    {
        return m_timeStepType;
    }

    void Variable::setTimeStepType(const OutputProcessor::TimeStepType timeStepType)
    {
        m_timeStepType = timeStepType;
    }

    int Variable::reportID() const
    {
        return rptID;
    }

    void Variable::setReportID(int Id)
    {
        rptID = Id;
    }

    OutputProcessor::Unit Variable::units() const
    {
        return Units;
    }

    void Variable::setUnits(const OutputProcessor::Unit &units)
    {
        Units = units;
    }

    std::string Variable::customUnits() const
    {
        return m_customUnits;
    }

    void Variable::setCustomUnits(const std::string &customUnits)
    {
        m_customUnits = customUnits;
    }

    void Variable::pushValue(const double val)
    {
        Values.push_back(val);
    }

    double Variable::value(size_t index) const
    {
        return Values.at(index);
    }

    size_t Variable::numValues() const
    {
        return Values.size();
    }

    json Variable::getJSON() const
    {
        json root;
        if (m_customUnits.empty()) {
            root = {{"Name", varName}, {"Units", unitEnumToString(Units)}, {"Frequency", sReportFreq}};
        } else {
            root = {{"Name", varName}, {"Units", m_customUnits}, {"Frequency", sReportFreq}};
        }
        return root;
    }

    // Class OutputVariable
    OutputVariable::OutputVariable(const std::string &VarName,
                                   const OutputProcessor::ReportingFrequency reportFrequency,
                                   const OutputProcessor::TimeStepType timeStepType,
                                   const int ReportID,
                                   const OutputProcessor::Unit &units)
        : Variable(VarName, reportFrequency, timeStepType, ReportID, units)
    {
    }

    OutputVariable::OutputVariable(const std::string &VarName,
                                   const OutputProcessor::ReportingFrequency reportFrequency,
                                   const OutputProcessor::TimeStepType timeStepType,
                                   const int ReportID,
                                   const OutputProcessor::Unit &units,
                                   const std::string &customUnits)
        : Variable(VarName, reportFrequency, timeStepType, ReportID, units, customUnits)
    {
    }

    // Class MeterVariable
    MeterVariable::MeterVariable(const std::string &VarName,
                                 const OutputProcessor::ReportingFrequency reportFrequency,
                                 const int ReportID,
                                 const OutputProcessor::Unit &units,
                                 const bool MeterOnly,
                                 const bool Accumulative)
        : Variable(VarName, reportFrequency, OutputProcessor::TimeStepType::TimeStepZone, ReportID, units)
    {
        acc = Accumulative;
        meter_only = MeterOnly;
    }

    bool MeterVariable::accumulative() const
    {
        return acc;
    }

    void MeterVariable::setAccumulative(bool state)
    {
        acc = state;
    }

    bool MeterVariable::meterOnly() const
    {
        return meter_only;
    }

    void MeterVariable::setMeterOnly(bool state)
    {
        meter_only = state;
    }

    json MeterVariable::getJSON() const
    {
        json root = Variable::getJSON();
        if (acc) {
            root["Cumulative"] = true;
        }
        //        if (meter_only) {
        //            root["MeterOnly"] = true;
        //        }
        return root;
    }

    // class DataFrame
    DataFrame::DataFrame(const std::string &ReportFreq)
    {
        ReportFrequency = ReportFreq;
    }

    void DataFrame::addVariable(Variable const &var)
    {
        lastVarID = var.reportID();
        variableMap.emplace(lastVarID, var);
    }

    Variable &DataFrame::lastVariable()
    {
        return variableMap.at(lastVarID);
    }

    void DataFrame::newRow(EnergyPlusData &state, const int month, const int dayOfMonth, int hourOfDay, int curMin)
    {
        char buffer[100];
        if (curMin > 0) {
            hourOfDay -= 1;
        }
        if (curMin == 60) {
            curMin = 0;
            hourOfDay += 1;
        }
        int cx = snprintf(buffer, 100, "%02d/%02d %02d:%02d:00", month, dayOfMonth, hourOfDay, curMin);

        // future start of ISO 8601 datetime output
        // int cx = snprintf(buffer, 100, "YYYY-%02d/%02dT%02d:%02d:00", month, dayOfMonth, hourOfDay, curMin );

        if (cx < 0 || cx > 100) {
            ShowWarningMessage(state, "Failed to convert datetime when adding new output row. Skipping row.");
            return;
        }
        TS.emplace_back(buffer);
    }

    //    void DataFrame::newRow(const std::string &ts)
    //    {
    //        TS.emplace_back(ts);
    //    }

    void DataFrame::setRDataFrameEnabled(bool state)
    {
        RDataFrameEnabled = state;
    }

    void DataFrame::setIDataFrameEnabled(bool state)
    {
        IDataFrameEnabled = state;
    }

    bool DataFrame::rDataFrameEnabled() const
    {
        return RDataFrameEnabled;
    }

    bool DataFrame::iDataFrameEnabled() const
    {
        return IDataFrameEnabled;
    }
    void DataFrame::setRVariablesScanned(bool state)
    {
        RVariablesScanned = state;
    }

    void DataFrame::setIVariablesScanned(bool state)
    {
        IVariablesScanned = state;
    }

    bool DataFrame::rVariablesScanned() const
    {
        return RVariablesScanned;
    }

    bool DataFrame::iVariablesScanned() const
    {
        return IVariablesScanned;
    }

    void DataFrame::pushVariableValue(const int reportID, double value)
    {
        variableMap[reportID].pushValue(value);
    }

    json DataFrame::getVariablesJSON()
    {
        json arr = json::array();
        for (auto const &varMap : variableMap) {
            arr.push_back(varMap.second.getJSON());
        }
        return arr;
    }

    json DataFrame::getJSON() const
    {
        json root;
        json cols = json::array();
        json rows = json::array();

        for (auto const &varMap : variableMap) {
            if (varMap.second.customUnits().empty()) {
                cols.push_back({{"Variable", varMap.second.variableName()}, {"Units", unitEnumToString(varMap.second.units())}});
            } else {
                cols.push_back({{"Variable", varMap.second.variableName()}, {"Units", varMap.second.customUnits()}});
            }
        }

        json vals = json::array();

        for (size_t row = 0; row < TS.size(); ++row) {
            vals.clear();

            for (auto const &varMap : variableMap) {
                if (row < varMap.second.numValues()) {
                    vals.push_back(varMap.second.value(row));
                } else {
                    vals.push_back(nullptr);
                }
            }
            rows.push_back({{TS.at(row), vals}});
        }
        root = {{"ReportFrequency", ReportFrequency}, {"Cols", cols}, {"Rows", rows}};
        return root;
    }

    void MeterDataFrame::addVariable(MeterVariable const &var)
    {
        lastVarID = var.reportID();
        meterMap.emplace(lastVarID, var);
    }

    void MeterDataFrame::pushVariableValue(const int reportID, double value)
    {
        meterMap[reportID].pushValue(value);
    }

    json MeterDataFrame::getJSON(bool meterOnlyCheck) const
    {
        json root;
        json cols = json::array();
        json rows = json::array();

        for (auto const &varMap : meterMap) {
            if (!(meterOnlyCheck && varMap.second.meterOnly())) {
                cols.push_back({{"Variable", varMap.second.variableName()}, {"Units", unitEnumToString(varMap.second.units())}});
            }
        }

        if (cols.empty()) return root;

        json vals = json::array();

        for (size_t row = 0; row < TS.size(); ++row) {
            vals.clear();

            for (auto const &varMap : meterMap) {
                if (!(meterOnlyCheck && varMap.second.meterOnly())) {
                    if (row < varMap.second.numValues()) {
                        vals.push_back(varMap.second.value(row));
                    } else {
                        vals.push_back(nullptr);
                    }
                }
            }
            rows.push_back({{TS.at(row), vals}});
        }
        root = {{"ReportFrequency", ReportFrequency}, {"Cols", cols}, {"Rows", rows}};
        return root;
    }

    void DataFrame::writeReport(JsonOutputStreams &jsonOutputStreams, bool outputJSON, bool outputCBOR, bool outputMsgPack)
    {

        json root = getJSON();
        if (ReportFrequency == "Detailed-HVAC") {
            if (outputJSON && jsonOutputStreams.json_TSstream_HVAC) {
                *(jsonOutputStreams.json_TSstream_HVAC) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_TSstream_HVAC) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_TSstream_HVAC));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_TSstream_HVAC) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_TSstream_HVAC));
            }
        } else if (ReportFrequency == "Detailed-Zone") {
            if (outputJSON && jsonOutputStreams.json_TSstream_Zone) {
                *(jsonOutputStreams.json_TSstream_Zone) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_TSstream_Zone) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_TSstream_Zone));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_TSstream_Zone) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_TSstream_Zone));
            }
        } else if (ReportFrequency == "TimeStep") {
            if (outputJSON && jsonOutputStreams.json_TSstream) {
                *(jsonOutputStreams.json_TSstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_TSstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_TSstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_TSstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_TSstream));
            }
        } else if (ReportFrequency == "Daily") {
            if (outputJSON && jsonOutputStreams.json_DYstream) {
                *(jsonOutputStreams.json_DYstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_DYstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_DYstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_DYstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_DYstream));
            }
        } else if (ReportFrequency == "Hourly") {
            if (outputJSON && jsonOutputStreams.json_HRstream) {
                *(jsonOutputStreams.json_HRstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_HRstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_HRstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_HRstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_HRstream));
            }
        } else if (ReportFrequency == "Monthly") {
            if (outputJSON && jsonOutputStreams.json_MNstream) {
                *(jsonOutputStreams.json_MNstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_MNstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_MNstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_MNstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_MNstream));
            }
        } else if (ReportFrequency == "RunPeriod") {
            if (outputJSON && jsonOutputStreams.json_SMstream) {
                *(jsonOutputStreams.json_SMstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_SMstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_SMstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_SMstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_SMstream));
            }
        } else if (ReportFrequency == "Yearly") {
            if (outputJSON && jsonOutputStreams.json_YRstream) {
                *(jsonOutputStreams.json_YRstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && jsonOutputStreams.cbor_YRstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.cbor_YRstream));
            }
            if (outputMsgPack && jsonOutputStreams.msgpack_YRstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.msgpack_YRstream));
            }
        }
    }
    // class Table

    Table::Table(Array2D_string const &body,
                 Array1D_string const &rowLabels,
                 Array1D_string const &columnLabels,
                 std::string const &tableName,
                 std::string const &footnoteText)
    {

        size_t sizeColumnLabels = columnLabels.size();
        size_t sizeRowLabels = rowLabels.size();
        TableName = tableName;
        FootnoteText = footnoteText;

        for (size_t iCol = 0, k = body.index(1, 1); iCol < sizeColumnLabels; ++iCol) {
            ColHeaders.push_back(columnLabels[iCol]);
            std::vector<std::string> col;
            for (size_t iRow = 0; iRow < sizeRowLabels; ++iRow) {
                if (iCol == 0) {
                    // do this once only
                    RowHeaders.push_back(rowLabels[iRow]);
                }
                col.push_back(trim(body[k]));
                ++k;
            }
            Data.push_back(col);
        }
    }

    json Table::getJSON() const
    {
        json root;
        json cols = json::array();
        json rows;

        for (size_t col = 0; col < ColHeaders.size(); ++col) {
            cols.push_back(ColHeaders[col]);
        }

        for (size_t row = 0; row < RowHeaders.size(); ++row) {
            json rowvec = json::array();
            for (size_t col = 0; col < ColHeaders.size(); ++col) {
                rowvec.push_back(Data[col][row]);
            }
            rows[RowHeaders[row]] = rowvec;
        }

        root = {{"TableName", TableName}, {"Cols", cols}, {"Rows", rows}};

        if (!FootnoteText.empty()) root["Footnote"] = FootnoteText;
        return root;
    }

    // class Report

    json Report::getJSON() const
    {

        json root = {{"ReportName", ReportName}, {"For", ReportForString}};

        json cols = json::array();

        for (auto const &table : Tables) {
            cols.push_back(table.getJSON());
        }

        root["Tables"] = cols;
        return root;
    }

    // class ReportsCollection
    ReportsCollection::ReportsCollection()
    {
    }

    void ReportsCollection::addReportTable(Array2D_string const &body,
                                           Array1D_string const &rowLabels,
                                           Array1D_string const &columnLabels,
                                           std::string const &reportName,
                                           std::string const &reportForString,
                                           std::string const &tableName)
    {
        addReportTable(body, rowLabels, columnLabels, reportName, reportForString, tableName, "");
    }

    void ReportsCollection::addReportTable(Array2D_string const &body,
                                           Array1D_string const &rowLabels,
                                           Array1D_string const &columnLabels,
                                           std::string const &reportName,
                                           std::string const &reportForString,
                                           std::string const &tableName,
                                           std::string const &footnoteText)
    {
        std::string const key = reportName + reportForString;
        // Report *r;
        Table tbl(body, rowLabels, columnLabels, tableName, footnoteText);

        auto search = reportsMap.find(key);
        if (search != reportsMap.end()) {
            // r = search->second;
            search->second.Tables.push_back(tbl);
        } else {
            // r = new Report();
            Report r;
            r.ReportName = reportName;
            r.ReportForString = reportForString;
            r.Tables.push_back(tbl);
            reportsMap.emplace(key, r);
        }

        // Table *tbl = new Table( body, rowLabels, columnLabels, tableName, footnoteText );
        // r->Tables.push_back( tbl );
    }

    json ReportsCollection::getJSON() const
    {
        json root = json::array();

        for (auto const &iter : reportsMap) {
            root.push_back(iter.second.getJSON());
        }
        return root;
    }

    void CSVWriter::parseTSOutputs(EnergyPlusData &state,
                                   json const &data,
                                   std::vector<std::string> const &outputVariables,
                                   OutputProcessor::ReportingFrequency reportingFrequency)
    {
        if (data.empty()) return;
        updateReportingFrequency(reportingFrequency);
        std::vector<int> indices;
        std::unordered_set<std::string> seen;
        std::string search_string;

        std::string reportFrequency = data.at("ReportFrequency").get<std::string>();
        if (reportFrequency == "Detailed-HVAC" || reportFrequency == "Detailed-Zone") {
            reportFrequency = "Each Call";
        }
        auto const &columns = data.at("Cols");
        for (auto const &column : columns) {
            search_string =
                fmt::format("{0} [{1}]({2})", column.at("Variable").get<std::string>(), column.at("Units").get<std::string>(), reportFrequency);
            auto found = std::find(outputVariables.begin(), outputVariables.end(), search_string);
            if (found == outputVariables.end()) {
                search_string =
                    fmt::format("{0} [{1}]({2})", column.at("Variable").get<std::string>(), column.at("Units").get<std::string>(), "Each Call");
                found = std::find(outputVariables.begin(), outputVariables.end(), search_string);
            }
            if (found == outputVariables.end()) {
                ShowFatalError(state, fmt::format("Output variable ({0}) not found output variable list", search_string));
            }
            outputVariableIndices[std::distance(outputVariables.begin(), found)] = true;
            indices.emplace_back(std::distance(outputVariables.begin(), found));
        }

        auto const &rows = data.at("Rows");
        for (auto const &row : rows) {
            for (auto &el : row.items()) {
                auto found_key = outputs.find(el.key());
                if (found_key == outputs.end()) {
                    std::vector<std::string> output(outputVariables.size());
                    int i = 0;
                    for (auto const &col : el.value()) {
                        if (col.is_null()) {
                            output[indices[i]] = "";
                        } else {
                            dtoa(col.get<double>(), s);
                            output[indices[i]] = s;
                        }
                        ++i;
                    }
                    outputs[el.key()] = output;
                } else {
                    int i = 0;
                    for (auto const &col : el.value()) {
                        if (col.is_null()) {
                            found_key->second[indices[i]] = "";
                        } else {
                            dtoa(col.get<double>(), s);
                            found_key->second[indices[i]] = s;
                        }
                        ++i;
                    }
                }
            }
        }
    }

    void CSVWriter::updateReportingFrequency(OutputProcessor::ReportingFrequency reportingFrequency)
    {
        if (reportingFrequency < smallestReportingFrequency) {
            smallestReportingFrequency = reportingFrequency;
        }
    }

    std::string &CSVWriter::convertToMonth(EnergyPlusData &state, std::string &datetime)
    {
        // if running this function, there should only ever be 12 + design days values to change
        static const std::map<std::string, std::string> months({{"01", "January"},
                                                                {"02", "February"},
                                                                {"03", "March"},
                                                                {"04", "April"},
                                                                {"05", "May"},
                                                                {"06", "June"},
                                                                {"07", "July"},
                                                                {"08", "August"},
                                                                {"09", "September"},
                                                                {"10", "October"},
                                                                {"11", "November"},
                                                                {"12", "December"}});
        // 01/01 24:00:00
        auto const month = datetime.substr(0, 2);
        auto const pos = datetime.find(' ');
        std::string time;
        if (pos != std::string::npos) {
            time = datetime.substr(pos);
        }
        if (time != " 24:00:00") {
            ShowFatalError(state, "Monthly output variables should occur at the end of the day.");
        }
        datetime = months.find(month)->second;
        return datetime;
    }

    void
    CSVWriter::writeOutput(EnergyPlusData &state, std::vector<std::string> const &outputVariables, InputOutputFile &outputFile, bool outputControl)
    {
        outputFile.ensure_open(state, "OpenOutputFiles", outputControl);

        print(outputFile, "{}", "Date/Time,");
        std::string sep;
        for (auto it = outputVariables.begin(); it != outputVariables.end(); ++it) {
            if (!outputVariableIndices[std::distance(outputVariables.begin(), it)]) continue;
            print(outputFile, "{}{}", sep, *it);
            if (sep.empty()) sep = ",";
        }
        print(outputFile, "{}", '\n');

        for (auto &item : outputs) {
            std::string datetime = item.first;
            if (smallestReportingFrequency < OutputProcessor::ReportingFrequency::Monthly) {
                datetime = datetime.replace(datetime.find(' '), 1, "  ");
            } else {
                convertToMonth(state, datetime);
            }
            print(outputFile, " {},", datetime);
            item.second.erase(std::remove_if(item.second.begin(),
                                             item.second.end(),
                                             [&](const std::string &d) {
                                                 auto pos = (&d - &*item.second.begin());
                                                 return !outputVariableIndices[pos];
                                             }),
                              item.second.end());
            auto result = std::find_if(item.second.rbegin(), item.second.rend(), [](std::string const &v) { return !v.empty(); });
            auto last = item.second.end() - 1;
            if (result != item.second.rend()) {
                last = (result + 1).base();
            }
            print(item.second.begin(), last, outputFile, ",");
            print(outputFile, "{}{}", *last, '\n');
        }

        outputFile.close();
    }

    void ResultsFramework::setupOutputOptions(EnergyPlusData &state)
    {
        if (state.files.outputControl.csv) {
            tsEnabled = true;
            tsAndTabularEnabled = true;
        }

        if (!state.files.outputControl.json) {
            return;
        }

        int numberOfOutputSchemaObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Output:JSON");
        if (numberOfOutputSchemaObjects == 0) {
            return;
        }

        Array1D_string alphas(5);
        int numAlphas;
        Array1D<Real64> numbers(2);
        int numNumbers;
        int status;
        state.dataInputProcessing->inputProcessor->getObjectItem(state, "Output:JSON", 1, alphas, numAlphas, numbers, numNumbers, status);

        if (numAlphas > 0) {
            std::string option = alphas(1);
            if (UtilityRoutines::SameString(option, "TimeSeries")) {
                tsEnabled = true;
            } else if (UtilityRoutines::SameString(option, "TimeSeriesAndTabular")) {
                tsEnabled = true;
                tsAndTabularEnabled = true;
            }

            // defaults
            outputJSON = true;
            outputCBOR = false;
            outputMsgPack = false;

            if (numAlphas >= 2) {
                outputJSON = UtilityRoutines::SameString(alphas(2), "Yes");
            }

            if (numAlphas >= 3) {
                outputCBOR = UtilityRoutines::SameString(alphas(3), "Yes");
            }

            if (numAlphas >= 4) {
                outputMsgPack = UtilityRoutines::SameString(alphas(4), "Yes");
            }
        }
    }

    bool ResultsFramework::timeSeriesEnabled() const
    {
        return tsEnabled;
    }

    bool ResultsFramework::timeSeriesAndTabularEnabled() const
    {
        return tsAndTabularEnabled;
    }

    bool ResultsFramework::JSONEnabled() const
    {
        return outputJSON;
    }

    bool ResultsFramework::CBOREnabled() const
    {
        return outputCBOR;
    }

    bool ResultsFramework::MsgPackEnabled() const
    {
        return outputMsgPack;
    }

    void ResultsFramework::initializeRTSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                                  const Array1D<RealVariableType> &RVariableTypes,
                                                  const int NumOfRVariable,
                                                  const OutputProcessor::TimeStepType timeStepType)
    {
        Reference<RealVariables> RVar;

        for (int Loop = 1; Loop <= NumOfRVariable; ++Loop) {
            RVar >>= RVariableTypes(Loop).VarPtr;
            auto &rVar(RVar());
            if (rVar.Report && rVar.frequency == reportFrequency) {
                Variable var;
                if (RVariableTypes(Loop).units == OutputProcessor::Unit::customEMS) {
                    var = Variable(RVariableTypes(Loop).VarName,
                                   reportFrequency,
                                   RVariableTypes(Loop).timeStepType,
                                   RVariableTypes(Loop).ReportID,
                                   RVariableTypes(Loop).units,
                                   RVariableTypes(Loop).unitNameCustomEMS);
                } else {
                    var = Variable(RVariableTypes(Loop).VarName,
                                   reportFrequency,
                                   RVariableTypes(Loop).timeStepType,
                                   RVariableTypes(Loop).ReportID,
                                   RVariableTypes(Loop).units);
                }
                switch (reportFrequency) {
                case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
                    if ((timeStepType == OutputProcessor::TimeStepType::TimeStepZone) &&
                        (RVariableTypes(Loop).timeStepType == OutputProcessor::TimeStepType::TimeStepZone)) {
                        RIDetailedZoneTSData.setRDataFrameEnabled(true);
                        RIDetailedZoneTSData.addVariable(var);
                    } else if ((timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) &&
                               (RVariableTypes(Loop).timeStepType == OutputProcessor::TimeStepType::TimeStepSystem)) {
                        RIDetailedHVACTSData.setRDataFrameEnabled(true);
                        RIDetailedHVACTSData.addVariable(var);
                    }
                    break;
                case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
                    RITimestepTSData.setRDataFrameEnabled(true);
                    RITimestepTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Hourly: // at 'EndHourFlag'
                    RIHourlyTSData.setRDataFrameEnabled(true);
                    RIHourlyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Daily: // at 'EndDayFlag'
                    RIDailyTSData.setRDataFrameEnabled(true);
                    RIDailyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Monthly: // at end of month
                    RIMonthlyTSData.setRDataFrameEnabled(true);
                    RIMonthlyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Simulation: // once per environment 'EndEnvrnFlag'
                    RIRunPeriodTSData.setRDataFrameEnabled(true);
                    RIRunPeriodTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Yearly: // at end of year
                    RIYearlyTSData.setRDataFrameEnabled(true);
                    RIYearlyTSData.addVariable(var);
                    break;
                }
            }
        }
        // set the scanned variables to true or false
        switch (reportFrequency) {
        case OutputProcessor::ReportingFrequency::EachCall:
            if (timeStepType == OutputProcessor::TimeStepType::TimeStepZone) {
                RIDetailedZoneTSData.setRVariablesScanned(true);
            } else if (timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) {
                RIDetailedHVACTSData.setRVariablesScanned(true);
            }
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
            RITimestepTSData.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at 'EndHourFlag'
            RIHourlyTSData.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at 'EndDayFlag'
            RIDailyTSData.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at end of month
            RIMonthlyTSData.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // once per environment 'EndEnvrnFlag'
            RIRunPeriodTSData.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // at end of year
            RIYearlyTSData.setRVariablesScanned(true);
            break;
        }
    }

    void ResultsFramework::initializeITSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                                  const Array1D<IntegerVariableType> &IVariableTypes,
                                                  const int NumOfIVariable,
                                                  const OutputProcessor::TimeStepType timeStepType)
    {
        Reference<IntegerVariables> IVar;

        // loop over values to suck in var info
        for (int Loop = 1; Loop <= NumOfIVariable; ++Loop) {
            IVar >>= IVariableTypes(Loop).VarPtr;
            auto &iVar(IVar());
            if (iVar.Report && iVar.frequency == reportFrequency) {
                OutputVariable var(IVariableTypes(Loop).VarName,
                                   reportFrequency,
                                   IVariableTypes(Loop).timeStepType,
                                   IVariableTypes(Loop).ReportID,
                                   IVariableTypes(Loop).units);
                switch (reportFrequency) {
                case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
                    if ((timeStepType == OutputProcessor::TimeStepType::TimeStepZone) &&
                        (IVariableTypes(Loop).timeStepType == OutputProcessor::TimeStepType::TimeStepZone)) {
                        RIDetailedZoneTSData.setIDataFrameEnabled(true);
                        RIDetailedZoneTSData.addVariable(var);
                    } else if ((timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) &&
                               (IVariableTypes(Loop).timeStepType == OutputProcessor::TimeStepType::TimeStepSystem)) {
                        RIDetailedHVACTSData.setIDataFrameEnabled(true);
                        RIDetailedHVACTSData.addVariable(var);
                    }
                    break;
                case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
                    RITimestepTSData.setIDataFrameEnabled(true);
                    RITimestepTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Hourly: // at 'EndHourFlag'
                    RIHourlyTSData.setIDataFrameEnabled(true);
                    RIHourlyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Daily: // at 'EndDayFlag'
                    RIDailyTSData.setIDataFrameEnabled(true);
                    RIDailyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Monthly: // at end of month
                    RIMonthlyTSData.setIDataFrameEnabled(true);
                    RIMonthlyTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Simulation: // once per environment 'EndEnvrnFlag'
                    RIRunPeriodTSData.setIDataFrameEnabled(true);
                    RIRunPeriodTSData.addVariable(var);
                    break;
                case OutputProcessor::ReportingFrequency::Yearly: // once per environment 'EndEnvrnFlag'
                    RIYearlyTSData.setIDataFrameEnabled(true);
                    RIYearlyTSData.addVariable(var);
                    break;
                }
            }
        }

        // set the scanned variables to true or false
        switch (reportFrequency) {
        case OutputProcessor::ReportingFrequency::EachCall:
            if (timeStepType == OutputProcessor::TimeStepType::TimeStepZone) {
                RIDetailedZoneTSData.setIVariablesScanned(true);
            } else if (timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) {
                RIDetailedHVACTSData.setIVariablesScanned(true);
            }
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
            RITimestepTSData.setIVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at 'EndHourFlag'
            RIHourlyTSData.setIVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at 'EndDayFlag'
            RIDailyTSData.setIVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at end of month
            RIMonthlyTSData.setIVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // once per environment 'EndEnvrnFlag'
            RIRunPeriodTSData.setIVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // once per environment 'EndEnvrnFlag'
            RIYearlyTSData.setIVariablesScanned(true);
            break;
        }
    }

    void ResultsFramework::initializeMeters(const Array1D<OutputProcessor::MeterType> &EnergyMeters,
                                            const OutputProcessor::ReportingFrequency reportFrequency)
    {
        switch (reportFrequency) {
        case OutputProcessor::ReportingFrequency::EachCall:
            // nothing to do; meters are not reported at this frequency
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'TimeStep'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptTS || EnergyMeters(Loop).RptTSFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).TSRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptTSFO);
                    TSMeters.addVariable(var);
                    TSMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccTS || EnergyMeters(Loop).RptAccTSFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).TSAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccTSFO);
                    TSMeters.addVariable(var);
                    TSMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at 'Hourly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptHR || EnergyMeters(Loop).RptHRFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).HRRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptHRFO);
                    HRMeters.addVariable(var);
                    HRMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccHR || EnergyMeters(Loop).RptAccHRFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).HRAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccHRFO);
                    HRMeters.addVariable(var);
                    HRMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at 'Daily'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptDY || EnergyMeters(Loop).RptDYFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).DYRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptDYFO);
                    DYMeters.addVariable(var);
                    DYMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccDY || EnergyMeters(Loop).RptAccDYFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).DYAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccDYFO);
                    DYMeters.addVariable(var);
                    DYMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at 'Monthly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptMN || EnergyMeters(Loop).RptMNFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).MNRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptMNFO);
                    MNMeters.addVariable(var);
                    MNMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccMN || EnergyMeters(Loop).RptAccMNFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).MNAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccMNFO);
                    MNMeters.addVariable(var);
                    MNMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // at 'RunPeriod'/'SM'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptSM || EnergyMeters(Loop).RptSMFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).SMRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptSMFO);
                    SMMeters.addVariable(var);
                    SMMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccSM || EnergyMeters(Loop).RptAccSMFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).SMAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccSMFO);
                    SMMeters.addVariable(var);
                    SMMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // at 'Yearly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptYR || EnergyMeters(Loop).RptYRFO) {
                    MeterVariable var(
                        EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).YRRptNum, EnergyMeters(Loop).Units, EnergyMeters(Loop).RptYRFO);
                    YRMeters.addVariable(var);
                    YRMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccYR || EnergyMeters(Loop).RptAccYRFO) {
                    MeterVariable var(EnergyMeters(Loop).Name,
                                      reportFrequency,
                                      EnergyMeters(Loop).YRAccRptNum,
                                      EnergyMeters(Loop).Units,
                                      EnergyMeters(Loop).RptAccDYFO);
                    YRMeters.addVariable(var);
                    YRMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        }

        // set the scanned variables to true or false
        switch (reportFrequency) {
        case OutputProcessor::ReportingFrequency::EachCall:
            // case should not happen in Meters
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at TimeStepFlag
            TSMeters.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at Hourly
            HRMeters.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at Daily
            DYMeters.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at Monthly
            MNMeters.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // at RunPeriod/SM
            SMMeters.setRVariablesScanned(true);
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // at Yearly
            YRMeters.setRVariablesScanned(true);
            break;
        }
    }

    void ResultsFramework::writeOutputs(EnergyPlusData &state)
    {
        if (state.files.outputControl.csv) {
            writeCSVOutput(state);
        }

        if (timeSeriesEnabled() && (outputJSON || outputCBOR || outputMsgPack)) {
            writeTimeSeriesReports(state.files.json);
        }

        if (timeSeriesAndTabularEnabled() && (outputJSON || outputCBOR || outputMsgPack)) {
            writeReport(state.files.json);
        }
    }

    void ResultsFramework::writeCSVOutput(EnergyPlusData &state)
    {
        if (!hasOutputData()) {
            return;
        }
        CSVWriter csv(outputVariables.size());
        CSVWriter mtr_csv(outputVariables.size());

        // Output yearly time series data
        if (hasRIYearlyTSData()) {
            csv.parseTSOutputs(state, RIYearlyTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Yearly);
        }

        if (hasYRMeters()) {
            csv.parseTSOutputs(state, YRMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::Yearly);
            mtr_csv.parseTSOutputs(state, YRMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Yearly);
        }

        // Output run period time series data
        if (hasRIRunPeriodTSData()) {
            csv.parseTSOutputs(state, RIRunPeriodTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Simulation);
        }

        if (hasSMMeters()) {
            csv.parseTSOutputs(state, SMMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::Simulation);
            mtr_csv.parseTSOutputs(state, SMMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Simulation);
        }

        // Output monthly time series data
        if (hasRIMonthlyTSData()) {
            csv.parseTSOutputs(state, RIMonthlyTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Monthly);
        }

        if (hasMNMeters()) {
            csv.parseTSOutputs(state, MNMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::Monthly);
            mtr_csv.parseTSOutputs(state, MNMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Monthly);
        }

        // Output daily time series data
        if (hasRIDailyTSData()) {
            csv.parseTSOutputs(state, RIDailyTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Daily);
        }

        if (hasDYMeters()) {
            csv.parseTSOutputs(state, DYMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::Daily);
            mtr_csv.parseTSOutputs(state, DYMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Daily);
        }

        // Output hourly time series data
        if (hasRIHourlyTSData()) {
            csv.parseTSOutputs(state, RIHourlyTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Hourly);
        }

        if (hasHRMeters()) {
            csv.parseTSOutputs(state, HRMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::Hourly);
            mtr_csv.parseTSOutputs(state, HRMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::Hourly);
        }

        // Output timestep time series data
        if (hasRITimestepTSData()) {
            csv.parseTSOutputs(state, RITimestepTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::TimeStep);
        }

        if (hasTSMeters()) {
            csv.parseTSOutputs(state, TSMeters.getJSON(true), outputVariables, OutputProcessor::ReportingFrequency::TimeStep);
            mtr_csv.parseTSOutputs(state, TSMeters.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::TimeStep);
        }

        // Output detailed HVAC time series data
        if (hasRIDetailedHVACTSData()) {
            csv.parseTSOutputs(state, RIDetailedHVACTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::EachCall);
        }

        // Output detailed Zone time series data
        if (hasRIDetailedZoneTSData()) {
            csv.parseTSOutputs(state, RIDetailedZoneTSData.getJSON(), outputVariables, OutputProcessor::ReportingFrequency::EachCall);
        }

        csv.writeOutput(state, outputVariables, state.files.csv, state.files.outputControl.csv);
        if (hasMeterData()) {
            mtr_csv.writeOutput(state, outputVariables, state.files.mtr_csv, state.files.outputControl.csv);
        }
    }

    void ResultsFramework::writeTimeSeriesReports(JsonOutputStreams &jsonOutputStreams)
    {
        // Output detailed Zone time series data
        if (hasRIDetailedZoneTSData()) {
            RIDetailedZoneTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output detailed HVAC time series data
        if (hasRIDetailedHVACTSData()) {
            RIDetailedHVACTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output timestep time series data
        if (hasRITimestepTSData()) {
            RITimestepTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output hourly time series data
        if (hasRIHourlyTSData()) {
            RIHourlyTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output daily time series data
        if (hasRIDailyTSData()) {
            RIDailyTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output monthly time series data
        if (hasRIMonthlyTSData()) {
            RIMonthlyTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output run period time series data
        if (hasRIRunPeriodTSData()) {
            RIRunPeriodTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }

        // Output yearly time series data
        if (hasRIYearlyTSData()) {
            RIYearlyTSData.writeReport(jsonOutputStreams, outputJSON, outputCBOR, outputMsgPack);
        }
    }

    void ResultsFramework::writeReport(JsonOutputStreams &jsonOutputStreams)
    {
        json root, outputVars, rdd, meterVars, meterData;
        json rddvals = json::array();
        root = {{"SimulationResults", {{"Simulation", SimulationInformation.getJSON()}}}};

        // output variables
        if (hasRIDetailedZoneTSData()) {
            outputVars["Detailed-Zone"] = RIDetailedZoneTSData.getVariablesJSON();
        }

        if (hasRIDetailedHVACTSData()) {
            outputVars["Detailed-HVAC"] = RIDetailedHVACTSData.getVariablesJSON();
        }

        if (hasRITimestepTSData()) {
            outputVars["TimeStep"] = RITimestepTSData.getVariablesJSON();
        }

        if (hasRIHourlyTSData()) {
            outputVars["Hourly"] = RIHourlyTSData.getVariablesJSON();
        }

        if (hasRIDailyTSData()) {
            outputVars["Daily"], RIDailyTSData.getVariablesJSON();
        }

        if (hasRIMonthlyTSData()) {
            outputVars["Monthly"] = RIMonthlyTSData.getVariablesJSON();
        }

        if (hasRIRunPeriodTSData()) {
            outputVars["RunPeriod"] = RIRunPeriodTSData.getVariablesJSON();
        }

        if (hasRIYearlyTSData()) {
            outputVars["Yearly"] = RIYearlyTSData.getVariablesJSON();
        }

        // output dictionary
        for (size_t i = 0; i < RDD.size(); i++) {
            rddvals.push_back(RDD[i]);
        }
        rdd = {{"Description", "Dictionary containing output variables that may be requested"}, {"Variables", rddvals}};
        outputVars["OutputDictionary"] = rdd;

        // meter variables

        // -- meter values
        if (hasTSMeters()) {
            meterVars["TimeStep"] = TSMeters.getVariablesJSON();
        }

        if (hasHRMeters()) {
            meterVars["Hourly"] = HRMeters.getVariablesJSON();
        }

        if (hasDYMeters()) {
            meterVars["Daily"] = DYMeters.getVariablesJSON();
        }

        if (hasMNMeters()) {
            meterVars["Monthly"] = MNMeters.getVariablesJSON();
        }

        if (hasSMMeters()) {
            meterVars["RunPeriod"] = SMMeters.getVariablesJSON();
        }

        if (hasYRMeters()) {
            meterVars["Yearly"] = YRMeters.getVariablesJSON();
        }

        if (hasTSMeters()) {
            meterData["TimeStep"] = TSMeters.getJSON();
        }

        if (hasHRMeters()) {
            meterData["Hourly"] = HRMeters.getJSON();
        }

        if (hasDYMeters()) {
            meterData["Daily"] = DYMeters.getJSON();
        }

        if (hasMNMeters()) {
            meterData["Monthly"] = MNMeters.getJSON();
        }

        if (hasSMMeters()) {
            meterData["RunPeriod"] = SMMeters.getJSON();
        }

        if (hasYRMeters()) {
            meterData["Yearly"] = YRMeters.getJSON();
        }

        json mdd;
        json mddvals = json::array();

        // -- meter dictionary
        for (size_t i = 0; i < MDD.size(); i++) {
            mddvals.push_back(MDD[i]);
        }
        mdd = {{"Description", "Dictionary containing meter variables that may be requested"}, {"Meters", mddvals}};

        meterVars["MeterDictionary"] = mdd;

        root["OutputVariables"] = outputVars;
        root["MeterVariables"] = meterVars;
        root["MeterData"] = meterData;
        root["TabularReports"] = TabularReportsCollection.getJSON();

        if (outputJSON && jsonOutputStreams.json_stream) {
            auto const dumped_json = root.dump(4, ' ', false, json::error_handler_t::replace);
            std::copy(dumped_json.begin(), dumped_json.end(), std::ostream_iterator<uint8_t>(*jsonOutputStreams.json_stream));
        }
        if (outputCBOR && jsonOutputStreams.cbor_stream) {
            json::to_cbor(root, *jsonOutputStreams.cbor_stream);
            //            std::vector<uint8_t> v_cbor = json::to_cbor(root);
            //            std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*json.cbor_stream));
        }
        if (outputMsgPack && jsonOutputStreams.msgpack_stream) {
            json::to_msgpack(root, *jsonOutputStreams.msgpack_stream);
            //            std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
            //            std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*json.msgpack_stream));
        }
    }

    void ResultsFramework::addReportVariable(std::string const &keyedValue,
                                             std::string const &variableName,
                                             std::string const &units,
                                             OutputProcessor::ReportingFrequency const reportingInterval)
    {
        outputVariables.emplace_back(fmt::format("{0}:{1} [{2}]({3})", keyedValue, variableName, units, reportingFrequency(reportingInterval)));
    }

    void
    ResultsFramework::addReportMeter(std::string const &meter, std::string const &units, OutputProcessor::ReportingFrequency const reportingInterval)
    {
        outputVariables.emplace_back(fmt::format("{0} [{1}]({2})", meter, units, reportingFrequency(reportingInterval)));
    }

} // namespace ResultsFramework

} // namespace EnergyPlus
