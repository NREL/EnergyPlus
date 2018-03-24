// C++ Headers
#include <cmath>
#include <fstream>
#include <iostream>
#include <random>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Reference.fwd.hh>
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include "DataStringGlobals.hh"
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <ResultsSchema.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ResultsFramework {

    using namespace DataHVACGlobals;
    using namespace DataPrecisionGlobals;
    using namespace OutputProcessor;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::InitConvTemp;
    using DataGlobals::SecInHour;
    using General::RoundSigDigits;
    using General::TrimSigDigits;
    using OutputProcessor::RealVariableType;
    using OutputProcessor::RealVariables;

    static gio::Fmt fmtLD("*");

    std::unique_ptr<ResultsSchema> OutputSchema(new ResultsSchema);

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
                       const int IndexType,
                       const int ReportID,
                       const OutputProcessor::Unit &units)
    {
        varName = VarName;
        setReportFrequency(reportFrequency);
        idxType = IndexType;
        rptID = ReportID;
        Units = units;
    }

    std::string Variable::variableName() const
    {
        return varName;
    }

    void Variable::setVariableName(const std::string &VarName)
    {
        varName = VarName;
    }

    std::string Variable::sReportFrequency()
    {
        return sReportFreq;
    }

    OutputProcessor::ReportingFrequency Variable::iReportFrequency()
    {
        return iReportFreq;
    }

    void Variable::setReportFrequency(const OutputProcessor::ReportingFrequency reportFrequency)
    {
        iReportFreq = reportFrequency;
        switch (iReportFreq) {
        case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
            if (idxType == ZoneVar) sReportFreq = "Detailed - Zone";
            if (idxType == HVACVar) sReportFreq = "Detailed - HVAC";
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'EndTimeStepFlag'
            sReportFreq = "Timestep";
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

    int Variable::indexType() const
    {
        return idxType;
    }

    void Variable::setIndexType(int IndexType)
    {
        idxType = IndexType;
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
        json root = {{"Name", varName}, {"Units", unitEnumToString(Units)}, {"Frequency", sReportFreq}};
        return root;
    }

    // Class OutputVariable
    OutputVariable::OutputVariable(const std::string &VarName,
                                   const OutputProcessor::ReportingFrequency reportFrequency,
                                   const int IndexType,
                                   const int ReportID,
                                   const OutputProcessor::Unit &units)
        : Variable(VarName, reportFrequency, IndexType, ReportID, units)
    {
    }

    // Class MeterVariable
    MeterVariable::MeterVariable(const std::string &VarName,
                                 const OutputProcessor::ReportingFrequency reportFrequency,
                                 const int ReportID,
                                 const OutputProcessor::Unit &units,
                                 const bool Accumulative)
        : Variable(VarName, reportFrequency, ZoneVar, ReportID, units)
    {
        acc = Accumulative;
    }

    bool MeterVariable::accumulative()
    {
        return acc;
    }

    void MeterVariable::setAccumulative(bool state)
    {
        acc = state;
    }

    json MeterVariable::getJSON() const
    {
        json root = Variable::getJSON();
        if (acc) {
            root["Cumulative"] = true;
        }
        return root;
    }

    // class DataFrame
    DataFrame::DataFrame(const std::string &ReportFreq)
    {
        ReportFrequency = ReportFreq;
    }

    DataFrame::~DataFrame()
    {
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

    void DataFrame::newRow(const int month, const int dayOfMonth, const int hourOfDay, const int curMin)
    {
        std::string ts =
            std::to_string(month) + "/" + std::to_string(dayOfMonth) + " " + std::to_string(hourOfDay) + ":" + std::to_string(curMin) + ":00";
        TS.push_back(ts);
    }

    void DataFrame::newRow(const std::string &ts)
    {
        TS.push_back(ts);
    }

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
            cols.push_back({{"Variable", varMap.second.variableName()}, {"Units", unitEnumToString(varMap.second.units())}});
        }

        std::vector<double> vals;
        vals.reserve(10000);

        // if DataFrame is enabled and control reaches here, there must be at least one o/p variable
        assert(TS.size() == variableMap.begin()->second.numValues());

        for (size_t row = 0; row < TS.size(); ++row) {
            vals.clear();

            for (auto const &varMap : variableMap) {
                vals.push_back(varMap.second.value(row));
            }

            rows.push_back({{TS.at(row), vals}});
        }
        root = {{"ReportFrequency", ReportFrequency}, {"Cols", cols}, {"Rows", rows}};
        return root;
    }

    void DataFrame::writeReport(bool outputJSON, bool outputCBOR, bool outputMsgPack)
    {

        json root = getJSON();
        if (ReportFrequency == "Detailed-HVAC") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_TSstream_HVAC) {
                *(DataGlobals::jsonOutputStreams.json_TSstream_HVAC) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_TSstream_HVAC) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_TSstream_HVAC));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_TSstream_HVAC) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_TSstream_HVAC));
            }
        } else if (ReportFrequency == "Detailed-Zone") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_TSstream_Zone) {
                *(DataGlobals::jsonOutputStreams.json_TSstream_Zone) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_TSstream_Zone) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_TSstream_Zone));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_TSstream_Zone) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_TSstream_Zone));
            }
        } else if (ReportFrequency == "Timestep") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_TSstream) {
                *(DataGlobals::jsonOutputStreams.json_TSstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_TSstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_TSstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_TSstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_TSstream));
            }
        } else if (ReportFrequency == "Daily") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_DYstream) {
                *(DataGlobals::jsonOutputStreams.json_DYstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_DYstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_DYstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_DYstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_DYstream));
            }
        } else if (ReportFrequency == "Hourly") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_HRstream) {
                *(DataGlobals::jsonOutputStreams.json_HRstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_HRstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_HRstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_HRstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_HRstream));
            }
        } else if (ReportFrequency == "Monthly") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_MNstream) {
                *(DataGlobals::jsonOutputStreams.json_MNstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_MNstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_MNstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_MNstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_MNstream));
            }
        } else if (ReportFrequency == "RunPeriod") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_SMstream) {
                *(DataGlobals::jsonOutputStreams.json_SMstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_SMstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_SMstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_SMstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_SMstream));
            }
        } else if (ReportFrequency == "Yearly") {
            if (outputJSON && DataGlobals::jsonOutputStreams.json_YRstream) {
                *(DataGlobals::jsonOutputStreams.json_YRstream) << std::setw(4) << root << std::endl;
            }
            if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_YRstream) {
                std::vector<uint8_t> v_cbor = json::to_cbor(root);
                std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_YRstream));
            }
            if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_YRstream) {
                std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
                std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_YRstream));
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
                auto found = body[k].find("\xC2\xB0");
                if (found != std::string::npos) {
                    std::string testing = body[k];
                    std::string test = "";
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

    void ResultsSchema::setupOutputOptions()
    {
        int numberOfOutputSchemaObjects = inputProcessor->getNumObjectsFound("Output:JSON");
        if (numberOfOutputSchemaObjects == 1) {
            Array1D_string alphas(5);
            int numAlphas;
            Array1D<Real64> numbers(2);
            int numNumbers;
            int status;
            inputProcessor->getObjectItem("Output:JSON", 1, alphas, numAlphas, numbers, numNumbers, status);

            if (numAlphas > 0) {
                std::string option = alphas(1);
                if (UtilityRoutines::SameString(option, "TimeSeries")) {
                    tsEnabled = true;
                } else if (UtilityRoutines::SameString(option, "TimeSeriesAndTabular")) {
                    tsEnabled = true;
                    tsAndTabularEnabled = true;
                }

                if (numAlphas >= 2) {
                    if (UtilityRoutines::SameString(alphas(2), "Yes")) {
                        outputJSON = true;
                    }
                }

                if (numAlphas >= 3) {
                    if (UtilityRoutines::SameString(alphas(3), "Yes")) {
                        outputCBOR = true;
                    }
                }

                if (numAlphas >= 4) {
                    if (UtilityRoutines::SameString(alphas(4), "Yes")) {
                        outputMsgPack = true;
                    }
                }
            }
        }
    }

    bool ResultsSchema::timeSeriesEnabled() const
    {
        return tsEnabled;
    }

    bool ResultsSchema::timeSeriesAndTabularEnabled() const
    {
        return tsAndTabularEnabled;
    }

    bool ResultsSchema::JSONEnabled() const
    {
        return outputJSON;
    }

    bool ResultsSchema::CBOREnabled() const
    {
        return outputCBOR;
    }

    bool ResultsSchema::MsgPackEnabled() const
    {
        return outputMsgPack;
    }

    void ResultsSchema::initializeRTSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                               const Array1D<RealVariableType> &RVariableTypes,
                                               const int NumOfRVariable,
                                               const int IndexType)
    {
        Reference<RealVariables> RVar;

        for (int Loop = 1; Loop <= NumOfRVariable; ++Loop) {
            RVar >>= RVariableTypes(Loop).VarPtr;
            auto &rVar(RVar());
            if (rVar.Report && rVar.frequency == reportFrequency) {
                // Variable *var = new Variable( RVariableTypes(Loop ).VarName,
                // 		reportFrequency, RVariableTypes( Loop ).IndexType,
                // 		RVariableTypes( Loop ).ReportID,
                // 		RVariableTypes( Loop ).units);
                Variable var(RVariableTypes(Loop).VarName, reportFrequency, RVariableTypes(Loop).IndexType, RVariableTypes(Loop).ReportID,
                             RVariableTypes(Loop).units);
                switch (reportFrequency) {
                case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
                    if (IndexType == ZoneVar && RVariableTypes(Loop).IndexType == ZoneVar) {
                        RIDetailedZoneTSData.setRDataFrameEnabled(true);
                        RIDetailedZoneTSData.addVariable(var);
                    } else if (IndexType == HVACVar && RVariableTypes(Loop).IndexType == HVACVar) {
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
            if (IndexType == ZoneVar) RIDetailedZoneTSData.setRVariablesScanned(true);
            if (IndexType == HVACVar) RIDetailedHVACTSData.setRVariablesScanned(true);
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

    void ResultsSchema::initializeITSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                               const Array1D<IntegerVariableType> &IVariableTypes,
                                               const int NumOfIVariable,
                                               const int IndexType)
    {
        Reference<IntegerVariables> IVar;

        // loop over values to suck in var info
        for (int Loop = 1; Loop <= NumOfIVariable; ++Loop) {
            IVar >>= IVariableTypes(Loop).VarPtr;
            auto &iVar(IVar());
            if (iVar.Report && iVar.frequency == reportFrequency) {
                // OutputVariable *var = new OutputVariable( IVariableTypes(Loop ).VarName, reportFrequency,
                // 			IVariableTypes( Loop ).IndexType,
                // 			IVariableTypes( Loop ).ReportID,
                // 			IVariableTypes( Loop ).units);
                OutputVariable var(IVariableTypes(Loop).VarName, reportFrequency, IVariableTypes(Loop).IndexType, IVariableTypes(Loop).ReportID,
                                   IVariableTypes(Loop).units);
                switch (reportFrequency) {
                case OutputProcessor::ReportingFrequency::EachCall: // each time UpdatedataandReport is called
                    if (IndexType == ZoneVar && IVariableTypes(Loop).IndexType == ZoneVar) {
                        RIDetailedZoneTSData.setIDataFrameEnabled(true);
                        RIDetailedZoneTSData.addVariable(var);
                    } else if (IndexType == HVACVar && IVariableTypes(Loop).IndexType == HVACVar) {
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
            if (IndexType == ZoneVar) RIDetailedZoneTSData.setIVariablesScanned(true);
            if (IndexType == HVACVar) RIDetailedHVACTSData.setIVariablesScanned(true);
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

    void ResultsSchema::initializeMeters(const Array1D<OutputProcessor::MeterType> &EnergyMeters,
                                         const OutputProcessor::ReportingFrequency reportFrequency)
    {
        switch (reportFrequency) {
        case OutputProcessor::ReportingFrequency::EachCall:
            // nothing to do; meters are not reported at this frequency
            break;
        case OutputProcessor::ReportingFrequency::TimeStep: // at 'Timestep'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptTS || EnergyMeters(Loop).RptTSFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).TSRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).TSRptNum, EnergyMeters(Loop).Units);
                    TSMeters.addVariable(var);
                    TSMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccTS || EnergyMeters(Loop).RptAccTSFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).TSAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).TSAccRptNum, EnergyMeters(Loop).Units);
                    TSMeters.addVariable(var);
                    TSMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Hourly: // at 'Hourly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptHR || EnergyMeters(Loop).RptHRFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).HRRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).HRRptNum, EnergyMeters(Loop).Units);
                    HRMeters.addVariable(var);
                    HRMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccHR || EnergyMeters(Loop).RptAccHRFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).HRAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).HRAccRptNum, EnergyMeters(Loop).Units);
                    HRMeters.addVariable(var);
                    HRMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Daily: // at 'Daily'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptDY || EnergyMeters(Loop).RptDYFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).DYRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).DYRptNum, EnergyMeters(Loop).Units);
                    DYMeters.addVariable(var);
                    DYMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccDY || EnergyMeters(Loop).RptAccDYFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).DYAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).DYAccRptNum, EnergyMeters(Loop).Units);
                    DYMeters.addVariable(var);
                    DYMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Monthly: // at 'Monthly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptMN || EnergyMeters(Loop).RptMNFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).MNRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).MNRptNum, EnergyMeters(Loop).Units);
                    MNMeters.addVariable(var);
                    MNMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccMN || EnergyMeters(Loop).RptAccMNFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).MNAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).MNAccRptNum, EnergyMeters(Loop).Units);
                    MNMeters.addVariable(var);
                    MNMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Simulation: // at 'RunPeriod'/'SM'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptSM || EnergyMeters(Loop).RptSMFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).SMRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).SMRptNum, EnergyMeters(Loop).Units);
                    SMMeters.addVariable(var);
                    SMMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccSM || EnergyMeters(Loop).RptAccSMFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).SMAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).SMAccRptNum, EnergyMeters(Loop).Units);
                    SMMeters.addVariable(var);
                    SMMeters.setRDataFrameEnabled(true);
                }
            }
            break;
        case OutputProcessor::ReportingFrequency::Yearly: // at 'Yearly'
            for (size_t Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
                if (EnergyMeters(Loop).RptYR || EnergyMeters(Loop).RptYRFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).YRRptNum, EnergyMeters(
                    // Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).YRRptNum, EnergyMeters(Loop).Units);
                    YRMeters.addVariable(var);
                    YRMeters.setRDataFrameEnabled(true);
                }
                if (EnergyMeters(Loop).RptAccYR || EnergyMeters(Loop).RptAccYRFO) {
                    // MeterVariable *var = new MeterVariable( EnergyMeters(Loop ).Name, reportFrequency, EnergyMeters( Loop ).YRAccRptNum,
                    // EnergyMeters( Loop ).Units);
                    MeterVariable var(EnergyMeters(Loop).Name, reportFrequency, EnergyMeters(Loop).YRAccRptNum, EnergyMeters(Loop).Units);
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

    void ResultsSchema::writeTimeSeriesReports()
    {
        // Output detailed Zone time series data
        if (OutputSchema->RIDetailedZoneTSData.rDataFrameEnabled() || OutputSchema->RIDetailedZoneTSData.iDataFrameEnabled()) {
            OutputSchema->RIDetailedZoneTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output detailed HVAC time series data
        if (OutputSchema->RIDetailedHVACTSData.iDataFrameEnabled() || OutputSchema->RIDetailedHVACTSData.rDataFrameEnabled()) {
            OutputSchema->RIDetailedHVACTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output timestep time series data
        if (OutputSchema->RITimestepTSData.iDataFrameEnabled() || OutputSchema->RITimestepTSData.rDataFrameEnabled()) {
            OutputSchema->RITimestepTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output hourly time series data
        if (OutputSchema->RIHourlyTSData.iDataFrameEnabled() || OutputSchema->RIHourlyTSData.rDataFrameEnabled()) {
            OutputSchema->RIHourlyTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output daily time series data
        if (OutputSchema->RIDailyTSData.iDataFrameEnabled() || OutputSchema->RIDailyTSData.rDataFrameEnabled()) {
            OutputSchema->RIDailyTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output monthly time series data
        if (OutputSchema->RIMonthlyTSData.iDataFrameEnabled() || OutputSchema->RIMonthlyTSData.rDataFrameEnabled()) {
            OutputSchema->RIMonthlyTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output run period time series data
        if (OutputSchema->RIRunPeriodTSData.iDataFrameEnabled() || OutputSchema->RIRunPeriodTSData.rDataFrameEnabled()) {
            OutputSchema->RIRunPeriodTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }

        // Output yearly time series data
        if (OutputSchema->RIYearlyTSData.iDataFrameEnabled() || OutputSchema->RIYearlyTSData.rDataFrameEnabled()) {
            OutputSchema->RIYearlyTSData.writeReport(outputJSON, outputCBOR, outputMsgPack);
        }
    }

    void ResultsSchema::WriteReport()
    {
        json root, outputVars, rdd, meterVars, meterData;
        json rddvals = json::array();
        root = {{"SimulationResults", {{"Simulation", SimulationInformation.getJSON()}}}};

        // output variables
        if (RIDetailedZoneTSData.iDataFrameEnabled() || RIDetailedZoneTSData.rDataFrameEnabled()) {
            outputVars["Detailed-Zone"] = RIDetailedZoneTSData.getVariablesJSON();
        }

        if (RIDetailedHVACTSData.iDataFrameEnabled() || RIDetailedHVACTSData.rDataFrameEnabled()) {
            outputVars["Detailed-HVAC"] = RIDetailedHVACTSData.getVariablesJSON();
        }

        if (RITimestepTSData.iDataFrameEnabled() || RITimestepTSData.rDataFrameEnabled()) {
            outputVars["Timestep"] = RITimestepTSData.getVariablesJSON();
        }

        if (RIHourlyTSData.iDataFrameEnabled() || RIHourlyTSData.rDataFrameEnabled()) {
            outputVars["Hourly"] = RIHourlyTSData.getVariablesJSON();
        }

        if (RIDailyTSData.iDataFrameEnabled() || RIDailyTSData.rDataFrameEnabled()) {
            outputVars["Daily"], RIDailyTSData.getVariablesJSON();
        }

        if (RIMonthlyTSData.iDataFrameEnabled() || RIMonthlyTSData.rDataFrameEnabled()) {
            outputVars["Monthly"] = RIMonthlyTSData.getVariablesJSON();
        }

        if (RIRunPeriodTSData.iDataFrameEnabled() || RIRunPeriodTSData.rDataFrameEnabled()) {
            outputVars["RunPeriod"] = RIRunPeriodTSData.getVariablesJSON();
        }

        if (RIYearlyTSData.iDataFrameEnabled() || RIYearlyTSData.rDataFrameEnabled()) {
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
        if (TSMeters.rDataFrameEnabled()) {
            meterVars["Timestep"] = TSMeters.getVariablesJSON();
        }

        if (HRMeters.rDataFrameEnabled()) {
            meterVars["Hourly"] = HRMeters.getVariablesJSON();
        }

        if (DYMeters.rDataFrameEnabled()) {
            meterVars["Daily"] = DYMeters.getVariablesJSON();
        }

        if (MNMeters.rDataFrameEnabled()) {
            meterVars["Monthly"] = MNMeters.getVariablesJSON();
        }

        if (SMMeters.rDataFrameEnabled()) {
            meterVars["RunPeriod"] = SMMeters.getVariablesJSON();
        }

        if (TSMeters.rDataFrameEnabled()) {
            meterData["Timestep"] = TSMeters.getJSON();
        }

        if (HRMeters.rDataFrameEnabled()) {
            meterData["Hourly"] = HRMeters.getJSON();
        }

        if (DYMeters.rDataFrameEnabled()) {
            meterData["Daily"] = DYMeters.getJSON();
        }

        if (MNMeters.rDataFrameEnabled()) {
            meterData["Monthly"] = MNMeters.getJSON();
        }

        if (SMMeters.rDataFrameEnabled()) {
            meterData["RunPeriod"] = SMMeters.getJSON();
        }

        if (YRMeters.rDataFrameEnabled()) {
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

        if (outputJSON && DataGlobals::jsonOutputStreams.json_stream) {
            *(DataGlobals::jsonOutputStreams.json_stream) << std::setw(4) << root << std::endl;
        }
        if (outputCBOR && DataGlobals::jsonOutputStreams.cbor_stream) {
            std::vector<uint8_t> v_cbor = json::to_cbor(root);
            std::copy(v_cbor.begin(), v_cbor.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.cbor_stream));
        }
        if (outputMsgPack && DataGlobals::jsonOutputStreams.msgpack_stream) {
            std::vector<uint8_t> v_msgpack = json::to_msgpack(root);
            std::copy(v_msgpack.begin(), v_msgpack.end(), std::ostream_iterator<uint8_t>(*DataGlobals::jsonOutputStreams.msgpack_stream));
        }
    }

    void clear_state()
    {
        OutputSchema->DYMeters.setRDataFrameEnabled(false);
        OutputSchema->DYMeters.setRVariablesScanned(false);
        OutputSchema->DYMeters.setIVariablesScanned(false);
        OutputSchema->DYMeters.setIDataFrameEnabled(false);

        OutputSchema->TSMeters.setRVariablesScanned(false);
        OutputSchema->TSMeters.setRDataFrameEnabled(false);
        OutputSchema->TSMeters.setIDataFrameEnabled(false);
        OutputSchema->TSMeters.setIVariablesScanned(false);

        OutputSchema->HRMeters.setRVariablesScanned(false);
        OutputSchema->HRMeters.setRDataFrameEnabled(false);
        OutputSchema->HRMeters.setIDataFrameEnabled(false);
        OutputSchema->HRMeters.setIVariablesScanned(false);

        OutputSchema->MNMeters.setRVariablesScanned(false);
        OutputSchema->MNMeters.setRDataFrameEnabled(false);
        OutputSchema->MNMeters.setIDataFrameEnabled(false);
        OutputSchema->MNMeters.setIVariablesScanned(false);

        OutputSchema->SMMeters.setRVariablesScanned(false);
        OutputSchema->SMMeters.setRDataFrameEnabled(false);
        OutputSchema->SMMeters.setIDataFrameEnabled(false);
        OutputSchema->SMMeters.setIVariablesScanned(false);

        OutputSchema->YRMeters.setRVariablesScanned(false);
        OutputSchema->YRMeters.setRDataFrameEnabled(false);
        OutputSchema->YRMeters.setIDataFrameEnabled(false);
        OutputSchema->YRMeters.setIVariablesScanned(false);
    }
} // namespace ResultsFramework

} // namespace EnergyPlus
