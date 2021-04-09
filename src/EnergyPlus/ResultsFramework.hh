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

#ifndef ResultsSchema_hh_INCLUDED
#define ResultsSchema_hh_INCLUDED

#include <memory>
#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Reference.hh>

#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/OutputProcessor.hh>

namespace EnergyPlus {

// Forward declarations
class EnergyPlusFixture;
class ResultsFrameworkFixture;
struct EnergyPlusData;

namespace ResultsFramework {

    using json = nlohmann::json;

    // trim string
    std::string trim(std::string str);

    // base result object
    class BaseResultObject
    {
    public:
        BaseResultObject(){};
    };

    class SimInfo : public BaseResultObject
    {
    public:
        void setProgramVersion(const std::string &programVersion);
        std::string getProgramVersion();
        void setSimulationEnvironment(const std::string &simulationEnvironment);
        void setInputModelURI(const std::string &inputModelURI);
        void setStartDateTimeStamp(const std::string &startDateTimeStamp);
        void setRunTime(const std::string &elapsedTime);
        void setNumErrorsWarmup(const std::string &numWarningsDuringWarmup, const std::string &numSevereDuringWarmup);
        void setNumErrorsSizing(const std::string &numWarningsDuringSizing, const std::string &numSevereDuringSizing);
        void setNumErrorsSummary(const std::string &numWarnings, const std::string &numSevere);
        json getJSON() const;

    protected:
        std::string ProgramVersion;
        std::string SimulationEnvironment;
        std::string InputModelURI;
        std::string StartDateTimeStamp;
        std::string RunTime;
        std::string NumWarningsDuringWarmup, NumSevereDuringWarmup, NumWarningsDuringSizing, NumSevereDuringSizing, NumWarnings, NumSevere;
    };

    class Variable : public BaseResultObject
    {
    public:
        Variable() = default;
        Variable(const std::string &VarName,
                 const OutputProcessor::ReportingFrequency reportFrequency,
                 const OutputProcessor::TimeStepType timeStepType,
                 const int ReportID,
                 const OutputProcessor::Unit &units);
        Variable(const std::string &VarName,
                 const OutputProcessor::ReportingFrequency reportFrequency,
                 const OutputProcessor::TimeStepType timeStepType,
                 const int ReportID,
                 const OutputProcessor::Unit &units,
                 const std::string &customUnits);

        std::string variableName() const;
        void setVariableName(const std::string &VarName);

        std::string sReportFrequency() const;
        OutputProcessor::ReportingFrequency iReportFrequency() const;
        void setReportFrequency(const OutputProcessor::ReportingFrequency reportFrequency);

        OutputProcessor::TimeStepType timeStepType() const;
        void setTimeStepType(const OutputProcessor::TimeStepType timeStepType);

        int reportID() const;
        void setReportID(const int Id);

        OutputProcessor::Unit units() const;
        void setUnits(const OutputProcessor::Unit &units);

        std::string customUnits() const;
        void setCustomUnits(const std::string &customUnits);

        void pushValue(const double val);
        double value(size_t index) const;
        size_t numValues() const;

        virtual json getJSON() const;

    protected:
        std::string varName;
        std::string sReportFreq;
        OutputProcessor::ReportingFrequency iReportFreq = OutputProcessor::ReportingFrequency::EachCall;
        OutputProcessor::TimeStepType m_timeStepType = OutputProcessor::TimeStepType::TimeStepZone;
        int rptID = -1;
        OutputProcessor::Unit Units;
        std::string m_customUnits;
        std::vector<double> Values;
    };

    class OutputVariable : public Variable
    {
    public:
        OutputVariable(const std::string &VarName,
                       const OutputProcessor::ReportingFrequency reportFrequency,
                       const OutputProcessor::TimeStepType timeStepType,
                       const int ReportID,
                       const OutputProcessor::Unit &units);

        OutputVariable(const std::string &VarName,
                       const OutputProcessor::ReportingFrequency reportFrequency,
                       const OutputProcessor::TimeStepType timeStepType,
                       const int ReportID,
                       const OutputProcessor::Unit &units,
                       const std::string &customUnits);
    };

    class MeterVariable : public Variable
    {
    public:
        MeterVariable() = default;
        MeterVariable(const std::string &VarName,
                      const OutputProcessor::ReportingFrequency reportFrequency,
                      const int ReportID,
                      const OutputProcessor::Unit &units,
                      const bool MeterOnly,
                      const bool Acculumative = false);

        bool accumulative() const;
        void setAccumulative(bool state);
        bool meterOnly() const;
        void setMeterOnly(bool state);

        json getJSON() const override;

    protected:
        bool acc = false;
        bool meter_only = true;
    };

    class DataFrame : public BaseResultObject
    {
    public:
        typedef std::pair<int, Variable> VarPtrPair;

        explicit DataFrame(const std::string &ReportFreq);
        virtual ~DataFrame() = default;

        void addVariable(Variable const &var);

        void setRDataFrameEnabled(bool state);
        void setIDataFrameEnabled(bool state);

        bool rDataFrameEnabled() const;
        bool iDataFrameEnabled() const;

        void setRVariablesScanned(bool state);
        void setIVariablesScanned(bool state);

        bool rVariablesScanned() const;
        bool iVariablesScanned() const;

        void newRow(EnergyPlusData &state, const int month, const int dayOfMonth, int hourOfDay, int curMin);
        //        void newRow(const std::string &ts);
        virtual void pushVariableValue(const int reportID, double value);

        Variable &lastVariable();

        json getVariablesJSON();
        json getJSON() const;

        void writeReport(JsonOutputStreams &jsonOutputStreams, bool outputJSON, bool outputCBOR, bool outputMsgPack);

    protected:
        bool IDataFrameEnabled = false;
        bool RDataFrameEnabled = false;
        bool RVariablesScanned = false;
        bool IVariablesScanned = false;
        std::string ReportFrequency;
        std::vector<std::string> TS;
        std::map<int, Variable> variableMap;
        int lastVarID = -1;
    };

    class MeterDataFrame : public DataFrame
    {
    public:
        explicit MeterDataFrame(const std::string &ReportFreq) : DataFrame(ReportFreq){};
        virtual ~MeterDataFrame() = default;

        void addVariable(MeterVariable const &var);

        void pushVariableValue(const int reportID, double value) override;

        json getJSON(bool meterOnlyCheck = false) const;

    protected:
        std::map<int, MeterVariable> meterMap;
    };

    class Table : public BaseResultObject
    {
    public:
        std::string TableName;
        std::string FootnoteText;
        std::vector<std::string> ColHeaders;
        std::vector<std::string> RowHeaders;
        std::vector<std::vector<std::string>> Data;

        Table(Array2D_string const &body,
              Array1D_string const &rowLabels,
              Array1D_string const &columnLabels,
              std::string const &tableName,
              std::string const &footnoteText);

        json getJSON() const;
    };

    class Report : public BaseResultObject
    {
    public:
        std::string ReportName;
        std::string ReportForString;
        std::vector<Table> Tables;

        json getJSON() const;
    };

    class ReportsCollection : public BaseResultObject
    {
    public:
        typedef std::pair<std::string, Report> RptPtrPair;

        ReportsCollection();

        void addReportTable(Array2D_string const &body,
                            Array1D_string const &rowLabels,
                            Array1D_string const &columnLabels,
                            std::string const &reportName,
                            std::string const &reportForString,
                            std::string const &tableName);

        void addReportTable(Array2D_string const &body,
                            Array1D_string const &rowLabels,
                            Array1D_string const &columnLabels,
                            std::string const &reportName,
                            std::string const &reportForString,
                            std::string const &tableName,
                            std::string const &footnoteText);

        json getJSON() const;

    protected:
        std::unordered_map<std::string, Report> reportsMap;
        Report rpt;
    };

    class CSVWriter : public BaseResultObject
    {
    public:
        CSVWriter() = default;
        explicit CSVWriter(std::size_t num_output_variables)
        {
            outputVariableIndices = std::vector<bool>(num_output_variables, false);
        }

        void writeOutput(EnergyPlusData &state, std::vector<std::string> const &outputVariables, InputOutputFile &outputFile, bool outputControl);
        void parseTSOutputs(EnergyPlusData &state,
                            json const &data,
                            std::vector<std::string> const &outputVariables,
                            OutputProcessor::ReportingFrequency reportingFrequency);

    private:
        friend class EnergyPlus::EnergyPlusFixture;
        friend class EnergyPlus::ResultsFrameworkFixture;

        char s[129] = {0};
        OutputProcessor::ReportingFrequency smallestReportingFrequency = OutputProcessor::ReportingFrequency::Yearly;
        std::map<std::string, std::vector<std::string>> outputs;
        std::vector<bool> outputVariableIndices;

        static std::string &convertToMonth(EnergyPlusData &state, std::string &datetime);
        void updateReportingFrequency(OutputProcessor::ReportingFrequency reportingFrequency);
        // void readRVI();
        // void readMVI();
    };

    class ResultsFramework : public BaseResultObject
    {
    public:
        ResultsFramework() = default;

        virtual ~ResultsFramework() = default;

        void setupOutputOptions(EnergyPlusData &state);

        bool timeSeriesEnabled() const;

        bool timeSeriesAndTabularEnabled() const;

        bool JSONEnabled() const;

        bool CBOREnabled() const;

        bool MsgPackEnabled() const;

        void initializeRTSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                    const Array1D<OutputProcessor::RealVariableType> &RVariableTypes,
                                    const int NumOfRVariable,
                                    const OutputProcessor::TimeStepType timeStepType = OutputProcessor::TimeStepType::TimeStepZone);

        void initializeITSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                    const Array1D<OutputProcessor::IntegerVariableType> &IVariableTypes,
                                    const int NumOfIVariable,
                                    const OutputProcessor::TimeStepType timeStepType = OutputProcessor::TimeStepType::TimeStepZone);

        void initializeMeters(const Array1D<OutputProcessor::MeterType> &EnergyMeters, const OutputProcessor::ReportingFrequency reportFrequency);

        DataFrame RIDetailedZoneTSData = DataFrame("Detailed-Zone");
        DataFrame RIDetailedHVACTSData = DataFrame("Detailed-HVAC");
        DataFrame RITimestepTSData = DataFrame("TimeStep");
        DataFrame RIHourlyTSData = DataFrame("Hourly");
        DataFrame RIDailyTSData = DataFrame("Daily");
        DataFrame RIMonthlyTSData = DataFrame("Monthly");
        DataFrame RIRunPeriodTSData = DataFrame("RunPeriod");
        DataFrame RIYearlyTSData = DataFrame("Yearly");
        MeterDataFrame TSMeters = MeterDataFrame("TimeStep");
        MeterDataFrame HRMeters = MeterDataFrame("Hourly");
        MeterDataFrame DYMeters = MeterDataFrame("Daily");
        MeterDataFrame MNMeters = MeterDataFrame("Monthly");
        MeterDataFrame SMMeters = MeterDataFrame("RunPeriod");
        MeterDataFrame YRMeters = MeterDataFrame("Yearly");

        void writeOutputs(EnergyPlusData &state);

        void addReportVariable(std::string const &keyedValue,
                               std::string const &variableName,
                               std::string const &units,
                               OutputProcessor::ReportingFrequency const reportingInterval);

        void addReportMeter(std::string const &meter, std::string const &units, OutputProcessor::ReportingFrequency const reportingInterval);

        SimInfo SimulationInformation;

        std::vector<std::string> MDD;
        std::vector<std::string> RDD;
        ReportsCollection TabularReportsCollection;

    protected:
        bool tsEnabled = false;
        bool tsAndTabularEnabled = false;
        bool outputJSON = false;
        bool outputCBOR = false;
        bool outputMsgPack = false;
        std::vector<std::string> outputVariables;

        void writeTimeSeriesReports(JsonOutputStreams &jsonOutputStreams);

        void writeReport(JsonOutputStreams &jsonOutputStreams);

        void writeCSVOutput(EnergyPlusData &state);

    private:
        friend class EnergyPlus::EnergyPlusFixture;
        friend class EnergyPlus::ResultsFrameworkFixture;

    protected:
        inline bool hasRIDetailedZoneTSData()
        {
            return RIDetailedZoneTSData.iDataFrameEnabled() || RIDetailedZoneTSData.rDataFrameEnabled();
        };

        inline bool hasRIDetailedHVACTSData()
        {
            return RIDetailedHVACTSData.iDataFrameEnabled() || RIDetailedHVACTSData.rDataFrameEnabled();
        };

        inline bool hasRITimestepTSData()
        {
            return RITimestepTSData.iDataFrameEnabled() || RITimestepTSData.rDataFrameEnabled();
        };

        inline bool hasRIHourlyTSData()
        {
            return RIHourlyTSData.iDataFrameEnabled() || RIHourlyTSData.rDataFrameEnabled();
        };

        inline bool hasRIDailyTSData()
        {
            return RIDailyTSData.iDataFrameEnabled() || RIDailyTSData.rDataFrameEnabled();
        };

        inline bool hasRIMonthlyTSData()
        {
            return RIMonthlyTSData.iDataFrameEnabled() || RIMonthlyTSData.rDataFrameEnabled();
        };

        inline bool hasRIRunPeriodTSData()
        {
            return RIRunPeriodTSData.iDataFrameEnabled() || RIRunPeriodTSData.rDataFrameEnabled();
        };

        inline bool hasRIYearlyTSData()
        {
            return RIYearlyTSData.iDataFrameEnabled() || RIYearlyTSData.rDataFrameEnabled();
        };

        inline bool hasTSMeters()
        {
            return TSMeters.rDataFrameEnabled();
        };

        inline bool hasHRMeters()
        {
            return HRMeters.rDataFrameEnabled();
        };

        inline bool hasDYMeters()
        {
            return DYMeters.rDataFrameEnabled();
        };

        inline bool hasMNMeters()
        {
            return MNMeters.rDataFrameEnabled();
        };

        inline bool hasSMMeters()
        {
            return SMMeters.rDataFrameEnabled();
        };

        inline bool hasYRMeters()
        {
            return YRMeters.rDataFrameEnabled();
        };

        inline bool hasMeterData()
        {
            return hasTSMeters() || hasHRMeters() || hasDYMeters() || hasMNMeters() || hasSMMeters() || hasYRMeters();
        };

        inline bool hasTSData()
        {
            return hasRIDetailedZoneTSData() || hasRIDetailedHVACTSData() || hasRITimestepTSData() || hasRIHourlyTSData() || hasRIDailyTSData() ||
                   hasRIMonthlyTSData() || hasRIRunPeriodTSData() || hasRIYearlyTSData();
        };

        inline bool hasOutputData()
        {
            return hasTSData() || hasMeterData();
        };
    };

} // namespace ResultsFramework

struct ResultsFrameworkData : BaseGlobalStruct
{

    std::unique_ptr<ResultsFramework::ResultsFramework> resultsFramework = std::make_unique<ResultsFramework::ResultsFramework>();

    void clear_state() override
    {
        this->resultsFramework->DYMeters.setRDataFrameEnabled(false);
        this->resultsFramework->DYMeters.setRVariablesScanned(false);
        this->resultsFramework->DYMeters.setIVariablesScanned(false);
        this->resultsFramework->DYMeters.setIDataFrameEnabled(false);

        this->resultsFramework->TSMeters.setRVariablesScanned(false);
        this->resultsFramework->TSMeters.setRDataFrameEnabled(false);
        this->resultsFramework->TSMeters.setIDataFrameEnabled(false);
        this->resultsFramework->TSMeters.setIVariablesScanned(false);

        this->resultsFramework->HRMeters.setRVariablesScanned(false);
        this->resultsFramework->HRMeters.setRDataFrameEnabled(false);
        this->resultsFramework->HRMeters.setIDataFrameEnabled(false);
        this->resultsFramework->HRMeters.setIVariablesScanned(false);

        this->resultsFramework->MNMeters.setRVariablesScanned(false);
        this->resultsFramework->MNMeters.setRDataFrameEnabled(false);
        this->resultsFramework->MNMeters.setIDataFrameEnabled(false);
        this->resultsFramework->MNMeters.setIVariablesScanned(false);

        this->resultsFramework->SMMeters.setRVariablesScanned(false);
        this->resultsFramework->SMMeters.setRDataFrameEnabled(false);
        this->resultsFramework->SMMeters.setIDataFrameEnabled(false);
        this->resultsFramework->SMMeters.setIVariablesScanned(false);

        this->resultsFramework->YRMeters.setRVariablesScanned(false);
        this->resultsFramework->YRMeters.setRDataFrameEnabled(false);
        this->resultsFramework->YRMeters.setIDataFrameEnabled(false);
        this->resultsFramework->YRMeters.setIVariablesScanned(false);
    }
};

} // namespace EnergyPlus

#endif
