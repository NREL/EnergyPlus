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
#include <DataGlobals.hh>
#include <EnergyPlus.hh>
#include <OutputProcessor.hh>

namespace EnergyPlus {

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
                 const int IndexType,
                 const int ReportID,
                 const OutputProcessor::Unit &units);

        std::string variableName() const;
        void setVariableName(const std::string &VarName);

        std::string sReportFrequency();
        OutputProcessor::ReportingFrequency iReportFrequency();
        void setReportFrequency(const OutputProcessor::ReportingFrequency reportFrequency);

        int indexType() const;
        void setIndexType(const int IndexType);

        int reportID() const;
        void setReportID(const int Id);

        OutputProcessor::Unit units() const;
        void setUnits(const OutputProcessor::Unit &units);

        void pushValue(const double val);
        double value(size_t index) const;
        size_t numValues() const;

        json getJSON() const;

    protected:
        std::string varName;
        std::string sReportFreq;
        OutputProcessor::Unit Units;
        OutputProcessor::ReportingFrequency iReportFreq = OutputProcessor::ReportingFrequency::EachCall;
        int idxType = -1;
        int rptID = -1;
        std::vector<double> Values;
    };

    class OutputVariable : public Variable
    {
    public:
        OutputVariable(const std::string &VarName,
                       const OutputProcessor::ReportingFrequency reportFrequency,
                       const int IndexType,
                       const int ReportID,
                       const OutputProcessor::Unit &units);
    };

    class MeterVariable : public Variable
    {
    public:
        MeterVariable(const std::string &VarName,
                      const OutputProcessor::ReportingFrequency reportFrequency,
                      const int ReportID,
                      const OutputProcessor::Unit &units,
                      const bool Acculumative = false);

        bool accumulative();
        void setAccumulative(bool state);

        json getJSON() const;

    protected:
        bool acc;
    };

    class DataFrame : public BaseResultObject
    {
    public:
        typedef std::pair<int, Variable> VarPtrPair;

        DataFrame(const std::string &ReportFreq);
        virtual ~DataFrame();

        void addVariable(Variable const &var);

        void setRDataFrameEnabled(bool state);
        void setIDataFrameEnabled(bool state);

        bool rDataFrameEnabled() const;
        bool iDataFrameEnabled() const;

        void setRVariablesScanned(bool state);
        void setIVariablesScanned(bool state);

        bool rVariablesScanned() const;
        bool iVariablesScanned() const;

        void newRow(const int month, const int dayOfMonth, const int hourOfDay, const int curMin);
        void newRow(const std::string &ts);
        void pushVariableValue(const int reportID, double value);

        Variable &lastVariable();

        json getVariablesJSON();
        json getJSON() const;

        void writeReport(bool outputJSON, bool outputCBOR, bool outputMsgPack);

    protected:
        bool IDataFrameEnabled = false;
        bool RDataFrameEnabled = false;
        bool RVariablesScanned = false;
        bool IVariablesScanned = false;
        std::string ReportFrequency;
        std::vector<std::string> TS;
        std::unordered_map<int, Variable> variableMap; // for O(1) lookup when adding to data structure
        int lastVarID;
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

    class ResultsSchema : public BaseResultObject
    {
    public:
        ResultsSchema() = default;
        virtual ~ResultsSchema() = default;

        void setupOutputOptions();

        bool timeSeriesEnabled() const;
        bool timeSeriesAndTabularEnabled() const;
        bool JSONEnabled() const;
        bool CBOREnabled() const;
        bool MsgPackEnabled() const;

        void initializeRTSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                    const Array1D<OutputProcessor::RealVariableType> &RVariableTypes,
                                    const int NumOfRVariable,
                                    const int IndexType = OutputProcessor::ZoneVar);
        void initializeITSDataFrame(const OutputProcessor::ReportingFrequency reportFrequency,
                                    const Array1D<OutputProcessor::IntegerVariableType> &IVariableTypes,
                                    const int NumOfIVariable,
                                    const int IndexType = OutputProcessor::ZoneVar);
        void initializeMeters(const Array1D<OutputProcessor::MeterType> &EnergyMeters, const OutputProcessor::ReportingFrequency reportFrequency);

        DataFrame RIDetailedZoneTSData = DataFrame("Detailed-Zone");
        DataFrame RIDetailedHVACTSData = DataFrame("Detailed-HVAC");
        DataFrame RITimestepTSData = DataFrame("Timestep");
        DataFrame RIHourlyTSData = DataFrame("Hourly");
        DataFrame RIDailyTSData = DataFrame("Daily");
        DataFrame RIMonthlyTSData = DataFrame("Monthly");
        DataFrame RIRunPeriodTSData = DataFrame("RunPeriod");
        DataFrame RIYearlyTSData = DataFrame("Yearly");
        DataFrame TSMeters = DataFrame("Timestep");
        DataFrame HRMeters = DataFrame("Hourly");
        DataFrame DYMeters = DataFrame("Daily");
        DataFrame MNMeters = DataFrame("Monthly");
        DataFrame SMMeters = DataFrame("RunPeriod");
        DataFrame YRMeters = DataFrame("Yearly");

        void writeTimeSeriesReports();
        void WriteReport();

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
    };

    extern std::unique_ptr<ResultsSchema> OutputSchema;

    void clear_state();
} // namespace ResultsFramework

} // namespace EnergyPlus

#endif
