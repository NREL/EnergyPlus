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
