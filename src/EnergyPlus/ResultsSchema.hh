#ifndef ResultsSchema_hh_INCLUDED
#define ResultsSchema_hh_INCLUDED

#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Reference.hh>

#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <OutputProcessor.hh>

#include <memory>
using json = nlohmann::json;
namespace EnergyPlus {

	namespace ResultsFramework {

		// getUUID copied from Shannon's code; to be replaced with the right UUID API calls
		std::string getUUID();
		int randomInRange(int min, int max);

		// trim string
		std::string trim(std::string str);
		
		// base result object
		class BaseResultObject {
		public:
			BaseResultObject();
			void setUUID(const std::string uuid_);
			const std::string UUID();
		protected:
			std::string uuid;
		};

		class SimInfo : public BaseResultObject {
		public:
			void setProgramVersion(const std::string programVersion);
			std::string getProgramVersion();
			void setSimulationEnvironment(const std::string simulationEnvironment);
			void setInputModelURI(const std::string inputModelURI);
			void setStartDateTimeStamp(const std::string startDateTimeStamp);
			void setRunTime(const std::string elapsedTime);
			void setNumErrorsWarmup(const std::string  numWarningsDuringWarmup, const std::string  numSevereDuringWarmup);
			void setNumErrorsSizing(const std::string  numWarningsDuringSizing, const std::string  numSevereDuringSizing);
			void setNumErrorsSummary(const std::string  numWarnings, const std::string  numSevere);
			json getJSON();
		protected:
			std::string ProgramVersion;
			std::string SimulationEnvironment;
			std::string InputModelURI;
			std::string StartDateTimeStamp;
			std::string RunTime;
			std::string NumWarningsDuringWarmup, NumSevereDuringWarmup, NumWarningsDuringSizing, NumSevereDuringSizing, NumWarnings, NumSevere;
		};

		class Variable : public BaseResultObject {
		public:
			Variable(const std::string VarName, const int ReportFrequency, const int IndexType, const int ReportID, const std::string units);

			std::string variableName();
			void setVariableName(const std::string VarName);

			std::string sReportFrequency();
			int iReportFrequency();
			void setReportFrequency(const int ReportFrequency);
					
			int indexType();
			void setIndexType(const int IndexType);

			int reportID();
			void setReportID(const int Id);
			
			std::string units();
			void setUnits(std::string units);

			void pushValue(const double val);
			std::vector<double>& values();
			
			json getJSON();

		protected:
			std::string varName;
			std::string sReportFreq;
			std::string Units;
			int iReportFreq;
			int idxType;
			int rptID;
			std::vector<double> Values;
		};

		class OutputVariable : public Variable {
		public:
			OutputVariable(const std::string VarName, const int ReportFrequency, const int IndexType, const int ReportID, const std::string units);
		};

		class MeterVariable : public Variable {
		public:
			MeterVariable(const std::string VarName, const int ReportFrequency, const int ReportID, const std::string units, const bool Acculumative=false);

			bool accumulative();
			void setAccumulative(bool state);

			json getJSON();

		protected:
			bool acc;
		};

		class DataFrame : public BaseResultObject {
		public:
			typedef std::pair< int, Variable* > VarPtrPair; 

			DataFrame(std::string ReportFreq);
			~DataFrame();

			void addVariable(Variable *var);

			void setRDataFrameEnabled(bool state);
			void setIDataFrameEnabled(bool state);
			
			bool rDataFrameEnabled();
			bool iDataFrameEnabled();

			void setRVariablesScanned(bool state);
			void setIVariablesScanned(bool state);

			bool rVariablesScanned();
			bool iVariablesScanned();

			void newRow(const int month, const int dayOfMonth, const int hourOfDay, const int curMin);
			void newRow(const std::string ts);
			void pushVariableValue(const int reportID, double value);
			
			Variable* lastVariable();
			
			json getVariablesJSON();
			json getJSON();

			void writeReport();

		protected:
			bool IDataFrameEnabled;
			bool RDataFrameEnabled;
			bool RVariablesScanned;
			bool IVariablesScanned;
			std::string ReportFrequency;
			std::vector < std::string > TS;
			std::unordered_map< int, Variable * > variableMap; // for O(1) lookup when adding to data structure
			int lastVarID;
		};



		class Table :public BaseResultObject {
		public:
			std::string TableName;
			std::string FootnoteText;
			std::vector< std::string > ColHeaders;
			std::vector< std::string > RowHeaders;
			std::vector< std::vector< std::string > > Data;

			Table(Array2D_string const & body,
				Array1D_string const & rowLabels,
				Array1D_string const & columnLabels,
				std::string const & tableName,
				std::string footnoteText = "");

			json getJSON();
		};

		class Report:public BaseResultObject {
		public:
			std::string ReportName;
			std::string ReportForString;
			std::vector< Table* > Tables;

			json getJSON();
		};

		class ReportsCollection : public BaseResultObject {
		public:			
			typedef std::pair< std::string, Report* > RptPtrPair;

			ReportsCollection();

			void addReportTable(Array2D_string const & body,
				Array1D_string const & rowLabels,
				Array1D_string const & columnLabels,
				std::string const & reportName, std::string const & reportForString, 
				std::string const & tableName,
				std::string footnoteText = "");

			json getJSON();

		protected:
			std::unordered_map< std::string, Report * > reportsMap;
			Report *rpt;
		};

		class ResultsSchema : public BaseResultObject {
		public:
			ResultsSchema();
			~ResultsSchema();
			
			void setupOutputOptions();

			bool timeSeriesEnabled();
			bool timeSeriesAndTabularEnabled();

			void initializeRTSDataFrame(const int ReportFrequency, const Array1D< OutputProcessor::RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType = OutputProcessor::ZoneVar);
			void initializeITSDataFrame(const int ReportFrequency, const Array1D< OutputProcessor::IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType = OutputProcessor::ZoneVar);
			void initializeMeters(const Array1D< OutputProcessor::MeterType > &EnergyMeters, const int ReportFrequency);

			static DataFrame RIDetailedZoneTSData, RIDetailedHVACTSData, RITimestepTSData, RIHourlyTSData, RIDailyTSData, RIMonthlyTSData, RIRunPeriodTSData;
			static DataFrame TSMeters, HRMeters, DYMeters, MNMeters, SMMeters;

			void writeTimeSeriesReports();
			void WriteReport();

			SimInfo SimulationInformation;

			std::vector<std::string> MDD;
			std::vector<std::string> RDD;
			ReportsCollection TabularReportsCollection;
		protected:
			char *convert(const std::string & s);

			bool tsEnabled;
			bool tsAndTabularEnabled;
		};

		extern std::unique_ptr<ResultsSchema> OutputSchema;

		void clear_state();
	} // ResultsFramework

} // EnergyPlus

#endif
