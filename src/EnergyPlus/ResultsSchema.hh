#ifndef ResultsSchema_hh_INCLUDED
#define ResultsSchema_hh_INCLUDED

#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Reference.hh>

// cJSON header
#include <cJSON.h>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <OutputProcessor.hh>

#include <memory>

namespace EnergyPlus {

	namespace ResultsFramework {

		// getUUID copied from Shannon's code; to be replaced with the right UUID API calls
		std::string getUUID();
		int randomInRange(int min, int max);

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
			void setSimulationEnvironment(const std::string simulationEnvironment);
			void setInputModelURI(const std::string inputModelURI);
			void setStartDateTimeStamp(const std::string startDateTimeStamp);
			void setRunTime(const std::string elapsedTime);
			void setNumErrorsWarmup(const std::string  numWarningsDuringWarmup, const std::string  numSevereDuringWarmup);
			void setNumErrorsSizing(const std::string  numWarningsDuringSizing, const std::string  numSevereDuringSizing);
			void setNumErrorsSummary(const std::string  numWarnings, const std::string  numSevere);
			cJSON* GetJSON();
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
			
			cJSON* GetJSON();
		protected:
			std::string varName;
			std::string sReportFreq;
			std::string Units;
			int iReportFreq;
			int idxType;
			int rptID;
			std::vector<double> Values;
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

			void newRow(const int month, const int dayOfMonth, const int hourOfDay, const int curMin);
			void pushVariableValue(const int reportID, double value);
			
			Variable* lastVariable();
			std::vector < Variable *> variables();

			void writeFile();
		protected:
			bool IDataFrameEnabled;
			bool RDataFrameEnabled;
			std::string ReportFrequency;
			std::vector < std::string > TS;
			std::vector < Variable *> outputVariables;
			std::unordered_map< int, Variable * > variableMap; // for O(1) lookup when adding to data structure
		};


		class ResultsSchema : public BaseResultObject {
		public:
			ResultsSchema();
			~ResultsSchema();
			
			void setupOutputOptions();

			bool timeSeriesEnabled();
			bool timeSeriesAndTabularEnabled();

			void initializeRTSDataFrame(const int ReportFrequency, const FArray1D< OutputProcessor::RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType = OutputProcessor::ZoneVar);
			void initializeITSDataFrame(const int ReportFrequency, const FArray1D< OutputProcessor::IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType = OutputProcessor::ZoneVar);

			static DataFrame RIDetailedZoneTSData, RIDetailedHVACTSData, RITimestepTSData, RIHourlyTSData, RIDailyTSData, RIMonthlyTSData, RIRunPeriodTSData;

			void writeTimeSeriesFiles();
			void writeFile();

			SimInfo SimulationInformation;
		protected:
			bool tsEnabled;
			bool tsAndTabularEnabled;
			
		};

		extern std::unique_ptr<ResultsSchema> OutputSchema;

	} // ResultsFramework

} // EnergyPlus

#endif
