#ifndef ResultsSchema_hh_INCLUDED
#define ResultsSchema_hh_INCLUDED

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
			void setUUID(const std::string uuid);
			const std::string getUUID();

		protected:
			std::string UUID;
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

			// I don't like this pointer stuff but passing back an std:string and converting back for cjson print is expensive
			cJSON* GetJSON();
		protected:
			std::string ProgramVersion;
			std::string SimulationEnvironment;
			std::string InputModelURI;
			std::string StartDateTimeStamp;
			std::string RunTime;
			std::string NumWarningsDuringWarmup, NumSevereDuringWarmup, NumWarningsDuringSizing, NumSevereDuringSizing, NumWarnings, NumSevere;
		};

		class DataFrame : public BaseResultObject {
		public:
			DataFrame(std::string ReportFreq);
			~DataFrame();
		
			std::string ReportFrequency;
			std::vector < std::pair <std::string, std::string> > RCols, ICols; // VarName and VarUnits
			std::vector < std::vector <double> > RRows;
			std::vector < std::vector <int> > IRows;
			std::vector < std::string > TS;

			void addRCol(std::string VarName, std::string VarUnits);
			void addICol(std::string VarName, std::string VarUnits);

			void newRow(std::string ts);

			void addToCurrentRRow(double value);
			void addToCurrentIRow(int value);

			void writeFile();

			bool RDataFrameEnabled;
			bool IDataFrameEnabled;
		protected:
			int CurrentRow;
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
