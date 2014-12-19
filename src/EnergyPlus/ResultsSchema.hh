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


namespace EnergyPlus {

	namespace ResultsSchema {
		using namespace OutputProcessor;

		class DataFrame {
		public:
			DataFrame(std::string ReportFreq);
			~DataFrame();
		
			std::string ReportFrequency;
			std::vector < std::pair <std::string, std::string> > RCols, ICols; // VarName and VarUnits
			std::vector < std::vector <double> > RRows;
			std::vector < std::vector <int> > IRows;
			std::vector < std::string > TS;

			void AddRCol(std::string VarName, std::string VarUnits);
			void AddICol(std::string VarName, std::string VarUnits);

			void NewRow(std::string ts);

			void AddToCurrentRRow(double value);
			void AddToCurrentIRow(int value);

			void WriteFile();

			bool RDataFrameEnabled;
			bool IDataFrameEnabled;
		protected:
			std::string UUID;
			int CurrentRow;
		};


		class ResultsSchema {
		public:
			ResultsSchema();
			~ResultsSchema();
			
			void InitializeRTSDataFrame(const int ReportFrequency, const FArray1D< RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType = ZoneVar);
			void InitializeITSDataFrame(const int ReportFrequency, const FArray1D< IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType = ZoneVar);

			static DataFrame RIDetailedZoneTSData, RIDetailedHVACTSData, RITimestepTSData, RIHourlyTSData, RIDailyTSData, RIMonthlyTSData, RIRunPeriodTSData;
		protected:
			std::string UUID;
		};

		static ResultsSchema OutputSchema;

	} // ResultsSchema

} // EnergyPlus

#endif
