// C++ Headers
#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <random>

// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Reference.fwd.hh>
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <ResultsSchema.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

	namespace ResultsFramework {

		using namespace DataHVACGlobals;
		using namespace DataPrecisionGlobals;
		using namespace OutputProcessor;
		using DataGlobals::InitConvTemp;
		using DataGlobals::SecInHour;
		using DataGlobals::DisplayExtraWarnings;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using OutputProcessor::RealVariables;
		using OutputProcessor::RealVariableType;

		static gio::Fmt fmtLD("*");

		std::unique_ptr<ResultsSchema> OutputSchema(new ResultsSchema);

		// getUUID and helper functions copied from shannon's code
		int randomInRange(int min, int max) {
			double scaled = (double)rand() / RAND_MAX;
			return (int)(max - min + 1)*scaled + min;
		}
		//return a version 4 (random) uuid
		std::string getNewUuid()
		{
			std::stringstream uuid_str;
			int four_low = 4096;
			int four_high = 65535;
			int three_low = 256;
			int three_high = 4095;
			uuid_str << std::hex << randomInRange(four_low, four_high);
			uuid_str << std::hex << randomInRange(four_low, four_high);
			uuid_str << "-" << std::hex << randomInRange(four_low, four_high);
			uuid_str << "-" << std::hex << randomInRange(four_low, four_high);
			uuid_str << "-4" << std::hex << randomInRange(three_low, three_high);
			uuid_str << "-8" << std::hex << randomInRange(three_low, three_high);
			uuid_str << std::hex << randomInRange(four_low, four_high);
			uuid_str << std::hex << randomInRange(four_low, four_high);
			return uuid_str.str();
		}


		// Class BaseResultObject
		BaseResultObject::BaseResultObject() {
			UUID = getNewUuid();
		}

		void BaseResultObject::setUUID(const std::string uuid) {
			UUID = uuid;
		}

		const std::string BaseResultObject::getUUID() {
			return UUID;
		}
			

		// Class SimInfo
		void SimInfo::setProgramVersion(const std::string programVersion) {
			ProgramVersion = programVersion;
		}

		void SimInfo::setSimulationEnvironment(const std::string simulationEnvironment) {
			SimulationEnvironment = simulationEnvironment;
		}

		void SimInfo::setInputModelURI(const std::string inputModelURI) {
			InputModelURI = inputModelURI;
		}

		void SimInfo::setStartDateTimeStamp(const std::string startDateTimeStamp) {
			StartDateTimeStamp = startDateTimeStamp;
		}

		void SimInfo::setRunTime(const std::string elapsedTime) {
			RunTime = elapsedTime;
		}
		
		void SimInfo::setNumErrorsWarmup(const std::string numWarningsDuringWarmup, const std::string numSevereDuringWarmup) {
			NumWarningsDuringWarmup = numWarningsDuringWarmup;
			NumSevereDuringWarmup = numSevereDuringWarmup;
		}

		void SimInfo::setNumErrorsSizing(const std::string numWarningsDuringSizing, const std::string numSevereDuringSizing) {
			NumWarningsDuringSizing = numWarningsDuringSizing;
			NumSevereDuringSizing = numSevereDuringSizing;
		}

		void SimInfo::setNumErrorsSummary(const std::string numWarnings, const std::string numSevere) {
			NumWarnings = numWarnings;
			NumSevere = numSevere;
		}

		cJSON* SimInfo::GetJSON()
		{
			cJSON *_root, *_errsummary, *_errwarmup, *_errsizing;

			_root = cJSON_CreateObject();
			cJSON_AddItemToObject(_root, "UUID", cJSON_CreateString(UUID.c_str()));
			cJSON_AddItemToObject(_root, "ProgramVersion", cJSON_CreateString(ProgramVersion.c_str()));
			cJSON_AddItemToObject(_root, "SimulationEnvironment", cJSON_CreateString(SimulationEnvironment.c_str()));
			cJSON_AddItemToObject(_root, "InputModelURI", cJSON_CreateString(InputModelURI.c_str()));
			cJSON_AddItemToObject(_root, "StartDateTimeStamp", cJSON_CreateString(StartDateTimeStamp.c_str()));
			cJSON_AddItemToObject(_root, "RunTime", cJSON_CreateString(RunTime.c_str()));

			cJSON_AddItemToObject(_root, "ErrorSummary", _errsummary = cJSON_CreateObject());
			cJSON_AddItemToObject(_errsummary, "NumWarnings", cJSON_CreateString(NumWarnings.c_str()));
			cJSON_AddItemToObject(_errsummary, "NumSevere", cJSON_CreateString(NumSevere.c_str()));

			cJSON_AddItemToObject(_root, "ErrorSummaryWarmup", _errwarmup = cJSON_CreateObject());
			cJSON_AddItemToObject(_errwarmup, "NumWarnings", cJSON_CreateString(NumWarningsDuringWarmup.c_str()));
			cJSON_AddItemToObject(_errwarmup, "NumSevere", cJSON_CreateString(NumSevereDuringWarmup.c_str()));

			cJSON_AddItemToObject(_root, "ErrorSummarySizing", _errsizing = cJSON_CreateObject());
			cJSON_AddItemToObject(_errsizing, "NumWarnings", cJSON_CreateString(NumWarningsDuringSizing.c_str()));
			cJSON_AddItemToObject(_errsizing, "NumSevere", cJSON_CreateString(NumSevereDuringSizing.c_str()));


			//return std::string(cJSON_Print(_root));
			return _root;
		}


		// class DataFrame
		DataFrame::DataFrame(std::string ReportFreq) {
			ReportFrequency = ReportFreq;
			CurrentRow = -1;
			RDataFrameEnabled = false;
			IDataFrameEnabled = false;
		}

		DataFrame::~DataFrame() {
		}

		void DataFrame::addRCol(std::string VarName, std::string VarUnits) {
			RCols.push_back(std::make_pair(VarName, VarUnits));
		}

		void DataFrame::addICol(std::string VarName, std::string VarUnits) {
			ICols.push_back(std::make_pair(VarName, VarUnits));
		}

		void DataFrame::newRow(std::string ts) {
			std::vector<double> rrow;
			std::vector<int> irow;
			RRows.push_back(rrow);
			IRows.push_back(irow);
			TS.push_back(ts);
			CurrentRow++;
		}

		void DataFrame::addToCurrentRRow(double value) {
			RRows[CurrentRow].push_back(value);
		}

		void DataFrame::addToCurrentIRow(int value) {
			IRows[CurrentRow].push_back(value);
		}

		void DataFrame::writeFile() {
			std::string jsonfilename = "eplusout_" + ReportFrequency + ".json";
			std::ofstream jsonfile(jsonfilename);

			cJSON *_root, *_col, *_colfld, *_val, *_row, *_rowvec;

			_root = cJSON_CreateObject();
			cJSON_AddItemToObject(_root, "UUID", cJSON_CreateString(UUID.c_str()));
			cJSON_AddItemToObject(_root, "ReportFrequency", cJSON_CreateString(ReportFrequency.c_str()));
			cJSON_AddItemToObject(_root, "Cols", _col = cJSON_CreateArray());
			cJSON_AddItemToObject(_root, "Rows", _row = cJSON_CreateArray());

			if (jsonfile.is_open())
			{
				for (int col = 0; col < RCols.size(); ++col)
				{
					cJSON_AddItemToArray(_col, _colfld = cJSON_CreateObject());
					cJSON_AddStringToObject(_colfld, "Variable", RCols[col].first.c_str());
					cJSON_AddStringToObject(_colfld, "Units", RCols[col].second.c_str());
				}
				for (int col = 0; col < ICols.size(); ++col)
				{
					cJSON_AddItemToArray(_col, _colfld = cJSON_CreateObject());
					cJSON_AddStringToObject(_colfld, "Variable", ICols[col].first.c_str());
					cJSON_AddStringToObject(_colfld, "Units", ICols[col].second.c_str());
				}

				std::string values;
				values.reserve(10000); // reserve a large size to improve efficiency

				for (int row = 0; row < RRows.size(); ++row)
				{
					values = "[";
					for (int col = 0; col < RRows[row].size(); ++col)
					{
						values += std::to_string(RRows[row][col]);
						if (IDataFrameEnabled == false && col < RRows[row].size() - 1)
							values += ", ";
					}
					for (int col = 0; col < IRows[row].size(); ++col)
					{
						values += std::to_string(IRows[row][col]);
						if (col < IRows[row].size() - 1)
							values += ", ";
					}
					values += "]";
					cJSON_AddItemToArray(_row, _rowvec = cJSON_CreateObject());
					cJSON_AddStringToObject(_rowvec, TS.at(row).c_str(), values.c_str());
					//cJSON_AddItemToObject(_rowvec, TS.at(row).c_str(), cJSON_CreateDoubleArray(&RRows[row][0], RRows[row].size()));
					//cJSON_AddItemToObject(_rowvec, TS.at(row).c_str(), cJSON_CreateIntArray(&IRows[row][0], RRows[row].size()));

				}
				jsonfile << cJSON_Print(_root);
				jsonfile.close();
				cJSON_Delete(_root);
			}
			// does this need to go to error?
			else
				ShowWarningError("Unable to open file for time-series output.");
		}


		// Class ResultsSchema
		// initialize data frames
		DataFrame ResultsSchema::RIDetailedZoneTSData("Detailed-Zone"),
			ResultsSchema::RIDetailedHVACTSData("Detailed-HVAC"),
			ResultsSchema::RITimestepTSData("Timestep"),
			ResultsSchema::RIHourlyTSData("Hourly"),
			ResultsSchema::RIDailyTSData("Daily"),
			ResultsSchema::RIMonthlyTSData("Monthly"),
			ResultsSchema::RIRunPeriodTSData("RunPeriod");


		ResultsSchema::ResultsSchema() {
			//UUID = getUUID();
			tsEnabled = false;
			tsAndTabularEnabled = false;
		}

		ResultsSchema::~ResultsSchema() {

		}

		void ResultsSchema::setupOutputOptions() {
			DisplayString("Setting up output schema options");
			int numberOfOutputSchemaObjects = InputProcessor::GetNumObjectsFound("Output:JSON");
			DisplayString("No of JSON objects: " + std::to_string(numberOfOutputSchemaObjects));
			if (numberOfOutputSchemaObjects == 1) {
				try {
					FArray1D_string alphas(5);
					int numAlphas;
					FArray1D< Real64 > numbers(2);
					int numNumbers;
					int status;
					DisplayString("getobjectItem");
					InputProcessor::GetObjectItem("Output:JSON", 1, alphas, numAlphas, numbers, numNumbers, status);
					DisplayString("numalphas: " + std::to_string(numAlphas));

					if (numAlphas > 0) {
						std::string option = alphas(1);
						if (InputProcessor::SameString(option, "TimeSeries")) {
							tsEnabled = true;

							DisplayString("time series enabedand set to true");
						}
						else if (InputProcessor::SameString(option, "TimeSeriesAndTabular")) {
							tsEnabled = true;
							tsAndTabularEnabled = true;
						}
					}
				}
				catch (const std::runtime_error& error) {
					ShowFatalError(error.what());
				}
			}
		}

		bool ResultsSchema::timeSeriesEnabled() {
			return tsEnabled;
		}

		bool ResultsSchema::timeSeriesAndTabularEnabled() {
			return tsAndTabularEnabled;
		}

		void ResultsSchema::initializeRTSDataFrame(const int ReportFrequency, const FArray1D< RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType) {
			Reference< RealVariables > RVar;
			
			/*if (timeSeriesEnabled == false)
			{
				DisplayString("False, Initializing Data Frame, and setting to true");
				timeSeriesEnabled = true;

			}
			else
				DisplayString("Already True, Initializing Data Frame");
			*/

			for (int Loop = 1; Loop <= NumOfRVariable; ++Loop) {
					RVar >>= RVariableTypes(Loop).VarPtr;
					auto & rVar(RVar());
					if (rVar.Report && rVar.ReportFreq == ReportFrequency && rVar.Stored)
					{
						switch (ReportFrequency)
						{
						case -1:  // each time UpdatedataandReport is called
							if (IndexType == ZoneVar && RVariableTypes(Loop).IndexType == ZoneVar)
							{
								RIDetailedZoneTSData.RDataFrameEnabled = true;
								RIDetailedZoneTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							}
							else if (IndexType == HVACVar && RVariableTypes(Loop).IndexType == HVACVar)
							{
								RIDetailedHVACTSData.RDataFrameEnabled = true;
								RIDetailedHVACTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							}
							break;
						case 0:  // at 'EndTimeStepFlag'
							RITimestepTSData.RDataFrameEnabled = true;
							RITimestepTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 1:  // at 'EndHourFlag'
							RIHourlyTSData.RDataFrameEnabled = true;
							RIHourlyTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 2: // at 'EndDayFlag'
							RIDailyTSData.RDataFrameEnabled = true;
							RIDailyTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 3:  // at end of month
							RIMonthlyTSData.RDataFrameEnabled = true;
							RIMonthlyTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 4:  // once per environment 'EndEnvrnFlag'
							RIRunPeriodTSData.RDataFrameEnabled = true;
							RIRunPeriodTSData.addRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
					}
				}
			}
		}

		void ResultsSchema::initializeITSDataFrame(const int ReportFrequency, const FArray1D< IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType) {
			Reference< IntegerVariables > IVar;

			for (int Loop = 1; Loop <= NumOfIVariable; ++Loop) {
				IVar >>= IVariableTypes(Loop).VarPtr;
				auto & iVar(IVar());
				if (iVar.Report && iVar.ReportFreq == ReportFrequency && iVar.Stored)
				{
					switch (ReportFrequency)
					{
					case -1:  // each time UpdatedataandReport is called
						if (IndexType == ZoneVar && IVariableTypes(Loop).IndexType == ZoneVar)
						{
							RIDetailedZoneTSData.IDataFrameEnabled = true;
							RIDetailedZoneTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						}
						else if (IndexType == HVACVar && IVariableTypes(Loop).IndexType == HVACVar)
						{
							RIDetailedZoneTSData.IDataFrameEnabled = true;
							RIDetailedZoneTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						}
						break;
					case 0:  // at 'EndTimeStepFlag'
						RITimestepTSData.IDataFrameEnabled = true;
						RITimestepTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 1:  // at 'EndHourFlag'
						RIHourlyTSData.IDataFrameEnabled = true;
						RIHourlyTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 2: // at 'EndDayFlag'
						RIDailyTSData.IDataFrameEnabled = true;
						RIDailyTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 3:  // at end of month
						RIMonthlyTSData.IDataFrameEnabled = true;
						RIMonthlyTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 4:  // once per environment 'EndEnvrnFlag'
						RIRunPeriodTSData.IDataFrameEnabled = true;
						RIRunPeriodTSData.addICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					}
				}
			}

		}

		void ResultsSchema::writeTimeSeriesFiles()
		{

			if (OutputSchema->RIDetailedZoneTSData.RDataFrameEnabled || OutputSchema->RIDetailedZoneTSData.IDataFrameEnabled)
				OutputSchema->RIDetailedZoneTSData.writeFile();

			// Output detailed HVAC time series data
			if (OutputSchema->RIDetailedHVACTSData.RDataFrameEnabled || OutputSchema->RIDetailedHVACTSData.IDataFrameEnabled)
				OutputSchema->RIDetailedHVACTSData.writeFile();

			// Output timestep time series data
			if (OutputSchema->RITimestepTSData.RDataFrameEnabled || OutputSchema->RITimestepTSData.IDataFrameEnabled)
				OutputSchema->RITimestepTSData.writeFile();

			// Output hourly time series data
			if (OutputSchema->RIHourlyTSData.RDataFrameEnabled || OutputSchema->RIHourlyTSData.IDataFrameEnabled)
				OutputSchema->RIHourlyTSData.writeFile();

			// Output daily time series data
			if (OutputSchema->RIDailyTSData.RDataFrameEnabled || OutputSchema->RIDailyTSData.IDataFrameEnabled)
				OutputSchema->RIDailyTSData.writeFile();

			// Output monthly time series data
			if (OutputSchema->RIMonthlyTSData.RDataFrameEnabled || OutputSchema->RIMonthlyTSData.IDataFrameEnabled)
				OutputSchema->RIMonthlyTSData.writeFile();

			// Output run period time series data
			if (OutputSchema->RIRunPeriodTSData.RDataFrameEnabled || OutputSchema->RIRunPeriodTSData.IDataFrameEnabled)
				OutputSchema->RIRunPeriodTSData.writeFile();
		}

		void ResultsSchema::writeFile()
		{
			std::string jsonfilename = "eplusout.json";
			std::ofstream jsonfile(jsonfilename);

			cJSON *_root, *_simRes;

			_root = cJSON_CreateObject();


			cJSON_AddItemToObject(_root, "SimulationResults", _simRes = cJSON_CreateObject());
			cJSON_AddItemToObject(_simRes, "Simulation", SimulationInformation.GetJSON());
			
			if (jsonfile.is_open())	{
				jsonfile << cJSON_Print(_root);
				jsonfile.close();
				cJSON_Delete(_root);
			}
		}
	} // ResultsFramework

} // EnergyPlus
