// C++ Headers
#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <random>

// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Reference.fwd.hh>
#include <ObjexxFCL/Array1D.hh>
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

		// trim string
		std::string trim(std::string str)
		{
			str.erase(str.begin(), find_if(str.begin(), str.end(),
				[](char& ch)->bool { return !isspace(ch); }));
			str.erase(find_if(str.rbegin(), str.rend(),
				[](char& ch)->bool { return !isspace(ch); }).base(), str.end());
			return str;
		}

		// Class BaseResultObject
		BaseResultObject::BaseResultObject() {
			uuid = getNewUuid();
		}

		void BaseResultObject::setUUID(const std::string uuid_) {
			uuid = uuid_;
		}

		const std::string BaseResultObject::UUID() {
			return uuid;
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

		cJSON* SimInfo::getJSON()
		{
			cJSON *_root, *_errsummary, *_errwarmup, *_errsizing;

			_root = cJSON_CreateObject();
			cJSON_AddItemToObject(_root, "UUID", cJSON_CreateString(uuid.c_str()));
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

			return _root;
		}

		

		// Class Variable
		Variable::Variable(const std::string VarName, const int ReportFrequency, const int IndexType, const int ReportID, const std::string units) {
			varName = VarName;
			setReportFrequency(ReportFrequency);
			idxType = IndexType;
			rptID = ReportID;
			Units = units;
		}

		std::string Variable::variableName() {
			return varName;
		}

		void Variable::setVariableName(const std::string VarName) {
			varName = VarName;
		}
		
		std::string Variable::sReportFrequency() {
			return sReportFreq;
		}

		int Variable::iReportFrequency() {
			return iReportFreq;
		}
		
		void Variable::setReportFrequency(const int ReportFrequency) {
			iReportFreq = ReportFrequency;
			switch (iReportFreq)
			{
			case -1:  // each time UpdatedataandReport is called
				if (idxType == ZoneVar)
					sReportFreq = "Detailed - Zone";
				if (idxType == HVACVar)
					sReportFreq = "Detailed - HVAC";
				break;
			case 0:  // at 'EndTimeStepFlag'
				sReportFreq = "Timestep";
				break;
			case 1:  // at 'EndHourFlag'
				sReportFreq = "Hourly";
				break;
			case 2: // at 'EndDayFlag'
				sReportFreq = "Daily";
				break;
			case 3:  // at end of month
				sReportFreq = "Monthly";
				break;
			case 4:  // once per environment 'EndEnvrnFlag'
				sReportFreq = "RunPeriod";
				break;
			}
		}
		
		int Variable::indexType() {
			return idxType;
		}

		void Variable::setIndexType(int IndexType) {
			idxType = IndexType;
		}

		int Variable::reportID() {
			return rptID;
		}

		void Variable::setReportID(int Id) {
			rptID = Id;
		}

		std::string Variable::units() {
			return Units;
		}

		void Variable::setUnits(std::string units) {
			Units = units;
		}

		void Variable::pushValue(const double val) {
			Values.push_back(val);
		}

		std::vector<double>& Variable::values() {
			return Values;
		}

		cJSON* Variable::getJSON() {
			cJSON *_root, *_errsummary, *_errwarmup, *_errsizing;

			_root = cJSON_CreateObject();
			cJSON_AddItemToObject(_root, "UUID", cJSON_CreateString(uuid.c_str()));
			cJSON_AddItemToObject(_root, "Name", cJSON_CreateString(varName.c_str()));
			cJSON_AddItemToObject(_root, "Units", cJSON_CreateString(Units.c_str()));
			cJSON_AddItemToObject(_root, "Frequency", cJSON_CreateString(sReportFreq.c_str()));

			return _root;
		}


		// Class OutputVariable
		OutputVariable::OutputVariable(const std::string VarName, const int ReportFrequency, const int IndexType, const int ReportID, const std::string units)
			:Variable(VarName, ReportFrequency, IndexType, ReportID, units)
		{}

		// Class MeterVariable
		MeterVariable::MeterVariable(const std::string VarName, const int ReportFrequency, const int ReportID, const std::string units, const bool Accumulative)
			: Variable(VarName, ReportFrequency, ZoneVar, ReportID, units)
		{
			acc = Accumulative;
		}

		bool MeterVariable::accumulative()
		{
			return acc;
		}

		void MeterVariable::setAccumulative(bool state) {
			acc = state;
		}

		cJSON* MeterVariable::getJSON() {
			cJSON *_root = Variable::getJSON();
			if (acc)
				cJSON_AddItemToObject(_root, "Cumulative", cJSON_CreateString("True"));
			return _root;
		}

		// class DataFrame
		DataFrame::DataFrame(std::string ReportFreq) {
			ReportFrequency = ReportFreq;
			RDataFrameEnabled = false;
			IDataFrameEnabled = false;
			RVariablesScanned = false;
			IVariablesScanned = false;
		}

		DataFrame::~DataFrame() {
		}

		void DataFrame::addVariable(Variable *var) {
			lastVarID = var->reportID();
			variableMap.insert(VarPtrPair(lastVarID, var));
		}

		Variable* DataFrame::lastVariable() {
			return variableMap.at(lastVarID);
		}

		void DataFrame::newRow(const int month, const int dayOfMonth, const int hourOfDay, const int curMin) {
			std::string ts = std::to_string(month) + "/" + std::to_string(dayOfMonth) + " " + std::to_string(hourOfDay) + ":" + std::to_string(curMin) + ":00";
			TS.push_back(ts);
		}
		
		void DataFrame::newRow(const std::string ts) {
			TS.push_back(ts);
		}

		void DataFrame::setRDataFrameEnabled(bool state) {
			RDataFrameEnabled = state;
		}

		void DataFrame::setIDataFrameEnabled(bool state) {
			IDataFrameEnabled = state;
		}

		bool DataFrame::rDataFrameEnabled() {
			return RDataFrameEnabled;
		}

		bool DataFrame::iDataFrameEnabled() {
			return IDataFrameEnabled;
		}
		void DataFrame::setRVariablesScanned(bool state) {
			RVariablesScanned = state;
		}

		void DataFrame::setIVariablesScanned(bool state) {
			IVariablesScanned = state;
		}

		bool DataFrame::rVariablesScanned() {
			return RVariablesScanned;
		}

		bool DataFrame::iVariablesScanned() {
			return IVariablesScanned;
		}
		
		void DataFrame::pushVariableValue(const int reportID, double value) {
			// this is O(1) complexity. I like this.
			variableMap[reportID]->pushValue(value); 
		}

		cJSON* DataFrame::getVariablesJSON() {
			cJSON *arr;
			arr = cJSON_CreateArray();
			//for (int i = 0; i < variables().size(); i++);
			for (auto it = variableMap.begin(); it != variableMap.end(); ++it)
				cJSON_AddItemToArray(arr, it->second->getJSON());
			return arr;
		}

		cJSON* DataFrame::getJSON() {
			cJSON *_root, *_col, *_colfld, *_val, *_row, *_rowvec;

			_root = cJSON_CreateObject();
			cJSON_AddItemToObject(_root, "UUID", cJSON_CreateString(uuid.c_str()));
			cJSON_AddItemToObject(_root, "ReportFrequency", cJSON_CreateString(ReportFrequency.c_str()));
			cJSON_AddItemToObject(_root, "Cols", _col = cJSON_CreateArray());
			cJSON_AddItemToObject(_root, "Rows", _row = cJSON_CreateArray());

			for (auto it = variableMap.begin(); it != variableMap.end(); ++it) {
				cJSON_AddItemToArray(_col, _colfld = cJSON_CreateObject());
				cJSON_AddStringToObject(_colfld, "Variable", it->second->variableName().c_str());
				cJSON_AddStringToObject(_colfld, "Units", it->second->units().c_str());
				cJSON_AddStringToObject(_colfld, "UUID", it->second->UUID().c_str());
			}

			std::vector <double> vals;
			vals.reserve(10000);

			// if DataFrame is enabled and control reaches here, there must be at least one o/p variable
			assert(TS.size() == variableMap.begin()->second->values().size());

			for (int row = 0; row < TS.size(); ++row) {
				vals.clear();
				for (auto it = variableMap.begin(); it != variableMap.end(); ++it) {
					vals.push_back(it->second->values()[row]);
				}
				cJSON_AddItemToArray(_row, _rowvec = cJSON_CreateObject());
				cJSON_AddItemToObject(_rowvec, TS.at(row).c_str(), cJSON_CreateDoubleArray(&vals[0], vals.size()));
			}
			return _root;
		}

		void DataFrame::writeFile() {
			std::string jsonfilename = "eplusout_" + ReportFrequency + ".json";
			std::ofstream jsonfile(jsonfilename);
			
			cJSON *_root;
			
			if (jsonfile.is_open())
			{
				_root = getJSON();
				jsonfile << cJSON_Print(_root);
				jsonfile.close();
			}
			// does this need to go to error?
			else
				ShowWarningError("Unable to open file for time-series output.");
			
			cJSON_Delete(_root);
		}


		// class Table

		Table::Table(Array2D_string const & body,
			Array1D_string const & rowLabels,
			Array1D_string const & columnLabels,
			std::string const & tableName,
			std::string footnoteText) 
		{

			size_t sizeColumnLabels = columnLabels.size();
			size_t sizeRowLabels = rowLabels.size();
			TableName = tableName;
			FootnoteText = footnoteText;

			for (size_t iCol = 0, k = body.index(1, 1); iCol < sizeColumnLabels; ++iCol) {
				ColHeaders.push_back(columnLabels[iCol]);
				std::vector< std::string > col;
				for (size_t iRow = 0; iRow < sizeRowLabels; ++iRow) {
					if (iCol == 0) {
						// do this once only
						RowHeaders.push_back(rowLabels[iRow]);
					}
					col.push_back(trim(body[k]));
					++k;
				}
				Data.push_back(col);
				col.clear();
			}
		}

		cJSON* Table::getJSON() {
			cJSON *_root;

			_root = cJSON_CreateObject();
			cJSON_AddStringToObject(_root, "TableName", TableName.c_str());
			cJSON_AddStringToObject(_root, "UUID", UUID().c_str());

			cJSON *_cols;
			cJSON_AddItemToObject(_root, "Cols", _cols = cJSON_CreateArray());
			for (int i = 0; i < ColHeaders.size(); i++) {
				cJSON* str = cJSON_CreateString(ColHeaders[i].c_str());
				cJSON_AddItemToArray(_cols, str);
			}

			cJSON *_rows;
			cJSON_AddItemToObject(_root, "Rows", _rows = cJSON_CreateObject());

			for (int row = 0; row < RowHeaders.size(); ++row) {
				cJSON *_rowvec;
				cJSON_AddItemToObject(_rows, RowHeaders[row].c_str(), _rowvec = cJSON_CreateArray());
				for (int col = 0; col < ColHeaders.size(); ++col) {
					cJSON *_val = cJSON_CreateString(Data[col][row].c_str());
					cJSON_AddItemToArray(_rowvec, _val);
				}
			}
			if (!FootnoteText.empty())
				cJSON_AddStringToObject(_root, "Footnote", FootnoteText.c_str());

			return _root;
		}

		// class Report

		cJSON* Report::getJSON() {
			cJSON *_root;
			_root = cJSON_CreateObject();
			cJSON_AddStringToObject(_root, "ReportName", ReportName.c_str());
			cJSON_AddStringToObject(_root, "For", ReportForString.c_str());
			cJSON_AddStringToObject(_root, "UUID", UUID().c_str());

			cJSON *_cols;
			cJSON_AddItemToObject(_root, "Tables", _cols = cJSON_CreateArray());
			for (int i = 0; i < Tables.size(); i++) {
				cJSON* tbl = Tables[i]->getJSON();
				cJSON_AddItemToArray(_cols, tbl);
			}

			return _root;
		}


		// class ReportsCollection
		ReportsCollection::ReportsCollection() {
		}
		
		void ReportsCollection::addReportTable(Array2D_string const & body,
				Array1D_string const & rowLabels,
				Array1D_string const & columnLabels,
				std::string const & reportName, std::string const & reportForString,
				std::string const & tableName,
				std::string footnoteText)	
		{
			std::string key = reportName + reportForString;
			Report *r;

			auto search = reportsMap.find(key);
			if (search != reportsMap.end()) {
				r = search->second;
			}
			else {
				r = new Report();
				r->ReportName = reportName;
				r->ReportForString = reportForString;
				reportsMap.insert(RptPtrPair(key, r));
			}
			
			Table *tbl = new Table(body, rowLabels, columnLabels, tableName, footnoteText);
			r->Tables.push_back(tbl);
		}

		cJSON* ReportsCollection::getJSON() {
			cJSON *_root;
			_root = cJSON_CreateArray();

			for (RptPtrPair iter : reportsMap) {
				cJSON *_rpts;
				cJSON_AddItemToArray(_root, iter.second->getJSON());
			}
			return _root;
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
		
		DataFrame ResultsSchema::TSMeters("Timestep"),
			ResultsSchema::HRMeters("Hourly"),
			ResultsSchema::DYMeters("Daily"),
			ResultsSchema::MNMeters("Monthly"),
			ResultsSchema::SMMeters("RunPeriod");

		ResultsSchema::ResultsSchema() {
			tsEnabled = false;
			tsAndTabularEnabled = false;
		}

		ResultsSchema::~ResultsSchema() {

		}

		void ResultsSchema::setupOutputOptions() {
			int numberOfOutputSchemaObjects = InputProcessor::GetNumObjectsFound("Output:JSON");
			if (numberOfOutputSchemaObjects == 1) {
				try {
					Array1D_string alphas(5);
					int numAlphas;
					Array1D< Real64 > numbers(2);
					int numNumbers;
					int status;
					InputProcessor::GetObjectItem("Output:JSON", 1, alphas, numAlphas, numbers, numNumbers, status);

					if (numAlphas > 0) {
						std::string option = alphas(1);
						if (InputProcessor::SameString(option, "TimeSeries")) {
							tsEnabled = true;
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

		void ResultsSchema::initializeRTSDataFrame(const int ReportFrequency, const Array1D< RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType) {
			Reference< RealVariables > RVar;

			for (int Loop = 1; Loop <= NumOfRVariable; ++Loop) {
					RVar >>= RVariableTypes(Loop).VarPtr;
					auto & rVar(RVar());
					if (rVar.Report && rVar.ReportFreq == ReportFrequency) {
						Variable *var = new Variable(RVariableTypes(Loop).VarName, 
								ReportFrequency, RVariableTypes(Loop).IndexType, 
								RVariableTypes(Loop).ReportID,
								RVariableTypes(Loop).UnitsString);
						switch (ReportFrequency) {
						case -1:  // each time UpdatedataandReport is called
							if (IndexType == ZoneVar && RVariableTypes(Loop).IndexType == ZoneVar)
							{
								RIDetailedZoneTSData.setRDataFrameEnabled(true);
								RIDetailedZoneTSData.addVariable(var);
							}
							else if (IndexType == HVACVar && RVariableTypes(Loop).IndexType == HVACVar)
							{
								RIDetailedHVACTSData.setRDataFrameEnabled(true);
								RIDetailedHVACTSData.addVariable(var);
							}
							break;
						case 0:  // at 'EndTimeStepFlag'
							RITimestepTSData.setRDataFrameEnabled(true);
							RITimestepTSData.addVariable(var);
							break;
						case 1:  // at 'EndHourFlag'
							RIHourlyTSData.setRDataFrameEnabled(true);
							RIHourlyTSData.addVariable(var);
							break;
						case 2: // at 'EndDayFlag'
							RIDailyTSData.setRDataFrameEnabled(true);
							RIDailyTSData.addVariable(var);
							break;
						case 3:  // at end of month
							RIMonthlyTSData.setRDataFrameEnabled(true);
							RIMonthlyTSData.addVariable(var);
							break;
						case 4:  // once per environment 'EndEnvrnFlag'
							RIRunPeriodTSData.setRDataFrameEnabled(true);
							RIRunPeriodTSData.addVariable(var);
							break;
						}
				}
			}
			// set the scanned variables to true or false
			switch (ReportFrequency) {
				case -1:
					if (IndexType == ZoneVar)
						RIDetailedZoneTSData.setRVariablesScanned(true);
					if (IndexType == HVACVar)
						RIDetailedHVACTSData.setRVariablesScanned(true);
					break;
				case 0:  // at 'EndTimeStepFlag'
					RITimestepTSData.setRVariablesScanned(true);
					break;
				case 1:  // at 'EndHourFlag'
					RIHourlyTSData.setRVariablesScanned(true);
					break;
				case 2: // at 'EndDayFlag'
					RIDailyTSData.setRVariablesScanned(true);
					break;
				case 3:  // at end of month
					RIMonthlyTSData.setRVariablesScanned(true);
					break;
				case 4:  // once per environment 'EndEnvrnFlag'
					RIRunPeriodTSData.setRVariablesScanned(true);
					break;
			}
		}

		void ResultsSchema::initializeITSDataFrame(const int ReportFrequency, const Array1D< IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType) {
			Reference< IntegerVariables > IVar;

			// loop over values to suck in var info
			for (int Loop = 1; Loop <= NumOfIVariable; ++Loop) {
				IVar >>= IVariableTypes(Loop).VarPtr;
				auto & iVar(IVar());
				if (iVar.Report && iVar.ReportFreq == ReportFrequency) {
					OutputVariable *var = new OutputVariable(IVariableTypes(Loop).VarName, ReportFrequency, 
								IVariableTypes(Loop).IndexType, 
								IVariableTypes(Loop).ReportID,
								IVariableTypes(Loop).UnitsString);
					switch (ReportFrequency) {
					case -1:  // each time UpdatedataandReport is called
						if (IndexType == ZoneVar && IVariableTypes(Loop).IndexType == ZoneVar)
						{
							RIDetailedZoneTSData.setIDataFrameEnabled(true);
							RIDetailedZoneTSData.addVariable(var);
						}
						else if (IndexType == HVACVar && IVariableTypes(Loop).IndexType == HVACVar)
						{
							RIDetailedHVACTSData.setIDataFrameEnabled(true);
							RIDetailedHVACTSData.addVariable(var);
						}
						break;
					case 0:  // at 'EndTimeStepFlag'
						RITimestepTSData.setIDataFrameEnabled(true);
						RITimestepTSData.addVariable(var);
						break;
					case 1:  // at 'EndHourFlag'
						RIHourlyTSData.setIDataFrameEnabled(true);
						RIHourlyTSData.addVariable(var);
						break;
					case 2: // at 'EndDayFlag'
						RIDailyTSData.setIDataFrameEnabled(true);
						RIDailyTSData.addVariable(var);
						break;
					case 3:  // at end of month
						RIMonthlyTSData.setIDataFrameEnabled(true);
						RIMonthlyTSData.addVariable(var);
						break;
					case 4:  // once per environment 'EndEnvrnFlag'
						RIRunPeriodTSData.setIDataFrameEnabled(true);
						RIRunPeriodTSData.addVariable(var);
						break;
					}
				}
			}

			// set the scanned variables to true or false
			switch (ReportFrequency) {
				case -1:
					if (IndexType == ZoneVar)
						RIDetailedZoneTSData.setIVariablesScanned(true);
					if (IndexType == HVACVar)
						RIDetailedHVACTSData.setIVariablesScanned(true);
					break;
				case 0:  // at 'EndTimeStepFlag'
					RITimestepTSData.setIVariablesScanned(true);
					break;
				case 1:  // at 'EndHourFlag'
					RIHourlyTSData.setIVariablesScanned(true);
					break;
				case 2: // at 'EndDayFlag'
					RIDailyTSData.setIVariablesScanned(true);
					break;
				case 3:  // at end of month
					RIMonthlyTSData.setIVariablesScanned(true);
					break;
				case 4:  // once per environment 'EndEnvrnFlag'
					RIRunPeriodTSData.setIVariablesScanned(true);
					break;
			}
		}

		void ResultsSchema::initializeMeters(const Array1D< OutputProcessor::MeterType > &EnergyMeters, const int ReportFrequency) {
			switch (ReportFrequency) {
				case -1:
					//nothing to do; meters are not reported at this frequency
					break;
				case 0:  // at 'Timestep'
					for (int Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
						if (EnergyMeters(Loop).RptTS || EnergyMeters(Loop).RptTSFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).TSRptNum, EnergyMeters(Loop).Units);
							TSMeters.addVariable(var);
							TSMeters.setRDataFrameEnabled(true);
						}
						if (EnergyMeters(Loop).RptAccTS || EnergyMeters(Loop).RptAccTSFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).TSAccRptNum, EnergyMeters(Loop).Units);
							TSMeters.addVariable(var);
							TSMeters.setRDataFrameEnabled(true);
						}
					}
					break;
				case 1:  // at 'Hourly'
					for (int Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
						if (EnergyMeters(Loop).RptHR || EnergyMeters(Loop).RptHRFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).HRRptNum, EnergyMeters(Loop).Units);
							HRMeters.addVariable(var);
							HRMeters.setRDataFrameEnabled(true);
						}
						if (EnergyMeters(Loop).RptAccHR || EnergyMeters(Loop).RptAccHRFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).HRAccRptNum, EnergyMeters(Loop).Units);
							HRMeters.addVariable(var);
							HRMeters.setRDataFrameEnabled(true);
						}
					}
					break;
				case 2:  // at 'Daily'
					for (int Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
						if (EnergyMeters(Loop).RptDY || EnergyMeters(Loop).RptDYFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).DYRptNum, EnergyMeters(Loop).Units);
							DYMeters.addVariable(var);
							DYMeters.setRDataFrameEnabled(true);
						}
						if (EnergyMeters(Loop).RptAccDY || EnergyMeters(Loop).RptAccDYFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).DYAccRptNum, EnergyMeters(Loop).Units);
							DYMeters.addVariable(var);
							DYMeters.setRDataFrameEnabled(true);
						}
					}
					break;
				case 3:  // at 'Monthly'
					for (int Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
						if (EnergyMeters(Loop).RptMN || EnergyMeters(Loop).RptMNFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).MNRptNum, EnergyMeters(Loop).Units);
							MNMeters.addVariable(var);
							MNMeters.setRDataFrameEnabled(true);
						}
						if (EnergyMeters(Loop).RptAccMN || EnergyMeters(Loop).RptAccMNFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).MNAccRptNum, EnergyMeters(Loop).Units);
							MNMeters.addVariable(var);
							MNMeters.setRDataFrameEnabled(true);
						}
					}
					break;
				case 4:  // at 'RunPeriod'/'SM'
					for (int Loop = 1; Loop <= EnergyMeters.size(); ++Loop) {
						if (EnergyMeters(Loop).RptSM || EnergyMeters(Loop).RptSMFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).SMRptNum, EnergyMeters(Loop).Units);
							SMMeters.addVariable(var);
							SMMeters.setRDataFrameEnabled(true);
						}
						if (EnergyMeters(Loop).RptAccSM || EnergyMeters(Loop).RptAccSMFO) {
							MeterVariable *var = new MeterVariable(EnergyMeters(Loop).Name, ReportFrequency, EnergyMeters(Loop).SMAccRptNum, EnergyMeters(Loop).Units);
							SMMeters.addVariable(var);
							SMMeters.setRDataFrameEnabled(true);
						}
					}
					break;
			}

			// set the scanned variables to true or false
			switch (ReportFrequency)
			{
			case -1:
				// case should not happen in Meters
				break;
			case 0:  // at TimeStepFlag
				TSMeters.setRVariablesScanned(true);
				break;
			case 1:  // at Hourly
				HRMeters.setRVariablesScanned(true);
				break;
			case 2: // at Daily
				DYMeters.setRVariablesScanned(true);
				break;
			case 3:  // at Monthly
				MNMeters.setRVariablesScanned(true);
				break;
			case 4:  // at RunPeriod/SM
				SMMeters.setRVariablesScanned(true);
				break;
			}
		}

		void ResultsSchema::writeTimeSeriesFiles()
		{
			// Output detailed Zone time series data
			if (OutputSchema->RIDetailedZoneTSData.rDataFrameEnabled() || OutputSchema->RIDetailedZoneTSData.iDataFrameEnabled())
				OutputSchema->RIDetailedZoneTSData.writeFile();

			// Output detailed HVAC time series data
			if (OutputSchema->RIDetailedHVACTSData.iDataFrameEnabled() || OutputSchema->RIDetailedHVACTSData.rDataFrameEnabled())
				OutputSchema->RIDetailedHVACTSData.writeFile();

			// Output timestep time series data
			if (OutputSchema->RITimestepTSData.iDataFrameEnabled() || OutputSchema->RITimestepTSData.rDataFrameEnabled())
				OutputSchema->RITimestepTSData.writeFile();

			// Output hourly time series data
			if (OutputSchema->RIHourlyTSData.iDataFrameEnabled() || OutputSchema->RIHourlyTSData.rDataFrameEnabled())
				OutputSchema->RIHourlyTSData.writeFile();

			// Output daily time series data
			if (OutputSchema->RIDailyTSData.iDataFrameEnabled() || OutputSchema->RIDailyTSData.rDataFrameEnabled())
				OutputSchema->RIDailyTSData.writeFile();

			// Output monthly time series data
			if (OutputSchema->RIMonthlyTSData.iDataFrameEnabled() || OutputSchema->RIMonthlyTSData.rDataFrameEnabled())
				OutputSchema->RIMonthlyTSData.writeFile();

			// Output run period time series data
			if (OutputSchema->RIRunPeriodTSData.iDataFrameEnabled() || OutputSchema->RIRunPeriodTSData.rDataFrameEnabled())
				OutputSchema->RIRunPeriodTSData.writeFile();
		}

		char* ResultsSchema::convert(const std::string & s) {
			char *pc = new char[s.size() + 1];
			std::strcpy(pc, s.c_str());
			return pc;
		}

		void ResultsSchema::writeFile()
		{
			std::string jsonfilename = "eplusout.json";
			std::ofstream jsonfile(jsonfilename);

			cJSON *_root, *_simRes, *outputVars, *meterVars, *meterData, *mdd;

			_root = cJSON_CreateObject();

			// simulation results
			cJSON_AddItemToObject(_root, "SimulationResults", _simRes = cJSON_CreateObject());
			cJSON_AddItemToObject(_simRes, "Simulation", SimulationInformation.getJSON());
			
			// output variables
			cJSON_AddItemToObject(_root, "OutputVariables", outputVars = cJSON_CreateObject());
			
			if (RIDetailedZoneTSData.iDataFrameEnabled() || RIDetailedZoneTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Detailed-Zone", RIDetailedZoneTSData.getVariablesJSON());
			
			if (RIDetailedHVACTSData.iDataFrameEnabled() || RIDetailedHVACTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Detailed-HVAC", RIDetailedHVACTSData.getVariablesJSON());

			if (RITimestepTSData.iDataFrameEnabled() || RITimestepTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Timestep", RITimestepTSData.getVariablesJSON());

			if (RIHourlyTSData.iDataFrameEnabled() || RIHourlyTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Hourly", RIHourlyTSData.getVariablesJSON());

			if (RIDailyTSData.iDataFrameEnabled() || RIDailyTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Daily", RIDailyTSData.getVariablesJSON());

			if (RIMonthlyTSData.iDataFrameEnabled() || RIMonthlyTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "Monthly", RIMonthlyTSData.getVariablesJSON());

			if (RIRunPeriodTSData.iDataFrameEnabled() || RIRunPeriodTSData.rDataFrameEnabled())
				cJSON_AddItemToObject(outputVars, "RunPeriod", RIRunPeriodTSData.getVariablesJSON());

			// output dictionary
			cJSON *rdd, *rddvals;
			cJSON_AddItemToObject(outputVars, "OutputDictonary", rdd = cJSON_CreateObject());
			cJSON_AddStringToObject(rdd, "Description", "Dictionary containing output variables that may be requested");
			cJSON_AddItemToObject(rdd, "Variables", rddvals = cJSON_CreateArray());
			for (int i = 0; i < RDD.size(); i++) {
				cJSON* str = cJSON_CreateString(RDD[i].c_str());
				cJSON_AddItemToArray(rddvals, str);
			}


			// meter variables
			cJSON_AddItemToObject(_root, "MeterVariables", meterVars = cJSON_CreateObject());
			
			// -- meter values
			if (TSMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterVars, "Timestep", TSMeters.getVariablesJSON());

			if (HRMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterVars, "Hourly", HRMeters.getVariablesJSON());
			
			if (DYMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterVars, "Daily", DYMeters.getVariablesJSON());
			
			if (MNMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterVars, "Monthly", MNMeters.getVariablesJSON());

			if (SMMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterVars, "RunPeriod", SMMeters.getVariablesJSON());

			cJSON_AddItemToObject(_root, "MeterData", meterData = cJSON_CreateObject());

			if (TSMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterData, "Timestep", TSMeters.getJSON());

			if (HRMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterData, "Hourly", HRMeters.getJSON());

			if (DYMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterData, "Daily", DYMeters.getJSON());

			if (MNMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterData, "Monthly", MNMeters.getJSON());

			if (SMMeters.rDataFrameEnabled())
				cJSON_AddItemToObject(meterData, "RunPeriod", SMMeters.getJSON());

			// -- meter dictionary
			cJSON_AddItemToObject(meterVars, "MeterDictonary", mdd = cJSON_CreateObject());
			cJSON_AddStringToObject(mdd, "Description", "Dictionary containing meter variables that may be requested");
			cJSON *mddvals;
			cJSON_AddItemToObject(mdd, "Meters", mddvals = cJSON_CreateArray());
			for (int i = 0; i < MDD.size(); i++) {
				cJSON* str = cJSON_CreateString(MDD[i].c_str());
				cJSON_AddItemToArray(mddvals, str);
			}

			// reports

			cJSON *reports;
			cJSON_AddItemToObject(_root, "TabularReports", reports = TabularReportsCollection.getJSON());

			

			// write json
			if (jsonfile.is_open())	{
				jsonfile << cJSON_Print(_root);
				jsonfile.close();
			}

			cJSON_Delete(_root);
		}
	} // ResultsFramework

} // EnergyPlus
