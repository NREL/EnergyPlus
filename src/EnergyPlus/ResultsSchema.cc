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
#include <DataGlobals.hh>

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

		json SimInfo::getJSON()
		{
			json root = {
					{ "UUID", uuid },
					{ "ProgramVersion", ProgramVersion },
					{ "SimulationEnvironment", SimulationEnvironment },
					{ "InputModelURI", InputModelURI },
					{ "StartDateTimeStamp", StartDateTimeStamp },
					{ "RunTime", RunTime },
					{ "ErrorSummary", {
							{ "NumWarnings" , NumWarnings },
							{ "NumSevere" , NumSevere }
					 }},
					{ "ErrorSummaryWarmup", {
							{ "NumWarnings" , NumWarningsDuringWarmup },
							{ "NumSevere" , NumSevereDuringWarmup }
				     }},
					{ "ErrorSummarySizing", {
							{ "NumWarnings" , NumWarningsDuringSizing },
							{ "NumSevere" , NumSevereDuringSizing }
					 }}
			};
			return root;
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

		json Variable::getJSON() {
			json root = {
					{ "UUID" , uuid },
					{ "Name" , varName },
					{ "Units" , Units },
					{ "Frequency", sReportFreq }
			};
			return root;
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

		json MeterVariable::getJSON() {
			json root = Variable::getJSON();
			if (acc)
				root[ "Cumulative" ] = "True";
			return root;
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

		json DataFrame::getVariablesJSON() {
			json arr = json::array();

			//for (int i = 0; i < variables().size(); i++);
			for (auto it = variableMap.begin(); it != variableMap.end(); ++it)
				arr.push_back(it->second->getJSON());
			return arr;
		}

		json DataFrame::getJSON() {
			json root;
			json cols = json::array();
			json rows = json::array();

			for (auto it = variableMap.begin(); it != variableMap.end(); ++it) {
				cols.push_back( {
						               { "Variable" , it->second->variableName() },
						               { "Units" , it->second->units() },
						               { "UUID" , it->second->UUID() }
				               }
				);
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
				rows.push_back( {
                                        {TS.at(row) , vals }
                                } );
			}
			root = {
					{ "UUID" , uuid },
					{ "ReportFrequency" , ReportFrequency },
					{ "Cols" , cols },
					{ "Rows" , rows }
			};
			return root;
		}

		void DataFrame::writeReport() {
			//std::string jsonfilename =  DataStringGlobals::outputDirPathName + "eplusout_" + ReportFrequency + ".json";
			//std::string jsonfilename =  "eplusout_" + ReportFrequency + ".json";
			//std::ofstream jsonfile(jsonfilename);
			
			json root;
			
//			if (jsonfile.is_open())
//			{
//				root = getJSON();
//				jsonfile << std::setw(4) << root << std::endl;
//				jsonfile.close();
//			}
//
//			if (DataGlobals::json_stream){
//				root = getJSON();
//				*(DataGlobals::json_stream) << std::setw(4) << root << std::endl;
//			}

			// does this need to go to error?
			//else
			//	ShowWarningError("Unable to open file for time-series output.");
			//cJSON_Delete(_root);
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

		json Table::getJSON() {
			json root;
			json cols = json::array();
			json rows;

			for (int i = 0; i < ColHeaders.size(); i++) {
				cols.push_back(ColHeaders[i]);
			}

			for (int row = 0; row < RowHeaders.size(); ++row) {
				json rowvec = json::array();
				for (int col = 0; col < ColHeaders.size(); ++col) {
					rowvec.push_back( Data[col][row] );
				}
				rows[ RowHeaders[row] ] = rowvec;
			}

			root = {
					{ "TableName" , TableName },
					{ "UUID" , UUID() },
					{ "Cols", cols },
					{ "Rows", rows }
			};
			if (!FootnoteText.empty())
				root["Footnote"] =  FootnoteText;
			return root;
		}

		// class Report

		json Report::getJSON() {

			json root = {
					{ "ReportName" , ReportName },
					{ "For" , ReportForString },
					{ "UUID" , UUID() }
			};

			json cols = json::array();

			for (int i = 0; i < Tables.size(); i++) {
				cols.push_back(Tables[i]->getJSON());
			}
			root["Tables"] = cols;
			return root;
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

		json ReportsCollection::getJSON() {
			json root = json::array();

			for (RptPtrPair iter : reportsMap) {
				root.push_back( iter.second->getJSON() );
			}
			return root;
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

		void ResultsSchema::writeTimeSeriesReports()
		{
			// Output detailed Zone time series data
			if (OutputSchema->RIDetailedZoneTSData.rDataFrameEnabled() || OutputSchema->RIDetailedZoneTSData.iDataFrameEnabled())
				OutputSchema->RIDetailedZoneTSData.writeReport();

			// Output detailed HVAC time series data
			if (OutputSchema->RIDetailedHVACTSData.iDataFrameEnabled() || OutputSchema->RIDetailedHVACTSData.rDataFrameEnabled())
				OutputSchema->RIDetailedHVACTSData.writeReport();

			// Output timestep time series data
			if (OutputSchema->RITimestepTSData.iDataFrameEnabled() || OutputSchema->RITimestepTSData.rDataFrameEnabled())
				OutputSchema->RITimestepTSData.writeReport();

			// Output hourly time series data
			if (OutputSchema->RIHourlyTSData.iDataFrameEnabled() || OutputSchema->RIHourlyTSData.rDataFrameEnabled())
				OutputSchema->RIHourlyTSData.writeReport();

			// Output daily time series data
			if (OutputSchema->RIDailyTSData.iDataFrameEnabled() || OutputSchema->RIDailyTSData.rDataFrameEnabled())
				OutputSchema->RIDailyTSData.writeReport();

			// Output monthly time series data
			if (OutputSchema->RIMonthlyTSData.iDataFrameEnabled() || OutputSchema->RIMonthlyTSData.rDataFrameEnabled())
				OutputSchema->RIMonthlyTSData.writeReport();

			// Output run period time series data
			if (OutputSchema->RIRunPeriodTSData.iDataFrameEnabled() || OutputSchema->RIRunPeriodTSData.rDataFrameEnabled())
				OutputSchema->RIRunPeriodTSData.writeReport();
		}

		char* ResultsSchema::convert(const std::string & s) {
			char *pc = new char[s.size() + 1];
			std::strcpy(pc, s.c_str());
			return pc;
		}

		void ResultsSchema::WriteReport() {
			json root, outputVars, rdd, meterVars, meterData;
			json rddvals = json::array();
			root = {
					{ "SimulationResults", {
							{"Simulation" , SimulationInformation.getJSON()}
										   }
					}
			};

			//output variables
			if (RIDetailedZoneTSData.iDataFrameEnabled() || RIDetailedZoneTSData.rDataFrameEnabled())
				outputVars[ "Detailed-Zone" ] = RIDetailedZoneTSData.getVariablesJSON();
			
			if (RIDetailedHVACTSData.iDataFrameEnabled() || RIDetailedHVACTSData.rDataFrameEnabled())
				outputVars[ "Detailed-HVAC" ] = RIDetailedHVACTSData.getVariablesJSON();

			if (RITimestepTSData.iDataFrameEnabled() || RITimestepTSData.rDataFrameEnabled())
				outputVars[ "Timestep" ] = RITimestepTSData.getVariablesJSON();

			if (RIHourlyTSData.iDataFrameEnabled() || RIHourlyTSData.rDataFrameEnabled())
				outputVars[ "Hourly" ] = RIHourlyTSData.getVariablesJSON();

			if (RIDailyTSData.iDataFrameEnabled() || RIDailyTSData.rDataFrameEnabled())
				outputVars[ "Daily" ], RIDailyTSData.getVariablesJSON();

			if (RIMonthlyTSData.iDataFrameEnabled() || RIMonthlyTSData.rDataFrameEnabled())
				outputVars[ "Monthly" ] = RIMonthlyTSData.getVariablesJSON();

			if (RIRunPeriodTSData.iDataFrameEnabled() || RIRunPeriodTSData.rDataFrameEnabled())
				outputVars[ "RunPeriod" ] = RIRunPeriodTSData.getVariablesJSON();

			// output dictionary
            for (int i = 0; i < RDD.size(); i++) {
                rddvals.push_back( RDD[i] );
            }
            rdd = {
					{ "Description", "Dictionary containing output variables that may be requested" },
					{ "Variables", rddvals}
			};
			outputVars [ "OutputDictionary" ] = rdd;

			// meter variables
			
			// -- meter values
			if (TSMeters.rDataFrameEnabled())
				meterVars[ "Timestep"] = TSMeters.getVariablesJSON();

			if (HRMeters.rDataFrameEnabled())
				meterVars[ "Hourly" ] = HRMeters.getVariablesJSON();
			
			if (DYMeters.rDataFrameEnabled())
				meterVars[  "Daily" ] = DYMeters.getVariablesJSON();
			
			if (MNMeters.rDataFrameEnabled())
				meterVars[  "Monthly" ] = MNMeters.getVariablesJSON();

			if (SMMeters.rDataFrameEnabled())
				meterVars[  "RunPeriod" ] = SMMeters.getVariablesJSON();

			if (TSMeters.rDataFrameEnabled())
				meterData[ "Timestep"] = TSMeters.getJSON();

			if (HRMeters.rDataFrameEnabled())
				meterData[ "Hourly" ] = HRMeters.getJSON();

			if (DYMeters.rDataFrameEnabled())
				meterData[ "Daily" ] = DYMeters.getJSON();

			if (MNMeters.rDataFrameEnabled())
				meterData[ "Monthly" ] = MNMeters.getJSON();

			if (SMMeters.rDataFrameEnabled())
				meterData[ "RunPeriod" ] = SMMeters.getJSON();

			json mdd;
			json mddvals = json::array();

			// -- meter dictionary
			for (int i = 0; i < MDD.size(); i++) {
				mddvals.push_back( MDD[i] );
			}
			mdd = {
					{ "Description" , "Dictionary containing meter variables that may be requested" },
					{ "Meters" , mddvals }
			};

			meterVars["MeterDictionary"] = mdd;

			root [ "OutputVariables" ] = outputVars;
            root [ "MeterVariables" ] = meterVars;
			root [ "MeterData" ] = meterData;

			// reports
			root[ "TabularReports" ] = TabularReportsCollection.getJSON();

			if(DataGlobals::json_stream){
				*(DataGlobals::json_stream) << std::setw(4) << root << std::endl;
				gio::close(DataGlobals::OutputFileJson);
				DataGlobals::json_stream = nullptr;
			}
		}
	} // ResultsFramework

} // EnergyPlus
