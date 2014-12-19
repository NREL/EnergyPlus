// C++ Headers
#include <cmath>
#include <string>
#include <iostream>
#include <fstream>

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
#include <General.hh>
#include <GlobalNames.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

	namespace ResultsSchema {

		using namespace DataHVACGlobals;
		using namespace DataPrecisionGlobals;
		using DataGlobals::InitConvTemp;
		using DataGlobals::SecInHour;
		using DataGlobals::DisplayExtraWarnings;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using OutputProcessor::RealVariables;
		using OutputProcessor::RealVariableType;

		static gio::Fmt fmtLD("*");

		DataFrame::DataFrame(std::string ReportFreq) {
			//UUID = getUUID();
			ReportFrequency = ReportFreq;
			CurrentRow = -1;
			RDataFrameEnabled = false;
			IDataFrameEnabled = false;
		}

		DataFrame::~DataFrame() {

		}

		void DataFrame::AddRCol(std::string VarName, std::string VarUnits) {
			RCols.push_back(std::make_pair(VarName, VarUnits));
		}

		void DataFrame::AddICol(std::string VarName, std::string VarUnits) {
			ICols.push_back(std::make_pair(VarName, VarUnits));
		}

		void DataFrame::NewRow(std::string ts) {
			std::vector<double> rrow;
			std::vector<int> irow;
			RRows.push_back( rrow );
			IRows.push_back( irow );
			TS.push_back(ts);
			CurrentRow++;
		}

		void DataFrame::AddToCurrentRRow(double value) {
			RRows[CurrentRow].push_back(value);
		}

		void DataFrame::AddToCurrentIRow(int value) {
			IRows[CurrentRow].push_back(value);
		}

		void DataFrame::WriteFile() {
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
		DataFrame ResultsSchema::RIDetailedZoneTSData("Detailed-Zone"), 
			ResultsSchema::RIDetailedHVACTSData("Detailed-HVAC"), 
			ResultsSchema::RITimestepTSData("Timestep"), 
			ResultsSchema::RIHourlyTSData("Hourly"), 
			ResultsSchema::RIDailyTSData("Daily"), 
			ResultsSchema::RIMonthlyTSData("Monthly"), 
			ResultsSchema::RIRunPeriodTSData("RunPeriod");
		

		ResultsSchema::ResultsSchema() {
			//UUID = getUUID();
		}

		ResultsSchema::~ResultsSchema() {

		}

		void ResultsSchema::InitializeRTSDataFrame(const int ReportFrequency, const FArray1D< RealVariableType > &RVariableTypes, const int NumOfRVariable, const int IndexType) {
			Reference< RealVariables > RVar;

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
								RIDetailedZoneTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							}
							else if (IndexType == HVACVar && RVariableTypes(Loop).IndexType == HVACVar)
							{
								RIDetailedHVACTSData.RDataFrameEnabled = true;
								RIDetailedHVACTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							}
							break;
						case 0:  // at 'EndTimeStepFlag'
							RITimestepTSData.RDataFrameEnabled = true;
							RITimestepTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 1:  // at 'EndHourFlag'
							RIHourlyTSData.RDataFrameEnabled = true;
							RIHourlyTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 2: // at 'EndDayFlag'
							RIDailyTSData.RDataFrameEnabled = true;
							RIDailyTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 3:  // at end of month
							RIMonthlyTSData.RDataFrameEnabled = true;
							RIMonthlyTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
						case 4:  // once per environment 'EndEnvrnFlag'
							RIRunPeriodTSData.RDataFrameEnabled = true;
							RIRunPeriodTSData.AddRCol(RVariableTypes(Loop).VarName, RVariableTypes(Loop).UnitsString);
							break;
					}
				}
			}
		}

		void ResultsSchema::InitializeITSDataFrame(const int ReportFrequency, const FArray1D< IntegerVariableType > &IVariableTypes, const int NumOfIVariable, const int IndexType) {
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
							RIDetailedZoneTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						}
						else if (IndexType == HVACVar && IVariableTypes(Loop).IndexType == HVACVar)
						{
							RIDetailedZoneTSData.IDataFrameEnabled = true;
							RIDetailedZoneTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						}
						break;
					case 0:  // at 'EndTimeStepFlag'
						RITimestepTSData.IDataFrameEnabled = true;
						RITimestepTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 1:  // at 'EndHourFlag'
						RIHourlyTSData.IDataFrameEnabled = true;
						RIHourlyTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 2: // at 'EndDayFlag'
						RIDailyTSData.IDataFrameEnabled = true;
						RIDailyTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 3:  // at end of month
						RIMonthlyTSData.IDataFrameEnabled = true;
						RIMonthlyTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					case 4:  // once per environment 'EndEnvrnFlag'
						RIRunPeriodTSData.IDataFrameEnabled = true;
						RIRunPeriodTSData.AddICol(IVariableTypes(Loop).VarName, IVariableTypes(Loop).UnitsString);
						break;
					}
				}
			}
		}

	} // ResultsSchema

} // EnergyPlus
