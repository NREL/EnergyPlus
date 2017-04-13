#include <iostream>
#include <string>
#include <fstream>
#include <HybridModelConfigFile.hh>
#include <map>
#include <math.h>       /* floor */
#include <string>
#include <list>
#include <windows.h>
#define PERFORMANCE_CURVE_ID_TAG "PERFRM_CRV"
#define OPERATIONAL_CONTRAINT_ID_TAG "OP_CON"
#define ENVIRONMENTAL_CONTRAINT_ID_TAG "ENV_CON"
#define SYSTEM_PERFORMANCE_ID_TAG "SYSTEM_INPUTS"
//#include <hash>
//std::fstream infile;
using namespace std;
list<string> line;
vector<double> curve;
vector<double> OpContraints;
vector<double> envContraint;

//Rated Msa  (m3/s)	Minium Ventialtion rate Msa  (m3/s)	Rated H (kw)

//list<curve*> PerformanceCurves;

namespace EnergyPlus {

	namespace HybridModelConfigFile {

		ConfigFile::ConfigFile()
		{
			Indexcount = 1;
		}

		void ConfigFile::ParseConfigFile(string ConfigFileName)
		{
			char str[2000];
			string indata;
			ConfigFileName = ConfigFileName + "\\Config.csv"; //\\resources\\HybridModelConfig
			//infile.open("V:\\LBNL_WCEC\\FMUDev\\fmusdk\\fmu10\\fmu\\cs\\HybridModelConfig\\Config.csv",ios::in|ios::out);
		//	cout <<"tried to open"<<ConfigFileName.c_str();
		//	Sleep(1000);
			infile.open(ConfigFileName.c_str(), ios::in | ios::out);
			if (!infile.is_open())
			{
				cout << "tried to open and failed";
				Sleep(40000);
				return;
			}
			infile.getline(str, 2000);

			while (!infile.eof())
			{
				infile.getline(str, 2000);
				indata = str;
				ProcessLine(indata);
			}
			//	cout <<"End of config file";
			infile.close();			//close it
			iterIndex_X = OperationalConstraintsX.begin();
			iterIndex_Y = OperationalConstraintsY.begin();
		}

		void ConfigFile::ProcessLine(string indata)
		{

			string::size_type pos;
			pos = 0;
			string left;
			pos = indata.find(',', 0);
			string DataKey = indata.substr(0, pos);
			indata = indata.substr(pos + 1);

			pos = indata.find(',', 0);
			string ModeName = indata.substr(0, pos);
			indata = indata.substr(pos + 1);

			ImportItem(DataKey, string(SYSTEM_PERFORMANCE_ID_TAG), indata, &SystemPerformanceParameters);

			if (ImportItem(DataKey, string(PERFORMANCE_CURVE_ID_TAG), indata, &curve))
			{
				ConfigDataItem CurveItem;
				CurveItem.ID = DataKey;
				CurveItem.Items = curve;
				PerformanceCurves.push_back(CurveItem);
				curve.clear();
			}

			if (ImportItem(DataKey, string(OPERATIONAL_CONTRAINT_ID_TAG), indata, &OpContraints))
			{
				if (DataKey.find("X") != -1)
				{
					OperationalConstraintsX[DataKey] = OpContraints;
				}
				if (DataKey.find("Y") != -1) {
					OperationalConstraintsY[DataKey] = OpContraints;
				}
				OpContraints.clear();
			}

			if (ImportItem(DataKey, string(ENVIRONMENTAL_CONTRAINT_ID_TAG), indata, &envContraint))
			{
				ConfigDataItem CurveItem;
				CurveItem.ID = DataKey;
				CurveItem.Items = envContraint;
				EnvContraints.push_back(CurveItem);
				envContraint.clear();
			}
		}
		double ConfigFile::GetRatedMsa()
		{
			int count = SystemPerformanceParameters.size();
			if (count == 0)
			{
				cout << "error no SystemPerformanceParameters read from config file \n";
				Sleep(100000);
				return 0;
			}
			double val = SystemPerformanceParameters[0];
			return val;
		}
		double ConfigFile::GetMinVR()
		{
			int count = SystemPerformanceParameters.size();
			if (count == 0)
			{
				cout << "error no SystemPerformanceParameters read from config file \n";
				Sleep(100000);
				return 0;
			}
			double val = SystemPerformanceParameters[1];
			return val;
		}
		double ConfigFile::GetRatedH()
		{
			int count = SystemPerformanceParameters.size();
			if (count == 0)
			{
				cout << "error no SystemPerformanceParameters read from config file \n";
				Sleep(100000);
				return 0;
			}
			double val = SystemPerformanceParameters[2];
			return val;
		}

		double ConfigFile::GetBcoefficient(int mode, int ID)
		{
			int count = PerformanceCurves.size();
			if (count == 0)
			{
				cout << "error no Bcoefficient read from config file \n";
				Sleep(100000);
				return 0;
			}
			ConfigDataItem modeCurve = PerformanceCurves[mode];
			double testv = modeCurve.Items[ID];
			return testv;
		}

		double ConfigFile::GetECLow(int EnvParamID, int ModeNumber)
		{
			ConfigDataItem * Env = GetEnvSet(ModeNumber);
			int index = EnvParamID * 2;
			double val = Env->Items[index];
			return val;
		}

		double ConfigFile::GetECHi(int EnvParamID, int ModeNumber)
		{

			ConfigDataItem* Env = GetEnvSet(ModeNumber);
			int index = EnvParamID * 2;
			double val = Env->Items[index + 1];
			return val;
		}

		ConfigDataItem* ConfigFile::GetBcoefficientCurve(int CurveID)
		{
			int count = PerformanceCurves.size();
			if (count == 0 || (CurveID > count))
			{
				cout << "error in call to request B coefficient curve\n";
				Sleep(100000);
				return 0;
			}
			ConfigDataItem* modeCurve = &(PerformanceCurves[CurveID]);
			return modeCurve;
		}
		ConfigDataItem* ConfigFile::GetEnvSet(int ModeNumber)
		{
			int count = PerformanceCurves.size();
			if (count == 0 || (ModeNumber > count))
			{
				cout << "error in call to request B coefficient curve\n";
				Sleep(100000);
				return 0;
			}
			ConfigDataItem* modeCurve = &(EnvContraints[ModeNumber]);
			return modeCurve;
		}


		bool ConfigFile::GetNextOpConst(vector<double> &XVals, vector<double>& YVals)
		{

			int count = OperationalConstraintsX.size();
			if (count == 0)
			{
				cout << "error no operational contraints read from config file \n";
				Sleep(100000);
				return 0;
			}
			if (Indexcount > count)
			{
				iterIndex_X = OperationalConstraintsX.begin();
				iterIndex_Y = OperationalConstraintsY.begin();
				return 0;
			}
			XVals = (iterIndex_X->second);
			YVals = (iterIndex_Y->second);
			++iterIndex_X;
			++iterIndex_Y;
			Indexcount++;

			return 1;
		}


		bool ConfigFile::ImportItem(string DataKey, string ID_TAD, string indata, vector<double> * container)
		{
			string::size_type pos;
			pos = 0;
			string left;
			if (DataKey.find(ID_TAD) != -1)
			{
				while (pos != -1)
				{
					pos = indata.find(',', 0);
					if (pos != -1)
					{
						left = indata.substr(0, pos);
						double point = atof(left.c_str());//bug
					//	point=round(point,8);
					//	point=GetFloatPrecision(point,8);
						container->push_back(point);
						indata = indata.substr(pos + 1);
					}
					else
					{
						double point = atof(indata.c_str());
						container->push_back(point);
						//	point=GetFloatPrecision(point,8);
					}
				}
				return true;
			}
			return false;
		}

		void ConfigFile::SplitFilename(const string& str, string& directory, string& file)
		{
			size_t found;
			found = str.find_last_of("/\\");
			directory = str.substr(0, found);
			file = str.substr(found + 1);
		}

		void ConfigFile::TrimFilename(const string& str, string& Trimmed)
		{
			size_t length;
			length = str.length();
			Trimmed = str.substr(0, length - 1);
		}
	}
}
