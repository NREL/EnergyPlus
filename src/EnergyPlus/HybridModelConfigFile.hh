#ifndef ConfigFile_hh_INCLUDED
#define ConfigFile_hh_INCLUDED
#include <iostream>  
#include <string>
#include <list>
#include <map>
#include <vector>
//#include <math.h>       /* floor */
#include <fstream>

using namespace std;
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

	namespace HybridModelConfigFile {

		class ConfigDataItem
		{
		public:
			string ID;
			vector<double> Items;
		};
		class ConfigFile                   // begin declaration of the class
		{
		public:                    // begin public section
			ConfigFile();     // constructor
				//~ConfigFile(){};                  // destructor
			void ProcessLine(string indata);
			void ParseConfigFile(string ConfigFileName);
			void SplitFilename(const string& str, string& directory, string& file);
			void ConfigFile::TrimFilename(const string& str, string& Trimmed);
			bool ImportItem(string datakey, string ID_TAD, string indata, vector<double> * container);
			bool GetNextOpConst(vector<double>& XVals, vector<double>& YVals);
			double GetBcoefficient(int mode, int ID);
			ConfigDataItem* ConfigFile::GetBcoefficientCurve(int CurveID);
			ConfigDataItem* ConfigFile::GetEnvSet(int ModeNumber);
			double ConfigFile::GetECLow(int EnvParamID, int ModeNumber);
			double ConfigFile::GetECHi(int EnvParamID, int ModeNumber);
			double GetRatedMsa();
			double GetMinVR();
			double GetRatedH();
			std::fstream infile;
		private:                   // begin private section
			vector<double> SystemPerformanceParameters; //SYSTEM_PERFORMANCE_ID_TAG
			vector<ConfigDataItem> PerformanceCurves;
			map<string, vector<double>> OperationalConstraintsX;
			map<string, vector<double>> OperationalConstraintsY;
			vector<ConfigDataItem> EnvContraints;
			map<std::string, vector<double>>::iterator iterIndex_X;
			map<std::string, vector<double>>::iterator iterIndex_Y;

			int Indexcount;

		};
	}
}

#endif // ObjexxFCL_Array1D_hh_INCLUDED