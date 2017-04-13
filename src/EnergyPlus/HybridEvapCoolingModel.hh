#include <iostream>  
//#include <fmiPlatformTypes.h>
#include <HybridModelConfigFile.hh>
using namespace std;
#include <string>
#include <list>
#include <map>
#include <vector>
//#include <math.h>       /* floor */

using namespace std;

#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

	namespace HybridEvapCoolingModel {
		using namespace HybridModelConfigFile;
		class CModeSolutionSpace
		{
		public:
			vector<double> PointX;
			vector<double> PointY;
			vector<double> PointMeta;
		};


		class Model                   // begin declaration of the class
		{
		public:                    // begin public section
			Model();     // constructor
			~Model();                  // destructor
			int GetID();            // accessor function
			void SetID(int vID) { ID = vID; };    // accessor function
			void doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double CapacityRatedCond, int CapacityFlag, double DesignMinVR, double rTestFlag, double *returnQSensible, double *returnQLatent, double *returnSupplyAirMassFlow, double *returnSupplyAirTemp, double *returnSupplyAirRelHum, double *returnVentilationAir, int *FMUmode, double *ElectricalPowerUse, double communicationStepSize, int *bpErrorCode);
			void Initialize(string fmuLocation);
			void Model::InitializeModelParams();
			void ModelLog(std::string fmuLocation);
			CModeSolutionSpace* Model::Tessellate(vector<double> & Xvals, vector<double> & Yvals);
			double Model::CalcHum_ratio_W(double Tdb, double RH, double P);
			bool Model::MeetsOAEnvConstraints(double Tosa, double Wosa, double RHosa, int ModeNumber);
			bool Model::MeetsSupplyAirTOC(double Tosa);
			bool Model::MeetsSupplyAirRHOC(double Wosa);
			double Model::CalculateCurveVal(double X_0, double X_1, double X_2, double X_3, double X_4, double X_5, double X_6, int mode_number, int curve_ID);
			double Model::EstimateQRemaining(double TroomTemp, Real64 communicationStepSize);
			double Model::CalculateSupplyAirDBTempAtRefCon();
			double Model::CalculateMixedAirTemp();
			double Model::CheckVal_T(double T);
			double Model::CheckVal_W(double W);
			ConfigFile Config;
			// void Meow();
		private:                   // begin private section
			int ID;              // member variable
			char * string;
			//holds the X and Y points of the possible sollutions within the operating conditions. Int is the mode number
			list<CModeSolutionSpace*> XandYPoints;
			double Round(double x);
			double Model::Sat_press(double Tdb);
			double Model::Part_press(double P, double W);
			double ResolutionX;
			double ResolutionY;

			//system parameters

		//this is the requested ventilation rate, and should be read in from ePlus for specific time step, similar to the requested sensible load
			int count_EnvironmentConditionsMetOnce;
			int count_SAHR_OC_MetOnce;
			int count_SAT_OC_MetOnce;
			int count_DidWeMeetLoad;
			double MinOA_Msa;
			double MsaRated;
			double RatedH;
			double optimal_power;
			//model variables
			double cp;
			double Lambna;
			bool optimal_EnvCondMet;
			bool RunningPeakCapacity_EnvCondMet;
			double optimal_Msa;
			double optimal_OSAF;
			//    optimal_power = 100000000
			double optimal_H_sensible_room;
			double optimal_TotalSystemH;
			double optimal_SHR;
			double optimal_Mvent;
			int optimal_Mode;
			double optimal_Wsa;
			double optimal_Tsa;
			double optimal_Point; //this is used to identify the point that
			double Tsa;
			double Wsa;
			bool DidWeMeetLoad;
			double RunningPeakCapacity_power;
			double RunningPeakCapacity_Msa;
			double RunningPeakCapacity_OSAF;
			double RunningPeakCapacity_H_sensible_room;
			double RunningPeakCapacity_TotalSystemH;
			double RunningPeakCapacity_SHR;
			double RunningPeakCapacity_Mvent;
			double RunningPeakCapacity_Mode;
			double Minimum_Supply_Air_Temp;
			double RunningPeakCapacity_Wsa;
			double RunningPeakCapacity_Tsa;
			double RunningPeakCapacity_Point;
			// holds a local copy of the operating conditions of a mode
			vector<double> PolygonXs;
			vector<double> PolygonYs;
			double RequestedLoad_t_n1;
			double RequestedLoad_t_n2;
			double RequestedLoad_t_n3;
			double RequestedLoad_t_n4;
			double RequestedLoad_t_n5;
			double RequestedLoad_t_n6;
			double RequestedLoad_t_n7;
			double RequestedLoad_t_n8;
			int NumberOfModes;
		};
	}
}