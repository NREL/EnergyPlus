
#ifndef HybridEvapCoolingModel_hh_INCLUDED
#define HybridEvapCoolingModel_hh_INCLUDED
#include <iostream>  
//#include <fmiPlatformTypes.h>
//#include <HybridModelConfigFile.hh>
//#include <math.h>       /* floor */
#include <fstream>
using namespace std;
#include <string>
#include <list>
#include <map>
#include <vector>
//#include <math.h>       /* floor */

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

using namespace std;

const int MODE_BLOCK_OFFSET_Alpha = 9;
const int BLOCK_HEADER_OFFSET_Alpha =  18;

const int MODE1_BLOCK_OFFSET_Number = 2;
const int MODE_BLOCK_OFFSET_Number = 16;
const int BLOCK_HEADER_OFFSET_Number = 6;
#define MINIMUM_LOAD_TO_ACTIVATE 0.5 // (kw)_ should think about if this needs to be user defined!!!!!
#define IMPLAUSIBLE_POWER 10000000
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

	namespace HybridEvapCoolingModel {

		//using namespace HybridModelConfigFile;
		enum class SYSTEMOUTPUTS { VENTILATION_AIR_V, SUPPLY_MASS_FLOW, SUPPLY_AIR_HUMIDITY, SYSTEM_FUEL_USE, SUPPLY_AIR_TEMP, MIXED_AIR_TEMP, SUPPLY_AIR_RH, SUPPLY_AIR_HR, MIXED_AIR_HR};
		
		class CModeSolutionSpace
		{
		public:
			vector<double> PointX;
			vector<double> PointY;
			vector<double> PointMeta;
			void AddItem(double X, double Y, double M)
			{
				PointX.push_back(X);
				PointY.push_back(Y);
				PointMeta.push_back(M);
			}
		};	 

		class CMode
		{
			public:
			CMode() : ModeID(0.0), Max_Msa(0.0), Min_Msa(0.0),Min_OAF(0.0), Max_OAF(0.0), Minimum_Outside_Air_Temperature(0.0), Maximum_Outside_Air_Temperature(0.0)
			,Minimum_Outside_Air_Humidity_Ratio(0.0), Maximum_Outside_Air_Humidity_Ratio(0.0), NormalizationReference(0.0), Correction (0.0) {}
			
			//finish init above
			~CMode();                  // destructor
			int ModeID;
			CModeSolutionSpace sol;
			string ModeName;
			int Tsa_curve_pointer;
			int HRsa_curve_pointer;
			int Psa_curve_pointer;
			double Max_Msa;
			double Min_Msa;
			double Min_OAF;
			double Max_OAF;
			double Minimum_Outside_Air_Temperature;
			double Maximum_Outside_Air_Temperature;
			double Minimum_Outside_Air_Humidity_Ratio;
			double Maximum_Outside_Air_Humidity_Ratio;
			double Minimum_Outside_Air_Relative_Humidity;
			double Maximum_Outside_Air_Relative_Humidity;
			double Minimum_Return_Air_Temperature;
			double Maximum_Return_Air_Temperature;
			double Minimum_Return_Air_Humidity_Ratio;
			double Maximum_Return_Air_Humidity_Ratio;
			double Minimum_Return_Air_Relative_Humidity;
			double Maximum_Return_Air_Relative_Humidity;
			double NormalizationReference;
			double Correction;
			bool CMode::ValidPointer(int curve_pointer);
			bool CMode::ValidateArrays(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, std::string cCurrentModuleObject);
			bool CMode::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject);
			bool CMode::ParseMode0(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject);
		
			void CMode::InitializeCurve( int curveType, int CurveID);
			double CMode::CalculateCurveVal(double X_0, double X_1, double X_2, double X_3, double X_4, double X_5, double X_6, int mode_number, int curve_ID);
			bool CMode::InitializeOSAFConstraints(double minOSAF, double maxOSAF);
			bool CMode::InitializeMsaRatioConstraints(double minMsa, double maxMsa);
			bool CMode::InitializeOutsideAirTemperatureConstraints(double min, double max);
			bool CMode::InitializeOutsideAirHumidityRatioConstraints(double min, double max);
			bool CMode::InitializeOutsideAirRelativeHumidityConstraints(double min, double max);
			bool CMode::InitializeReturnAirTemperatureConstraints(double min, double max);
			bool CMode::InitializeReturnAirHumidityRatioConstraints(double min, double max);
			bool CMode::InitializeReturnAirRelativeHumidityConstraints(double min, double max); 
			bool CMode::GenerateSolutionSpace(double ResolutionMsa, double ResolutionOSA);
			bool CMode::MeetsOAEnvConstraints(double Tosa, double Wosa, double RHos);
		private:
		
		};
		
		class CSetting
		{
		public:
			CSetting() :Runtime_Fraction(0), Mode(0), Outside_Air_Fraction(0), Supply_Air_Mass_Flow_Rate(0), Supply_Air_Ventilation_Volume(0), Supply_Air_Mass_Flow_Rate_Ratio(0),
				SupplyAirTemperature(0), Mixed_Air_Temperature(0), SupplyAirW(0), Mixed_Air_W(0), TotalSystem(0), SensibleSystem(0), LatentSystem(0), TotalZone(0), SensibleZone(0), LatentZone(0), Dehumidification(0), ElectricalPower(IMPLAUSIBLE_POWER), pMode(NULL){}
			double Runtime_Fraction;
			double Mode;
			double Outside_Air_Fraction;
			double Supply_Air_Mass_Flow_Rate;
			double Supply_Air_Ventilation_Volume;
			double Supply_Air_Mass_Flow_Rate_Ratio;
			double SupplyAirTemperature;
			double Mixed_Air_Temperature;
			double SupplyAirW;
			double Mixed_Air_W;
			double TotalSystem;
			double SensibleSystem;
			double LatentSystem;
			double TotalZone;
			double SensibleZone;
			double LatentZone;
			double Dehumidification; 
			double ElectricalPower;
			// add other fuels, or change name to be fuel
			CMode* pMode;
			CSetting& operator=(CSetting other)
			{
				using std::swap;
				swap(Runtime_Fraction, other.Runtime_Fraction);
				swap(Mode, other.Mode);
				swap(Outside_Air_Fraction, other.Outside_Air_Fraction);
				swap(Supply_Air_Mass_Flow_Rate, other.Supply_Air_Mass_Flow_Rate);
				swap(Supply_Air_Ventilation_Volume, other.Supply_Air_Ventilation_Volume);
				swap(Supply_Air_Mass_Flow_Rate_Ratio, other.Supply_Air_Mass_Flow_Rate_Ratio);
				swap(SupplyAirTemperature, other.SupplyAirTemperature);
				swap(Mixed_Air_Temperature, other.Mixed_Air_Temperature);
				swap(SupplyAirW, other.SupplyAirW);
				swap(Mixed_Air_W, other.Mixed_Air_W);
				swap(TotalSystem, other.TotalSystem);
				swap(SensibleSystem, other.SensibleSystem);
				swap(LatentSystem, other.LatentSystem);
				swap(TotalZone, other.TotalZone);
				swap(SensibleZone, other.SensibleZone);
				swap(LatentZone, other.LatentZone);
				swap(Dehumidification, other.Dehumidification);
				swap(ElectricalPower, other.ElectricalPower);
				swap(pMode, other.pMode);
				
				// repeat for other member variables;
				return *this;
			}
		};

		class CStepInputs
		{
			public:
			CStepInputs() : Tosa(0), Tra(0), RHosa(0), RHra(0), RequestedLoad(0), ZoneHeatingLoad(0), ZoneMoistureLoad(0), ZoneDehumidificationLoad(0), MinimumOA(0) {}
			double Tosa; double Tra; double RHosa; double RHra; double RequestedLoad; double ZoneHeatingLoad; double ZoneMoistureLoad; double ZoneDehumidificationLoad; double MinimumOA ;
		};

		class Model                   // begin declaration of the class
		{
		public:                    // begin public section
			Model();
			~Model();                  // destructor
	
			// Default Constructor
			std::string Name; // user identifier
			bool Initialized;
			int ZoneNum;
			std::string Schedule; 
			std::string Tsa_Lookup_Name;
			std::string Mode1_Hsa_Lookup_Name;
			std::string Mode1_Power_Lookup_Name;
			Real64 SystemMaximumSupplyAirFlowRate;
			Real64 ScalingFactor;
			int SchedPtr; // Pointer to the correct schedule
			int UnitOn;
			Real64 UnitTotalCoolingRate; // unit output to zone, total cooling rate [W]
			Real64 UnitTotalCoolingEnergy; // unit output to zone, total cooling energy [J]
			Real64 UnitSensibleCoolingRate;
			Real64 UnitSensibleCoolingEnergy;
			Real64 UnitLatentCoolingRate;
			Real64 UnitLatentCoolingEnergy;
			Real64 SystemTotalCoolingRate; // unit output to zone, total cooling rate [W]
			Real64 SystemTotalCoolingEnergy; // unit output to zone, total cooling energy [J]
			Real64 SystemSensibleCoolingRate;
			Real64 SystemSensibleCoolingEnergy;
			Real64 SystemLatentCoolingRate;
			Real64 SystemLatentCoolingEnergy;
			Real64 UnitTotalHeatingRate; // unit output to zone, total Heating rate [W]
			Real64 UnitTotalHeatingEnergy; // unit output to zone, total Heating energy [J]
			Real64 UnitSensibleHeatingRate;
			Real64 UnitSensibleHeatingEnergy;
			Real64 UnitLatentHeatingRate;
			Real64 UnitLatentHeatingEnergy;
			Real64 SystemTotalHeatingRate; // unit output to zone, total Heating rate [W]
			Real64 SystemTotalHeatingEnergy; // unit output to zone, total Heating energy [J]
			Real64 SystemSensibleHeatingRate;
			Real64 SystemSensibleHeatingEnergy;
			Real64 SystemLatentHeatingRate;
			Real64 SystemLatentHeatingEnergy;
			Real64 RequestedLoadToCoolingSetpoint;
			int TsaMin_schedule_pointer;
			int TsaMax_schedule_pointer;
			int RHsaMin_schedule_pointer;
			int RHsaMax_schedule_pointer;
			int PrimaryMode;
			double PrimaryModeRuntimeFraction;
			double averageOSAF;
			int CurrentPrimaryMode();
			double CurrentPrimaryRuntimeFraction();
			
			int ErrorCode;
			int InletNode;
			int OutletNode;
			int SecondaryInletNode; // This is usually OA node feeding into the purge/secondary side
			int SecondaryOutletNode; // This outlet node of the secondary side and ilet to the secondary fan
			vector<int> Tsa_curve_pointer;
			vector<int>  HRsa_curve_pointer;
			vector<int>  Psa_curve_pointer;
			list<CMode*> OperatingModes; 
			list<CSetting*> CurrentOperatingSettings;
			
			CSetting *pOptimal;
			CSetting *pSubOptimal;
			list<CSetting*> Settings;
			Real64 FinalElectricalPower;
			Real64 FinalElectricalEnergy;
			Real64 InletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 InletTemp;
			Real64 InletWetBulbTemp;
			Real64 InletHumRat;
			Real64 InletEnthalpy;
			Real64 InletPressure;
			Real64 InletRH;
			Real64 OutletVolumetricFlowRate;
			Real64 OutletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 OutletTemp;
			Real64 OutletWetBulbTemp;
			Real64 OutletHumRat;
			Real64 OutletEnthalpy;
			Real64 OutletPressure;
			Real64 OutletRH;
			Real64 SecInletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 SecInletTemp;
			Real64 SecInletWetBulbTemp;
			Real64 SecInletHumRat;
			Real64 SecInletEnthalpy;
			Real64 SecInletPressure;
			Real64 SecInletRH;
			Real64 SecOutletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 SecOutletTemp;
			Real64 SecOutletWetBulbTemp;
			Real64 SecOutletHumRat;
			Real64 SecOutletEnthalpy;
			Real64 SecOutletPressure;
			Real64 SecOutletRH;
			Real64 ScaledSystemMaximumSupplyAirMassFlowRate;
			int OARequirementsPtr; // Index to DesignSpecification:OutdoorAir object
			bool OutdoorAir;
			double MinOA_Msa;

			int Model::GetID();            // accessor function
			void Model::SetID(int vID) { ID = vID; };    // accessor function
			void Model::doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double ZoneHeatingLoad, double ZoneMoistureLoad, double ZoneDehumidificationLoad, double DesignMinVR);
			void Model::Initialize(int ZoneNumber);//, ConfigFile* pConfig);
			CMode* Model::AddNewOperatingMode(double correction);
			void Model::InitializeModelParams();
			void Model::ModelLog(std::string fmuLocation);
			double Model::CalcHum_ratio_W(double Tdb, double RH, double P);
			bool Model::MeetsSupplyAirTOC(double Tosa);
			bool Model::MeetsSupplyAirRHOC(double Wosa);
			double Model::CalculateMixedAirTemp();
			double Model::CheckVal_T(double T);
			double Model::CheckVal_W(double W);
			bool Model::SetStandByMode(CMode* pMode0, CSetting *pStandBy, double Tosa, double Wosa, double Tra, double Wra );
			double Model::CalculateTimeStepAverage(SYSTEMOUTPUTS val);
			int Model::SetOperatingSetting(CStepInputs StepIns);
			CSetting *pStandBy;
			double Tsa;
			Real64 Wsa;
			Real64 SupplyVentilationAir;
			Real64 SupplyVentilationVolume; //SupplyVentilationAir/StdRhoAir
			 int ModeCounter;
			 bool CoolingRequested ;
			 bool HeatingRequested ;
		
		private:                   // begin private section
			int ID;              // member variable
			char * string;
			//numver of times in a day it failed to 
			vector<int> SAT_OC_MetinMode_v;
			vector<int> SAHR_OC_MetinMode_v;
			bool WarnOnceFlag;
			//holds the X and Y points of the possible sollutions within the operating conditions. Int is the mode number
			list<CModeSolutionSpace*> SolutionSpaces;
			double Round(double x);
			double Model::Sat_press(double Tdb);
			double Model::Part_press(double P, double W);
			double ResolutionMsa;
			double ResolutionOSA;

			int count_EnvironmentConditionsMetOnce;
			int count_SAHR_OC_MetOnce;
			int count_SAT_OC_MetOnce;
			int count_DidWeMeetLoad;
			
			double MsaRated;
			double RatedH;
		
			double cp;
			double Lambna;
			bool optimal_EnvCondMet;
			bool RunningPeakCapacity_EnvCondMet;
		
			double Minimum_Supply_Air_Temp;
			double RunningPeakCapacity_Wsa;
			double RunningPeakCapacity_Tsa;
			double RunningPeakCapacity_Point;
			vector<double> PolygonXs;
			vector<double> PolygonYs;

			int NumberOfModes;
		};

	
	}
}

#endif