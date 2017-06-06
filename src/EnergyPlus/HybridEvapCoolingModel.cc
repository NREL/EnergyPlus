
#include <HybridEvapCoolingModel.hh>

#include <UtilityRoutines.hh>

#include <string>
#include <list>
#include <math.h>
#include <windows.h>
#include <ScheduleManager.hh>
#include <General.hh>
#include <CurveManager.hh>
#include <DataGlobals.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <Psychrometrics.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
using namespace std;

namespace EnergyPlus {//***************

	namespace HybridEvapCoolingModel {
		using namespace std;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::SecInHour;
		using namespace Psychrometrics;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::CurveValue;

		typedef int* MyType;
		map<string, string> testop;
		#define  DEF_Tdb 0
		#define  DEF_RH 1

#define  TEMP_CURVE 0 
#define  W_CURVE 1
#define  POWER_CURVE 2
				// constructor of Model,

		#include <fstream>
		fstream logfile;
		//ZoneHybridUnitaryACSystem

		bool CMode::InitializeOutsideAirTemperatureConstraints(double min, double max)
		{
			//note If this field is blank, there will be no lower constraint on outside air temperature.
			//if()
			Minimum_Outside_Air_Temperature=min;
			Maximum_Outside_Air_Temperature=max;
			return true;
		}
		bool CMode::InitializeOutsideAirHumidityRatioConstraints(double min, double max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Outside_Air_Humidity_Ratio = min;
			Maximum_Outside_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeOutsideAirRelativeHumidityConstraints(double min, double max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)

			Minimum_Outside_Air_Relative_Humidity = min;
			Maximum_Outside_Air_Relative_Humidity = max;
			return true;
		}
		bool CMode::InitializeReturnAirTemperatureConstraints(double min, double max)
		{
			//will not be considered when the return air temperature is below the value in this field.
			//If this field is blank, there will be no lower constraint on return air temperature
			Minimum_Return_Air_Temperature = min;
			Maximum_Return_Air_Temperature = max;
			return true;
		}
		bool CMode::InitializeReturnAirHumidityRatioConstraints(double min, double max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Return_Air_Humidity_Ratio = min;
			Maximum_Return_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeReturnAirRelativeHumidityConstraints(double min, double max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)
			Minimum_Return_Air_Relative_Humidity = min;
			Maximum_Return_Air_Relative_Humidity = max;
			return true;
		}


		bool CMode::InitializeOSAFConstraints(double minOSAF, double maxOSAF)
		{
			//minimum 0.00, maximum 1.00, Outside air fractions below this value will not be considered.
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_OAF=minOSAF;
			Max_OAF=maxOSAF;
			return true;
		}
		bool CMode::InitializeMsaRatioConstraints(double minMsa, double maxMsa)
		{
			//minimum 0.00, maximum 1.00, Supply air mass flow rate ratios below this value will not be considered.
			//Supply air mass flow rate ratio describes supply air mass flow rate as a fraction of mass flow rate associated with the value in field : "System Maximum Supply Air Flow Rate".
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_Msa=minMsa;
			Max_Msa=maxMsa;
			return true;
		}
		bool CMode::ValidPointer(int curve_pointer) {
			if (curve_pointer >= 0) return true;
			else  return false;
		}
		double CMode::CalculateCurveVal(double X_0, double X_1, double X_2, double X_3, double X_4, double X_5, double X_6, int mode_number, int curveType)
		{
			double Y_val = 0;

			switch (curveType)
			{
			case TEMP_CURVE:
				if( ValidPointer(Tsa_curve_pointer))
				{
					Y_val = CurveValue(Tsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else { Y_val = X_4;//return air temp
				}
				
				break;
			case W_CURVE:
				
				if (ValidPointer(HRsa_curve_pointer))
				{
					Y_val = CurveValue(HRsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);//0.01;//
				}
				else {
					Y_val = X_5;//return HR
				}
				break;
			case POWER_CURVE:
				
				if (ValidPointer(Psa_curve_pointer))
				{
					Y_val = CurveValue(Psa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;//or set a more reasonable default
				}
				break;
			default:
				break;
			}

			return Y_val;
		}
		void CMode::InitializeCurve(int curveType, int curve_ID)
		{
			switch (curveType)
			{
			case TEMP_CURVE:
				Tsa_curve_pointer=curve_ID;
				break;
			case W_CURVE:
				HRsa_curve_pointer=curve_ID;
				break;
			case POWER_CURVE:
				Psa_curve_pointer=curve_ID;
				break;
			default:
				break;
			}
		}

		bool CMode::GenerateSolutionSpace(double ResolutionMsa,double ResolutionOSA)
		{
			double deltaMsa = Max_Msa -Min_Msa;
			double deltaOAF = Max_OAF - Min_OAF  ;
			double Msastep_size = (deltaMsa * ResolutionMsa);
			double OAFsteps_size = (deltaOAF * ResolutionOSA);


			for (double Msa_val = Min_Msa; Msa_val <= Max_Msa; Msa_val = Msa_val + Msastep_size)
			{
				for (double OAF_val = Min_OAF; OAF_val <= Max_OAF; OAF_val = OAF_val + OAFsteps_size)
				{
					//sol.PointX.push_back( i);
					//sol.PointY.push_back(j);
					sol.AddItem(Msa_val, OAF_val, 0);
				}

			}
			return true;
		}
		bool CMode::ValidateArrays(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, std::string cCurrentModuleObject)
		{
			int alphas_len = Alphas.size();
			int numbers_len = Numbers.size();
			int OpperatingModes = Numbers(4);
			int parmsnumber = alphas_len + numbers_len;
			int MinimumExpectedLength = OpperatingModes*(MODE_BLOCK_OFFSET_Number + MODE_BLOCK_OFFSET_Alpha) + BLOCK_HEADER_OFFSET_Alpha + BLOCK_HEADER_OFFSET_Number;
			if (MinimumExpectedLength > (parmsnumber)) return false;
			if (OpperatingModes< ModeID) return false;
			return true;
		}


		bool CMode::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields,  Array1D<bool>  lAlphaBlanks,std::string cCurrentModuleObject)
		{
			
			if(!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				ShowSevereError("Mode description array does not match mode number, in"  + cCurrentModuleObject);
				return false;
			}
				 
			bool ErrorsFound=false;
			int inter_Alpha = BLOCK_HEADER_OFFSET_Alpha + MODE_BLOCK_OFFSET_Alpha * ModeID;
			int inter_Number = BLOCK_HEADER_OFFSET_Number + MODE_BLOCK_OFFSET_Number* ModeID;

			int curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject );
					ErrorsFound = true;
					InitializeCurve(TEMP_CURVE, -1);
				}
				else { InitializeCurve(TEMP_CURVE, curveID); }

			}

			inter_Alpha = inter_Alpha + 1;
			//ZoneHybridUnitaryAirConditioner(UnitLoop).Tsa_curve_pointer.push_back(GetCurveIndex(Alphas(19)));

			//A20, \field Mode0 Supply Air Humidity Ratio Lookup Table Name
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(W_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject );
					ErrorsFound = true;
					InitializeCurve(W_CURVE, -1);
				}
				else { InitializeCurve(W_CURVE, curveID); }

			}
			inter_Alpha = inter_Alpha + 1;
			//A21, \field Mode0 System Electric Power Lookup Table Name
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(POWER_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject );
					ErrorsFound = true;
					InitializeCurve(POWER_CURVE, -1);
				}
				else { InitializeCurve(POWER_CURVE, curveID); }

			}
			//A22, \field Mode0 Supply Fan Electric Power Lookup Table Name
			//A23, \field Mode0 External Static Pressure Lookup Table Name
			//A24, \field Mode0 System Second Fuel Consumption Lookup Table Name
			//A25, \field Mode0 System Third Fuel Consumption Lookup Table Name
			//A26, \field Mode0 System Water Use Lookup Table Name
			//N6, \field Mode0  Minimum Outside Air Temperature
			//N7, \field Mode0  Maximum Outside Air Temperature
			bool ok = InitializeOutsideAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number+1)); //Numbers(6), Numbers(7));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N8, \field Mode0  Minimum Outside Air Humidity Ratio
			//N9, \field Mode0  Maximum Outside Air Humidity Ratio
			ok = InitializeOutsideAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(8), Numbers(9));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N10, \field Mode0 Minimum Outside Air Relative Humidity
			//N11, \field Mode0 Maximum Outside Air Relative Humidity
			ok = InitializeOutsideAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(10), Numbers(11));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N12, \field Mode0 Minimum Return Air Temperature
			//N13, \field Mode0 Maximum Return Air Temperature
			ok = InitializeReturnAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(12), Numbers(13));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N14, \field Mode0 Minimum Return Air Humidity Ratio 
			//N15, \field Mode0 Maximum Return Air Humidity Ratio
			ok = InitializeReturnAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(14), Numbers(15))
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N16, \field Mode0 Minimum Return Air Relative HumidityInitialize
			//N17, \field Mode0 Maximum Return Air Relative Humidity
			ok = InitializeReturnAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(16), Numbers(17));
			if (!ok) {
				ShowSevereError("Invalid " + cAlphaFields(inter_Number) + '=' + Alphas(inter_Number) + "Or Invalid" + cAlphaFields(inter_Number + 1) + '=' + Alphas(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N18, \field Mode0 Minimum Outside Air Fraction
			//N19, \field Mode0 Maximum Outside Air Fraction

			ok = InitializeOSAFConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(18), Numbers(19));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number+1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}
			//N20, \field Mode0 Minimum Supply Air Mass Flow Rate Ratio
			//N21, \field Mode0 Maximum Supply Air Mass Flow Rate Ratio
			inter_Number = inter_Number + 2;
			ok = InitializeMsaRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(20), Numbers(21));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject );
				ErrorsFound = true;
			}

			return ErrorsFound;
		}
		// Alpha items for object
		 // Numeric items for object
		  // Alpha field names
		// Numeric field names

		bool CMode::MeetsOAEnvConstraints(double Tosa, double Wosa, double RHosa)
		{
			bool OATempConstraintmet = false;
			bool OARHConstraintmet = false;
		  //	double RHosa = Part_press(101.325, Wosa) / Sat_press(Tosa);

			if (Tosa >= Minimum_Outside_Air_Temperature && Tosa <= Maximum_Outside_Air_Temperature)
			{
				OATempConstraintmet = true;
			}

			if (Wosa >= Minimum_Outside_Air_Humidity_Ratio && Wosa <= Maximum_Outside_Air_Humidity_Ratio)
			{
				OARHConstraintmet = 1;
			}

			if (RHosa >=  Minimum_Outside_Air_Relative_Humidity  && RHosa <= Maximum_Outside_Air_Relative_Humidity)
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		bool Model::MeetsSupplyAirTOC(double Tosa)
		{
			using ScheduleManager::GetCurrentScheduleValue;
			double MinSAT = 10;
			double MaxSAT = 20;
			if (TsaMin_schedule_pointer > 0) {
				MinSAT = GetCurrentScheduleValue(TsaMin_schedule_pointer);
			}
			if (TsaMax_schedule_pointer > 0) {
				MaxSAT = GetCurrentScheduleValue(TsaMax_schedule_pointer);
			}
			if (Tosa < MinSAT || Tosa > MaxSAT)
				return false;
			return true;
		}

		bool Model::MeetsSupplyAirRHOC(double Wosa)
		{
			// implement
			if (Wosa < 0)
				//temp fix
				Wosa = 0.003;
			//return false;
			return true;
		}

		Model::Model() :
				//Tsa_curve_pointer(0), HRsa_curve_pointer(0),Psa_curve_pointer(0),
				TsaMin_schedule_pointer(0),
				TsaMax_schedule_pointer(0),
				RHsaMin_schedule_pointer(0),
				RHsaMax_schedule_pointer(0),
				ZoneNodeNum(0),
				MsaCapacityRatedCond(0),
				SchedPtr(0),
				UnitTotalCoolingRate(0.0),
				UnitTotalCoolingEnergy(0.0),
				UnitSensibleCoolingRate(0.0),
				UnitSensibleCoolingEnergy(0.0),
				RequestedLoadToCoolingSetpoint(0.0),
				Mode(0),
				ErrorCode(0),
				InletNode(0),
				OutletNode(0),
				SecondaryInletNode(0),
				SecondaryOutletNode(0),
				InletMassFlowRate(0.0),
				InletTemp(0.0),
				InletWetBulbTemp(0.0),
				InletHumRat(0.0),
				InletEnthalpy(0.0),
				InletPressure(0.0),
				InletRH(0.0),
				OutletMassFlowRate(0.0),
				OutletTemp(0.0),
				OutletWetBulbTemp(0.0),
				OutletHumRat(0.0),
				OutletEnthalpy(0.0),
				OutletPressure(0.0),
				OutletRH(0.0),
				SecInletMassFlowRate(0.0),
				SecInletTemp(0.0),
				SecInletWetBulbTemp(0.0),
				SecInletHumRat(0.0),
				SecInletEnthalpy(0.0),
				SecInletPressure(0.0),
				SecInletRH(0.0),
				SecOutletMassFlowRate(0.0),
				SecOutletTemp(0.0),
				SecOutletWetBulbTemp(0.0),
				SecOutletHumRat(0.0),
				SecOutletEnthalpy(0.0),
				SecOutletPressure(0.0),
				SecOutletRH(0.0), 
				Wsa(0.0), ElectricalPower(0.0), SupplyVentilationAir(0.0),
			Initialized(false)
		{
			//InitializeModelParams();
			Minimum_Supply_Air_Temp = 8; //must set this propoerly 
			cp = 1; //kJ/degreesC.kg ePlus should cary this as an avialble variable
			Lambna = 2260; //(kJ/kg) latent heat of vaporization ePlus should carry this an available variable
			count_EnvironmentConditionsMetOnce = 0;
			count_SAHR_OC_MetOnce = 0;
			count_SAT_OC_MetOnce = 0;
			count_DidWeMeetLoad = 0;
			InitializeModelParams();

			ModeCounter = 0;
		}
		//*************************************************
	

		void Model::InitializeModelParams()
		{
			optimal_EnvCondMet = false;
			optimal_power = 10e+10;
			optimal_Msa = 0;
			optimal_OSAF = 0;
			optimal_H_sensible_room = 0;
			optimal_TotalSystemH = 0;
			optimal_SHR = 0;
			optimal_Mvent = 0;
			optimal_Mode = 0;
			optimal_Point = 0; //this is used to identify the point that
			optimal_Wsa = 0;
			optimal_Tsa = 0;
			Tsa = 0;
			//Wsa = 0;
			DidWeMeetLoad = 0;
			RunningPeakCapacity_EnvCondMet = false;
			RunningPeakCapacity_power = 10e+10;
			RunningPeakCapacity_Msa = 0;
			RunningPeakCapacity_OSAF = 0;
			RunningPeakCapacity_H_sensible_room = 0;
			RunningPeakCapacity_TotalSystemH = 0;
			RunningPeakCapacity_SHR = 0;
			RunningPeakCapacity_Mvent = 0;
			RunningPeakCapacity_Mode = 0;
			RunningPeakCapacity_Wsa = 0;
			RunningPeakCapacity_Tsa = 0;
			RunningPeakCapacity_Point = 0;
			RequestedLoad_t_n1 = 0;
			RequestedLoad_t_n2 = 0;
			RequestedLoad_t_n3 = 0;
			RequestedLoad_t_n4 = 0;
			RequestedLoad_t_n5 = 0;
			RequestedLoad_t_n6 = 0;
			RequestedLoad_t_n7 = 0;
			RequestedLoad_t_n8 = 0;
		}
			
		Model::~Model()                 // destructor, just an example
		{
			list<CModeSolutionSpace*>::iterator iter;
			for (iter = SolutionSpaces.begin(); iter != SolutionSpaces.end(); ++iter) {
				CModeSolutionSpace* p = (*iter);
				delete p;
			}
		}
		CMode::~CMode()                 // destructor, just an example
		{
			/*list<CModeSolutionSpace*>::iterator iter;
			for (iter = sol.begin(); iter != sol.end(); ++iter) {
				CModeSolutionSpace* p = (*iter);
				delete p;
			}*/
		}

		// GetAge, Public accessor function
		// returns value of itsAge member
		int Model::GetID()
		{
			return ID;
		}


		double Model::EstimateQRemaining(double TroomTemp, Real64 communicationStepSize) {
			double Q;
			double volume = 800; //m3
			double mass = volume*1.2;
			double TheatingSetpoint = 18;
			double TcoolingSetpoint = 24;

			Q = 0;
			if (TroomTemp > TcoolingSetpoint) {
				Q = -((TroomTemp - TcoolingSetpoint)*mass * 1006) / (2 * communicationStepSize);
			}

			if (TroomTemp < TheatingSetpoint) {
				Q = ((TheatingSetpoint - TroomTemp)*mass * 1006) / (2 * communicationStepSize);
			}


			return -Q;
		}

		CMode* Model::AddNewOperatingMode()
		{
			CMode* pMode = new CMode;
			pMode->ModeID = ModeCounter;
			ModeCounter++;
			OperatingModes.push_back(pMode);
	
			return pMode;
		}
		void Model::Initialize(std::string fmuLocation)//, ConfigFile* pConfig)
		{
			if (Initialized) return;
			std::string dir;
			std::string file;
			std::string strfmuLocation(fmuLocation);
			
			//	Config->TrimFilename(strfmuLocation,dir);
			//	cout<<"debug 1"<<strfmuLocation;
			//Config = new ConfigFile;
	//		Config->ParseConfigFile(strfmuLocation);
			Initialized = true;
			//Iterate through modes of operation
			ResolutionMsa = 0.2; //msa/msaRATED
			ResolutionOSA = 0.2; //OSAF as absolute fraction (not %)
								// get system values
			for each (CMode* Mode in OperatingModes)
			{
				Mode->GenerateSolutionSpace(ResolutionMsa, ResolutionOSA);
			}


			Initialized = true;
		}


		void Model::ModelLog(std::string fmi_logmessage)
		{
			std::string logmessage(fmi_logmessage);
			cout << logmessage;
		}

		double Model::Round(double x)
		{
			return floor(x + 0.5);
		}



		

		double Model::Part_press(double P, double W)
		{
			// Function to compute partial vapor pressure in [kPa]
			// From page 6.9 equation 38 in ASHRAE Fundamentals handbook (2005)
			//   P = ambient pressure [kPa]
			//   W = humidity ratio [kg/kg dry air]

			return (P * W / (0.62198 + W));
		}

		double Model::Sat_press(double Tdb)
		{
			// Function to compute saturation vapor pressure in [kPa]
			//ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
			//   Tdb = Dry bulb temperature [degC]
			// Valid from -100C to 200 C

			double  C1 = -5674.5359;
			double  C2 = 6.3925247;
			double  C3 = -0.009677843;
			double  C4 = 0.00000062215701;
			double  C5 = 2.0747825E-09;
			double  C6 = -9.484024E-13;
			double  C7 = 4.1635019;
			double  C8 = -5800.2206;
			double  C9 = 1.3914993;
			double  C10 = -0.048640239;
			double  C11 = 0.000041764768;
			double  C12 = -0.000000014452093;
			double  C13 = 6.5459673;
			double  Sat_press_val = 0;

			double   TK = Tdb + 273.15;         //Converts from degC to degK

			if (TK <= 273.15)
			{
				Sat_press_val = exp(C1 / TK + C2 + C3 * TK + C4 * pow(TK, 2) + C5 * pow(TK, 3) + C6 * pow(TK, 4) + C7 * log(TK)) / 1000;
			}
			else
			{
				Sat_press_val = exp(C8 / TK + C9 + C10 * TK + C11 * pow(TK, 2) + C12 * pow(TK, 3) + C13 * log(TK)) / 1000;
			}
			return Sat_press_val;

		}


		double Model::CalcHum_ratio_W(double Tdb, double RH, double P)
		{
			// Function to calculate humidity ratio [kg H2O/kg air]
			// Given dry bulb and wet bulb temperature inputs [degC]
			// ASHRAE Fundamentals handbood (2005)
			//   Tdb = Dry bulb temperature [degC]
			//   RH = Relative Humidity [Fraction or %]
			//   P = Ambient Pressure [kPa]

			double Pws = Sat_press(Tdb);
			double Hum_rat = 0.62198 * RH * Pws / (P - RH * Pws);   // Equation 22, 24, p6.8
			return Hum_rat;
		}

		double Model::CheckVal_W(double W)
		{
			if ((W > 1) || (W < 0))
			{
				//issue
				W = W;
			}
			return W;
		}
		double Model::CheckVal_T(double T)
		{
			if ((T > 50) || (T < 10))
			{
				//issue
				T = T;
			}
			return T;
		}


		double Model::CalculateMixedAirTemp()
		{
			double Tosa_ref = 37.78; //these can not be hard coded and need to be from config file?
			double MsaRatio_ref = 0.45;
			double TdbRA_ref = 25.56;
			double MixedAirDBTempAtRefCon = TdbRA_ref + MsaRatio_ref*(Tosa_ref - TdbRA_ref);
			return MixedAirDBTempAtRefCon;

		}

		void Model::RunTestModel(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double CapacityRatedCond, int CapacityFlag, double DesignMinVR)
		{
			//double tempHumidity = RHra;
			OutletTemp = 12;
			MinOA_Msa= DesignMinVR;
			double max_Msa = MsaRated;
			double EIR = 8;
			Mode = 1;
			if (RequestedLoad < 0 )
			{
				if (Tra != OutletTemp)
				{
					optimal_Msa = -RequestedLoad / (1006 * (Tra - OutletTemp));
					if (optimal_Msa > max_Msa)
					{
						optimal_Msa = max_Msa;
					}
				}
				else
				{
					optimal_Msa = max_Msa; // not sure anbout that.
				}

				if (MinOA_Msa > optimal_Msa)
				{
					optimal_Msa = MinOA_Msa;
				}
			}

			SupplyVentilationAir = optimal_Msa;

			
			OutletRH = CheckVal_W(RHra);
			OutletHumRat = PsyWFnTdbRhPb(OutletTemp, OutletRH, InletPressure);
			OutletEnthalpy = PsyHFnTdbRhPb(OutletTemp, OutletRH, InletPressure); // is the outlet presure going to be different? //InletEnthalpy - (ZoneCoolingLoad / AirMassFlow);
			OutletMassFlowRate = optimal_Msa;
			
			double QTotUnitOut = 0;
			double QSensUnitOut = 0;
			if (OutletEnthalpy < InletEnthalpy)
			{
				QTotUnitOut = OutletMassFlowRate * (OutletEnthalpy - InletEnthalpy);
				QSensUnitOut = OutletMassFlowRate * (PsyHFnTdbW(OutletTemp, OutletHumRat) - PsyHFnTdbW(InletTemp, OutletHumRat));
			}
			else
			{
				QTotUnitOut = 0;
				QSensUnitOut = 0;
			}

			UnitTotalCoolingRate = std::abs(min(0.0, QTotUnitOut));
			UnitTotalCoolingEnergy = UnitTotalCoolingRate * TimeStepSys * SecInHour;
			UnitSensibleCoolingRate = std::abs(min(0.0, optimal_H_sensible_room * 1000));
			UnitSensibleCoolingEnergy = UnitSensibleCoolingRate * TimeStepSys * SecInHour;

			ElectricalPower = UnitTotalCoolingRate/ EIR;
		
		}
		//void Model::doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double CapacityRatedCond, int CapacityFlag, double DesignMinVR, double rTestFlag, double *returnQSensible, double *returnQLatent, double *returnSupplyAirMassFlow, double *returnSupplyAirTemp, double *returnSupplyAirRelHum, double *returnVentilationAir, int *FMUmode, double *ElectricalPowerUse, double communicationStepSize, int *bpErrorCode) {

		void Model::doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double CapacityRatedCond, int CapacityFlag, double DesignMinVR, double rTestFlag, double communicationStepSize) {
			
			using General::RoundSigDigits; 
			int modenumber = 0;
			int point_number = 0;
			double MsaRatio = 0;
			double OSAF = 0;
			double Msa = 0;
			double Mvent = 0;
			double EIR, ElectricalPower, SHR, Tma, Wma, Hsa, Hma, Y_DeltaH, H_SENS_ROOM, mode_optimal_power, mode_optimal_point, RequestedCoolingLoad;
			mode_optimal_power = 0;
			double PreviousMaxiumOutput = 0;
			double Wosa = CalcHum_ratio_W(Tosa, RHosa / 100, 101.325);
			double Wra = CalcHum_ratio_W(Tra, RHra / 100, 101.325);
			bool EnvironmentConditionsMet, EnvironmentConditionsMetOnce, MinVRMet, MinVRMetOnce, SAT_OC_Met, SAT_OC_MetOnce, SARH_OC_Met, SAHR_OC_MetOnce;
			EnvironmentConditionsMetOnce = SAT_OC_Met = SAT_OC_MetOnce = SARH_OC_Met = SAHR_OC_MetOnce = false;
			double SupplyAirDBTempAtRefCon = 10;// CalculateSupplyAirDBTempAtRefCon();
			double MixedAirDBTempAtRefCon = 25;// CalculateMixedAirTemp();
			MinOA_Msa = DesignMinVR;

			if (CapacityFlag == 1)
			{
				MsaRated = CapacityRatedCond;//MsaRated
			}
			else
			{
				MsaRated = CapacityRatedCond / 1000 * (MixedAirDBTempAtRefCon - SupplyAirDBTempAtRefCon);
			}

			double averaged_requestedLoad = 0;

		//	*returnVentilationAir = 0;
		//	*returnSupplyAirMassFlow = 0;
		//	*bpErrorCode = 0;

			if (rTestFlag == 1)
			{  
				//RunTestModel();
				return;
			}

			if (RequestedLoad < 0)
			{
				RequestedCoolingLoad = -RequestedLoad / 1000; //convert to kw
				std::list<CMode*>::const_iterator iterator;
				for (iterator = OperatingModes.begin(); iterator != OperatingModes.end(); ++iterator) // iterate though the modes.
				{
					CMode* pMode = *iterator;
					CModeSolutionSpace* solutionspace = &(pMode->sol);
					int solution_map_sizeX = solutionspace->PointY.size() - 1;
					int solution_map_sizeY = solutionspace->PointX.size() - 1;
					int solution_map_sizeM = solutionspace->PointMeta.size() - 1;
					if (solution_map_sizeX != solution_map_sizeY)
					{
						ShowWarningError("Error in CModeSolutionSpace for mose, called in HybridEvapCooling:dostep");
						//cout << "Error in CModeSolutionSpace for mose ";
						return;
					}

					// Check that in this mode the 
					//Outside Air Relative Humidity(0 - 100 % )
					//Outside Air Humidity Ratio(g / g)
					//Outside Air Temperature(°C)
					if (pMode->MeetsOAEnvConstraints(Tosa, Wosa, RHosa) == true)//fix this it should not end if it does not meet contraints.
					{
						EnvironmentConditionsMet = EnvironmentConditionsMetOnce = true;
					}
					else
					{
						EnvironmentConditionsMet = false;
					}

					if (EnvironmentConditionsMet == true)
					{

						for (point_number = 0;point_number != solution_map_sizeX;point_number++) // within each mode go though all the combinations of solution spaces.
						{
							//Supply Air Mass Flow Rate(kg / s)
							//Outside Air Fraction(0 - 1)
							MsaRatio = solutionspace->PointX[point_number];// fractions of rated mass flow rate, so for some modes this might be low but others hi
							OSAF = solutionspace->PointY[point_number];
							// there is no operating condition test, becuase it uses those to make the map so only allowed values are here.
							Msa = MsaRated * MsaRatio;
							//Calculate the ventilation mass flow rate
							Mvent = Msa * OSAF;
							if (Mvent > MinOA_Msa) MinVRMet = MinVRMetOnce = true;
							else MinVRMet = false;

							if (MinVRMet)
							{
								//all these points meet the minimum VR requirement
								solutionspace->PointMeta[point_number] = 1;
								//'Set B_coefficients for DeltaH from lookup table for the specific mode
								Tsa = pMode->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, TEMP_CURVE); //TEMP_CURVE W_CURVE POWER_CURVE
																														  //Set B_coefficients for SHR from lookup table for the specific mode
																														  //Return Air Temperature(°C)
								if (MeetsSupplyAirTOC(Tsa)) SAT_OC_Met = SAT_OC_MetOnce = true;
								else
								{
									SAT_OC_Met = false;
								}

								Wsa = pMode->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, W_CURVE);
								//Return Air Relative Humidity(0 - 100 % )
								//Return Air Humidity Ratio(g / g)
								if (MeetsSupplyAirRHOC(Wsa)) SARH_OC_Met = SAHR_OC_MetOnce = true;
								else
								{
									//Wsa = 0;
									ShowWarningError("MeetsSupplyAirRHOC failed given a Wsa of"+ RoundSigDigits(Wsa,5));
									
									SARH_OC_Met = false;
								}

								if (SARH_OC_Met == true || SAT_OC_Met == true)
								{
									//Calculate the delta H 
									Tma = Tra + OSAF * (Tosa - Tra);
									Wma = Wra + OSAF * (Wosa - Wra);
									Hma = 1.006 * Tma * (2501 + 1.86 * Tma);
									Hsa = 1.006 * Tsa * (2501 + 1.86 * Tsa);
									Y_DeltaH = (Hma - Hsa) * Msa;     //kW
																	  //Calculate possible sensible load
									H_SENS_ROOM = cp *Msa* (Tra - Tsa);
									// even though we don't even know yet if this point can

									// does it meet minimum load requirement 
									if (H_SENS_ROOM > RequestedCoolingLoad)
									{

										//all these points meet the sensible heating load
										solutionspace->PointMeta[point_number] = 2;
										double Y_val = Msa*(pMode->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, POWER_CURVE));
										ElectricalPower = Y_val / 1000;  //kW
																		 //calculate the electrical power usage
																		 //ElectricalPower = EIR * Y_DeltaH * H_Rated 'kw?
																		 //Calculate EIR and SHR
										EIR = ElectricalPower / Y_DeltaH;
										SHR = cp * (Tma - Tsa) / (Hma - Hsa);
										//NCD Additions------------------------------------
										if (ElectricalPower < mode_optimal_power)
										{
											mode_optimal_power = ElectricalPower;
											mode_optimal_point = point_number;
										}
										//-------------------------------------------------
										//store a copy of the acceptable (meets load points for debug reaons can remove from final version
										if (ElectricalPower < optimal_power)
										{
											optimal_EnvCondMet = EnvironmentConditionsMet;
											optimal_Msa = Msa;
											optimal_OSAF = OSAF;
											optimal_power = ElectricalPower;
											optimal_H_sensible_room = H_SENS_ROOM;
											optimal_TotalSystemH = Y_DeltaH; //* H_Rated
											optimal_SHR = SHR;
											optimal_Mvent = Mvent;
											optimal_Mode = modenumber;
											optimal_Wsa = Wsa;
											optimal_Tsa = Tsa;
											optimal_Point = point_number; //this is used to identify the point that
											DidWeMeetLoad = true;
										}
									}
									else
									{
										//the system might never actually be able to meet the load so we need a method of remembering the best attempt at satisfying load
										if (H_SENS_ROOM > PreviousMaxiumOutput)
										{
											PreviousMaxiumOutput = H_SENS_ROOM;
											double Y_val = pMode->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, POWER_CURVE);
											ElectricalPower = Y_val / 1000;  //kW
																			 //Calculate EIR and SHR
											EIR = ElectricalPower / Y_DeltaH;
											SHR = cp * (Tma - Tsa) / (Hma - Hsa);
											//-------------------------------------------------
											//store a copy of the acceptable (meets load points for debug reaons can remove from final version
											//  If ElectricalPower < RunningPeakCapacity_power Then
											RunningPeakCapacity_EnvCondMet = EnvironmentConditionsMet;
											RunningPeakCapacity_power = ElectricalPower;
											RunningPeakCapacity_Msa = Msa;
											RunningPeakCapacity_OSAF = OSAF;
											RunningPeakCapacity_H_sensible_room = H_SENS_ROOM;
											RunningPeakCapacity_TotalSystemH = Y_DeltaH; //* H_Rated
											RunningPeakCapacity_SHR = SHR;
											RunningPeakCapacity_Mvent = Mvent;
											RunningPeakCapacity_Mode = modenumber;
											RunningPeakCapacity_Tsa = Tsa;
											RunningPeakCapacity_Wsa = Wsa;
											RunningPeakCapacity_Point = point_number; //this is used to identify the point that
										}
									}
								}

							}



						}
					}

					modenumber++;
				}
				if (EnvironmentConditionsMetOnce == false)
				{
					//*bpErrorCode = 1;
					ErrorCode = 1;
					count_EnvironmentConditionsMetOnce++;

					//error 
				}
				if (SAHR_OC_MetOnce == false)
				{
					count_SAHR_OC_MetOnce++;
					ErrorCode = 2;
					//error 
				}
				if (SAT_OC_MetOnce == false)
				{
					count_SAT_OC_MetOnce++;
					ErrorCode = 3;
					//error 
				}
				if (DidWeMeetLoad == false)
				{
					//*bpErrorCode = 3;
					count_DidWeMeetLoad++;
					//what is this ??
					if (RunningPeakCapacity_power > 100000)
					{
						ShowWarningError("Model was not able to provide cooling for a time step, called in HybridEvapCooling:dostep");
						//cout << "Model was not able to provide cooling.";
						RunningPeakCapacity_power = 0;
					}
					optimal_EnvCondMet = RunningPeakCapacity_EnvCondMet;
					optimal_Msa = RunningPeakCapacity_Msa;
					optimal_OSAF = RunningPeakCapacity_OSAF;
					optimal_power = RunningPeakCapacity_power;
					optimal_H_sensible_room = RunningPeakCapacity_H_sensible_room;
					optimal_TotalSystemH = RunningPeakCapacity_TotalSystemH;
					optimal_SHR = RunningPeakCapacity_SHR;
					optimal_Mvent = RunningPeakCapacity_Mvent;
					optimal_Mode = RunningPeakCapacity_Mode;
					optimal_Wsa = RunningPeakCapacity_Wsa;
					optimal_Tsa = RunningPeakCapacity_Tsa;
				}
				if (optimal_EnvCondMet == false)
				{
					ShowWarningError("Environmental conditions exceeded model limits, called in HybridEvapCooling:dostep");
					//cout << "Environmental conditions exceeded model limits./n";
				}
				if (ErrorCode == 0)
				{

			/*		*returnQSensible = -optimal_H_sensible_room * 1000;//RequestedLoad/(2);//*communicationStepSize);
					*returnQLatent = 0;
					*returnSupplyAirMassFlow = optimal_Msa;
					*returnSupplyAirTemp = CheckVal_T(optimal_Tsa);
					*returnSupplyAirRelHum = CheckVal_W(optimal_Wsa);
					*returnVentilationAir = optimal_Mvent;
					*FMUmode = optimal_Mode;
					*ElectricalPowerUse = optimal_power;*/
					SupplyVentilationAir = optimal_Mvent;
					OutletTemp = CheckVal_T(optimal_Tsa);//PsyTdbFnHW(ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy, MinHumRat);
					OutletRH = CheckVal_W(optimal_Wsa);
					OutletHumRat = PsyWFnTdbRhPb(OutletTemp, OutletRH, InletPressure);
					OutletEnthalpy = PsyHFnTdbRhPb(OutletTemp, OutletRH, InletPressure); // is the outlet presure going to be different? //InletEnthalpy - (ZoneCoolingLoad / AirMassFlow);
					OutletMassFlowRate = optimal_Msa;
					Mode = optimal_Mode;
					double QTotUnitOut = 0;
					double QSensUnitOut = 0;
					if (OutletEnthalpy < InletEnthalpy)
					{
						QTotUnitOut = OutletMassFlowRate * (OutletEnthalpy - InletEnthalpy);
						QSensUnitOut = OutletMassFlowRate * (PsyHFnTdbW(OutletTemp, OutletHumRat) - PsyHFnTdbW(InletTemp, OutletHumRat));
					}
					else
					{
						QTotUnitOut = 0;
						QSensUnitOut = 0;
					}

					UnitTotalCoolingRate = std::abs(min(0.0, QTotUnitOut));
					UnitTotalCoolingEnergy = UnitTotalCoolingRate * TimeStepSys * SecInHour;
					UnitSensibleCoolingRate = std::abs(min(0.0, optimal_H_sensible_room * 1000));
					UnitSensibleCoolingEnergy = UnitSensibleCoolingRate * TimeStepSys * SecInHour;
				
					ElectricalPower = optimal_power;
				
				}
				else
				{
					/**returnQSensible = 0;
					*returnQLatent = 0;
					*returnSupplyAirMassFlow = 0;
					*returnSupplyAirTemp = Tra;
					*returnSupplyAirRelHum = Wra;
					*returnVentilationAir = 0;
					*FMUmode = -2;
					*ElectricalPowerUse = 0;*/

					OutletRH = InletRH;
					OutletHumRat = InletHumRat;
					OutletEnthalpy = InletEnthalpy;
					OutletTemp = InletTemp;
					OutletMassFlowRate = InletMassFlowRate;
					Mode = -2;

					UnitTotalCoolingRate = 0;
					UnitTotalCoolingEnergy = 0;
					UnitSensibleCoolingRate = 0;
					UnitSensibleCoolingEnergy = 0;

					ElectricalPower = 0;

				}

			}
			else
			{ //current heating mode, do nothing
				UnitTotalCoolingRate = 0;//std::abs(min(0.0, QTotUnitOut));
				UnitTotalCoolingEnergy = 0;// ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate * TimeStepSys * SecInHour;
				UnitSensibleCoolingRate = 0;// std::abs(min(0.0, QSensUnitOut));
				UnitSensibleCoolingEnergy = 0;// ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate * TimeStepSys * SecInHour;
				/**returnQSensible = 0;
				*returnQLatent = 0;
				*returnSupplyAirMassFlow = 0;
				*returnSupplyAirTemp = Tra;
				*returnSupplyAirRelHum = Wra;
				*returnVentilationAir = 0;
				*FMUmode = -1;
				*ElectricalPowerUse = 0;*/

				SupplyVentilationAir = 0;
			}
			InitializeModelParams();
			//SetEnvironmentConditions

			//Iterate through modes of operation

			//	Generate point matrix using Tessellate function from operatiing conditions

			//	Calculate the number of modes based on the configuration file

			//	Iterate through solution spaceof mode to identify lowest energy consuming solution for each mode.



		}

	}
}

