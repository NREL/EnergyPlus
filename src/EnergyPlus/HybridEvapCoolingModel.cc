
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
#include <DataEnvironment.hh>
#include <DataZoneEquipment.hh>
// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
using namespace std;

namespace EnergyPlus {//***************

	namespace HybridEvapCoolingModel {
		using namespace std;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::NumOfSysTimeSteps;
		using DataHVACGlobals::NumOfSysTimeStepsLastZoneTimeStep;
		using DataHVACGlobals::LimitNumSysSteps;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::SecInHour;
		using namespace EnergyPlus::DataEnvironment;
		using namespace Psychrometrics;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::CurveValue;
		using EnergyPlus::CurveManager::GetNormalPoint;
		using ScheduleManager::GetCurrentScheduleValue;
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
			Minimum_Outside_Air_Temperature = min;
			Maximum_Outside_Air_Temperature = max;
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
			Min_OAF = minOSAF;
			Max_OAF = maxOSAF;
			return true;
		}
		bool CMode::InitializeMsaRatioConstraints(double minMsa, double maxMsa)
		{
			//minimum 0.00, maximum 1.00, Supply air mass flow rate ratios below this value will not be considered.
			//Supply air mass flow rate ratio describes supply air mass flow rate as a fraction of mass flow rate associated with the value in field : "System Maximum Supply Air Flow Rate".
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_Msa = minMsa;
			Max_Msa = maxMsa;
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
				if (ValidPointer(Tsa_curve_pointer))
				{
					Y_val = NormalizationReference*CurveValue(Tsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = X_3;//return air temp
				}
				break;
			case W_CURVE:

				if (ValidPointer(HRsa_curve_pointer))
				{
					Y_val = NormalizationReference*CurveValue(HRsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);//0.01;//
				}
				else {
					Y_val = X_4;//return HR
				}
				break;
			case POWER_CURVE:

				if (ValidPointer(Psa_curve_pointer))
				{
					Y_val = Correction*CurveValue(Psa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
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
				Tsa_curve_pointer = curve_ID;
				break;
			case W_CURVE:
				HRsa_curve_pointer = curve_ID;
				break;
			case POWER_CURVE:
				Psa_curve_pointer = curve_ID;
				break;
			default:
				break;
			}
		}

		bool CMode::GenerateSolutionSpace(double ResolutionMsa, double ResolutionOSA)
		{
			double deltaMsa = Max_Msa - Min_Msa;
			double deltaOAF = Max_OAF - Min_OAF;
			if (deltaMsa < ResolutionMsa) deltaMsa = ResolutionMsa;
			if (deltaOAF < ResolutionOSA) deltaOAF = ResolutionOSA;
			double Msastep_size = (deltaMsa * ResolutionMsa);
			double OAFsteps_size = (deltaOAF * ResolutionOSA);


			for (double Msa_val = Max_Msa; Msa_val >= Min_Msa; Msa_val = Msa_val - Msastep_size)
			{
				for (double OAF_val = Max_OAF; OAF_val >= Min_OAF; OAF_val = OAF_val - OAFsteps_size)
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
			if (OpperatingModes < ModeID) return false;
			return true;
		}
		bool CMode::ParseMode0(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{
			if (!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				ShowSevereError("Mode description array does not match mode number, in" + cCurrentModuleObject);
				return false;
			}

			bool ErrorsFound = false;
			int inter_Alpha = BLOCK_HEADER_OFFSET_Alpha + MODE_BLOCK_OFFSET_Alpha * ModeID;
			int inter_Number = BLOCK_HEADER_OFFSET_Number + MODE_BLOCK_OFFSET_Number* ModeID;
			std::ostringstream strs;
			strs << ModeID;

			int curveID = -1;
			if (lAlphaBlanks(inter_Alpha - 1)) {
				//InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
				ModeName = "Mode" + strs.str();
			}
			else
			{
				ModeName = Alphas(inter_Alpha - 1);
			}

			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
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
					ShowContinueError("Entered in " + cCurrentModuleObject);
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
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(POWER_CURVE, -1);
				}
				else { InitializeCurve(POWER_CURVE, curveID); }

			}
			//A21, \field Mode0 Supply Fan Electric Power Lookup Table Name
			//A22, \field Mode0 External Static Pressure Lookup Table Name
			//A23, \field Mode0 System Second Fuel Consumption Lookup Table Name
			//A24, \field Mode0 System Third Fuel Consumption Lookup Table Name
			//A25, \field Mode0 System Water Use Lookup Table Name
			//Numbers(inter_Number) //N6 field Mode0 Outside Air Fraction
								  //N7,field Mode0 Supply Air Mass Flow Rate Ratio
			//injest OSAF and MsaRatio setting.

			return false;
		}
		bool CMode::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{


			if (!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				ShowSevereError("Mode description array does not match mode number, in" + cCurrentModuleObject);
				return false;
			}
			int inter_Number;
			bool ErrorsFound = false;
			int inter_Alpha = BLOCK_HEADER_OFFSET_Alpha + MODE_BLOCK_OFFSET_Alpha * ModeID;
			if (ModeID == 1)
			{
				inter_Number = BLOCK_HEADER_OFFSET_Number + MODE1_BLOCK_OFFSET_Number* ModeID;
			}
			else
			{
				inter_Number = BLOCK_HEADER_OFFSET_Number + MODE1_BLOCK_OFFSET_Number + MODE_BLOCK_OFFSET_Number* (ModeID - 1);
			}
			std::ostringstream strs;
			strs << ModeID;

			int curveID = -1;
			if (lAlphaBlanks(inter_Alpha - 1)) {
				//as this is invalid curve id CalculateCurveVal will return a default 
				ModeName = "Mode" + strs.str();
			}
			else
			{
				ModeName = Alphas(inter_Alpha - 1);
			}

			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
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
					ShowContinueError("Entered in " + cCurrentModuleObject);
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
					ShowContinueError("Entered in " + cCurrentModuleObject);
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
			bool ok = InitializeOutsideAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1)); //Numbers(6), Numbers(7));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N8, \field Mode0  Minimum Outside Air Humidity Ratio
			//N9, \field Mode0  Maximum Outside Air Humidity Ratio
			ok = InitializeOutsideAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(8), Numbers(9));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N10, \field Mode0 Minimum Outside Air Relative Humidity
			//N11, \field Mode0 Maximum Outside Air Relative Humidity
			ok = InitializeOutsideAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(10), Numbers(11));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N12, \field Mode0 Minimum Return Air Temperature
			//N13, \field Mode0 Maximum Return Air Temperature
			ok = InitializeReturnAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(12), Numbers(13));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N14, \field Mode0 Minimum Return Air Humidity Ratio 
			//N15, \field Mode0 Maximum Return Air Humidity Ratio
			ok = InitializeReturnAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(14), Numbers(15))
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N16, \field Mode0 Minimum Return Air Relative HumidityInitialize
			//N17, \field Mode0 Maximum Return Air Relative Humidity
			ok = InitializeReturnAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(16), Numbers(17));
			if (!ok) {
				ShowSevereError("Invalid " + cAlphaFields(inter_Number) + '=' + Alphas(inter_Number) + "Or Invalid" + cAlphaFields(inter_Number + 1) + '=' + Alphas(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N18, \field Mode0 Minimum Outside Air Fraction
			//N19, \field Mode0 Maximum Outside Air Fraction

			ok = InitializeOSAFConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(18), Numbers(19));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			//N20, \field Mode0 Minimum Supply Air Mass Flow Rate Ratio
			//N21, \field Mode0 Maximum Supply Air Mass Flow Rate Ratio
			inter_Number = inter_Number + 2;
			ok = InitializeMsaRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));//Numbers(20), Numbers(21));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}

			return ErrorsFound;
		}


		bool CMode::MeetsOAEnvConstraints(double Tosa, double Wosa, double RHosa)
		{
			bool OATempConstraintmet = false;
			bool OAHRConstraintmet = false;
			bool OARHConstraintmet = false;
			//	double RHosa = Part_press(101.325, Wosa) / Sat_press(Tosa);

			if (Tosa >= Minimum_Outside_Air_Temperature && Tosa <= Maximum_Outside_Air_Temperature)
			{
				OATempConstraintmet = true;
			}

			if (Wosa >= Minimum_Outside_Air_Humidity_Ratio && Wosa <= Maximum_Outside_Air_Humidity_Ratio)
			{
				OAHRConstraintmet = true;
			}

			if (RHosa >= Minimum_Outside_Air_Relative_Humidity  && RHosa <= Maximum_Outside_Air_Relative_Humidity)
			{
				OARHConstraintmet = true;
			}
			if (OATempConstraintmet&&OAHRConstraintmet&&OARHConstraintmet)
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
			ZoneNum(0),
			SystemMaximumSupplyAirFlowRate(0),
			ScaledSystemMaximumSupplyAirMassFlowRate(0),
			ScalingFactor(0),
			SchedPtr(0),
			UnitTotalCoolingRate(0.0),
			UnitTotalCoolingEnergy(0.0),
			UnitSensibleCoolingRate(0.0),
			UnitSensibleCoolingEnergy(0.0),
			UnitLatentCoolingRate(0.0),
			UnitLatentCoolingEnergy(0.0),
			SystemTotalCoolingRate(0.0),
			SystemTotalCoolingEnergy(0.0),
			SystemSensibleCoolingRate(0.0),
			SystemSensibleCoolingEnergy(0.0),
			SystemLatentCoolingRate(0.0),
			SystemLatentCoolingEnergy(0.0),
			UnitTotalHeatingRate(0.0),
			UnitTotalHeatingEnergy(0.0),
			UnitSensibleHeatingRate(0.0),
			UnitSensibleHeatingEnergy(0.0),
			UnitLatentHeatingRate(0.0),
			UnitLatentHeatingEnergy(0.0),
			SystemTotalHeatingRate(0.0),
			SystemTotalHeatingEnergy(0.0),
			SystemSensibleHeatingRate(0.0),
			SystemSensibleHeatingEnergy(0.0),
			SystemLatentHeatingRate(0.0),
			SystemLatentHeatingEnergy(0.0),
			RequestedLoadToHeatingSetpoint(0.0),
			RequestedLoadToCoolingSetpoint(0.0),
			RequestedHumdificationMass(0.0),
		RequestedHumdificationLoad(0.0),
		RequestedHumdificationEnergy(0.0),	
			RequestedDeHumdificationMass(0.0),
			RequestedDeHumdificationLoad(0.0),
			RequestedDeHumdificationEnergy(0.0),
			PrimaryMode(0),
			PrimaryModeRuntimeFraction(0.0),
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
			OutletVolumetricFlowRate(0.0),
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
			Wsa(0.0), FinalElectricalPower(0.0), FinalElectricalEnergy(0.0), SupplyVentilationAir(0.0), SupplyVentilationVolume(0.0),
			Initialized(false),
			OutdoorAir(false),
			MinOA_Msa(0.0),
			UnitOn(0),
			averageOSAF(0)
		{
			WarnOnceFlag = false;
			//InitializeModelParams();
			Minimum_Supply_Air_Temp = 8; //must set this propoerly 
			cp = 1; //kJ/degreesC.kg default value, reset during calculation
			Lambna = 2260; //(kJ/kg) latent heat of vaporization ePlus should carry this an available variable
			count_EnvironmentConditionsMetOnce = 0;
			count_EnvironmentConditionsNotMet = 0;
			count_SAHR_OC_MetOnce = 0;
			count_SAT_OC_MetOnce = 0;
			count_DidWeMeetLoad = 0;
			count_DidWeNotMeetLoad = 0;
			vector<int> temp(25);
			SAT_OC_MetinMode_v= temp;
			SAHR_OC_MetinMode_v= temp;
			
			int MAXIMUM_OPERATIONAL_SETTINGS = 5;
			ModeCounter = 0;
			pStandBy = new CSetting;
			pOptimal= new CSetting;
			pSubOptimal = pOptimal;
			for (int i = 0; i< MAXIMUM_OPERATIONAL_SETTINGS; i++)
			{
				CSetting * pointer= new CSetting();
				pointer->ElectricalPower = 0;
				CurrentOperatingSettings.push_back(pointer);
			}
			InitializeModelParams();
		}
		//*************************************************
			void Model::ResetOutputs()
			{
				UnitTotalCoolingRate = 0;
				UnitTotalCoolingEnergy = 0;
				UnitSensibleCoolingRate = 0;
				UnitSensibleCoolingEnergy = 0;
				UnitLatentCoolingRate = 0;
				UnitLatentCoolingEnergy = 0;
				SystemTotalCoolingRate = 0;
				SystemTotalCoolingEnergy = 0;
				SystemSensibleCoolingRate = 0;
				SystemSensibleCoolingEnergy = 0;
				SystemLatentCoolingRate = 0;
				SystemLatentCoolingEnergy = 0;
				UnitTotalHeatingRate = 0;
				UnitTotalHeatingEnergy = 0;
				UnitSensibleHeatingRate = 0;
				UnitSensibleHeatingEnergy = 0;
				UnitLatentHeatingRate = 0;
				UnitLatentHeatingEnergy = 0;
				SystemTotalHeatingRate = 0;
				SystemTotalHeatingEnergy = 0;
				SystemSensibleHeatingRate = 0;
				SystemSensibleHeatingEnergy = 0;
				SystemLatentHeatingRate = 0;
				SystemLatentHeatingEnergy = 0;
				RequestedLoadToCoolingSetpoint = 0;
				RequestedHumdificationMass = 0;
				RequestedHumdificationLoad = 0;
				RequestedHumdificationEnergy = 0;
				RequestedDeHumdificationMass = 0;
				RequestedDeHumdificationLoad = 0;
				RequestedDeHumdificationEnergy = 0;
				PrimaryMode = 0;
				PrimaryModeRuntimeFraction = 0;

			}

		void Model::InitializeModelParams()
		{
			
			ResetOutputs();
			optimal_EnvCondMet = false;

			Tsa = 0;
			
			pOptimal->ElectricalPower = IMPLAUSIBLE_POWER;
			pSubOptimal->ElectricalPower = IMPLAUSIBLE_POWER;
			RunningPeakCapacity_EnvCondMet = false;
	
			Settings.clear();
			
		}

		Model::~Model()                 
		{
			
			list<CModeSolutionSpace*>::iterator iter;
			for (iter = SolutionSpaces.begin(); iter != SolutionSpaces.end(); ++iter) {
				CModeSolutionSpace* p = (*iter);
				delete p;
			}
			
			list<CSetting*>::iterator iterSettings;
			for (iterSettings = Settings.begin(); iterSettings != Settings.end(); ++iterSettings) {
				CSetting* ps = (*iterSettings);
				delete ps;
			}
			CurrentOperatingSettings.clear();

		}
		CMode::~CMode()                 // destructor, just an example
		{
	
		}

		// GetAge, Public accessor function
		// returns value of itsAge member
		int Model::GetID()
		{
			return ID;
		}


		CMode* Model::AddNewOperatingMode(double scaledCorrection)
		{
			CMode* pMode = new CMode;
			pMode->ModeID = ModeCounter;
			pMode->Correction = scaledCorrection;
			pMode->NormalizationReference = GetNormalPoint(1);
			ModeCounter++;
			OperatingModes.push_back(pMode);
			return pMode;
		}
		void Model::Initialize(int ZoneNumber)//, ConfigFile* pConfig)
		{
			ZoneNum = ZoneNumber;
			if (Initialized) return;
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

		bool Model::SetStandByMode(CMode* pMode0, CSetting *pStandBy, double Tosa, double Wosa, double Tra, double Wra)
		{
			bool error;
			CModeSolutionSpace* solutionspace = &(pMode0->sol);
			int solution_map_sizeX = solutionspace->PointX.size();
			double NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same.

			if (solution_map_sizeX > 0)
			{
				double MsaRatio = solutionspace->PointX[0];
				double Msa = NormalizationReference * MsaRatio;
				double OSAF = solutionspace->PointY[0];
				double ElectricalPower = pMode0->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, Msa, OSAF, 0, POWER_CURVE);
				pStandBy->ElectricalPower = ElectricalPower;
				pStandBy->Supply_Air_Mass_Flow_Rate = Msa;
				pStandBy->Supply_Air_Mass_Flow_Rate_Ratio = MsaRatio;
				pStandBy->Outside_Air_Fraction = OSAF;
				// for now just use return air but should use look up table
				pStandBy->SupplyAirTemperature = Tra;
				pStandBy->SupplyAirW = Wra;
				pStandBy->Mode = 0;
			}
			else
			{
				return true;
			}

			return false;
		}

		double Model::CalculateTimeStepAverage(SYSTEMOUTPUTS val)
		{
			double averagedVal = 0;
			double MassFlowDependentDenominator = 0;
			double value = 0;
			list<CSetting*>::iterator iterOperatingSettings;
			iterOperatingSettings = CurrentOperatingSettings.begin();
			CSetting* StandbyMode = (*iterOperatingSettings);
			for (; iterOperatingSettings != CurrentOperatingSettings.end(); ++iterOperatingSettings) {
				CSetting* po = (*iterOperatingSettings);
				// add up part runtime fractions
				switch (val)
				{
					case SYSTEMOUTPUTS::VENTILATION_AIR_V:  value = po->Supply_Air_Ventilation_Volume; break;
					case SYSTEMOUTPUTS::SYSTEM_FUEL_USE:  value = po->ElectricalPower; break;
					case SYSTEMOUTPUTS::SUPPLY_AIR_TEMP:  value = po->SupplyAirTemperature * po->Supply_Air_Mass_Flow_Rate; break;
					case SYSTEMOUTPUTS::MIXED_AIR_TEMP:  value = po->Mixed_Air_Temperature * po->Supply_Air_Mass_Flow_Rate; break;
					case SYSTEMOUTPUTS::SUPPLY_MASS_FLOW:  value = po->Supply_Air_Mass_Flow_Rate; break;
					case SYSTEMOUTPUTS::SUPPLY_AIR_HR:  value = po->SupplyAirW * po->Supply_Air_Mass_Flow_Rate; break;
					case SYSTEMOUTPUTS::MIXED_AIR_HR:  value = po->Mixed_Air_W * po->Supply_Air_Mass_Flow_Rate; break;
						
				}
				double part_run = po->Runtime_Fraction;
				averagedVal = averagedVal + value*part_run; 
				MassFlowDependentDenominator = po->Supply_Air_Mass_Flow_Rate*part_run + MassFlowDependentDenominator;
			}
			switch (val)
			{
				case SYSTEMOUTPUTS::SUPPLY_AIR_TEMP:  
					if (MassFlowDependentDenominator==0)
					{
						averagedVal = StandbyMode->SupplyAirTemperature;
					}
					else averagedVal = averagedVal/ MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::SUPPLY_AIR_HR:  
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode->SupplyAirW;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::MIXED_AIR_TEMP:  
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode->Mixed_Air_Temperature;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::MIXED_AIR_HR: 
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode->Mixed_Air_W;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
			}
			return averagedVal;
		}
		double Model::CalculatePartRuntimeFraction(double Mvent, double MinOA_Msa, double RequestedConditioningLoad,double SensibleRoomORZone, double RequestedLatentRoomORZone, double ZoneDehumidificationLoad, double requestedhumidificationload, double humidification)
		{
			double PLLatetRatio, PLVentRatio, PLSensibleRatio, PartRuntimeFraction;
			PLLatetRatio = PLVentRatio = PLSensibleRatio = 0;

			if (Mvent > 0)   PLVentRatio = MinOA_Msa / Mvent;
			PartRuntimeFraction = PLVentRatio;
			if (SensibleRoomORZone != 0)   PLSensibleRatio = abs(RequestedConditioningLoad) / abs(SensibleRoomORZone);
			if (PLSensibleRatio > PartRuntimeFraction) PartRuntimeFraction = PLSensibleRatio;
			if (ZoneDehumidificationLoad > 0)  PLLatetRatio = RequestedLatentRoomORZone / ZoneDehumidificationLoad;
			if (PLLatetRatio > PartRuntimeFraction) PartRuntimeFraction = PLLatetRatio;

			return PartRuntimeFraction;
		}

		int Model::SetOperatingSetting(CStepInputs StepIns)
		{


			double Requestedlatent = 0; //needs to come from EP
			bool DidWeMeetLoad = false;
			bool DidWeMeetHumidificaiton = false;
			bool DidWePartlyMeetLoad = false;
			using General::RoundSigDigits;
			int modenumber = 0;
			int point_number = 0;
			double MsaRatio = 0;
			double OSAF = 0;
			double Msa = 0;
			double Mvent = 0;
			double pOptimal_RunFractionTotalFuel = IMPLAUSIBLE_POWER;
			double EIR, ElectricalPower, SHR, Tma, Wma, Hsa, Hma, TotalSystem;
			//mode_optimal_power = 0;
			double PreviousMaxiumConditioningOutput = 0;
			double PreviousMaxiumHumidOrDehumidOutput = 0;
			
			
			if (StepIns.RHosa > 1) { ShowSevereError("Unitary hybrid system error, required RH 0-1"); } //should be fractional 
			if (StepIns.RHra > 1) { ShowSevereError("Unitary hybrid system error, required RH 0-1"); } //should be fractional 

			double Wosa = CalcHum_ratio_W(StepIns.Tosa, StepIns.RHosa, 101.325);
			double Wra = CalcHum_ratio_W(StepIns.Tra, StepIns.RHra, 101.325);
			bool EnvironmentConditionsMet, EnvironmentConditionsMetOnce, MinVRMet, MinVRMetOnce, SAT_OC_Met, SAT_OC_MetOnce, SARH_OC_Met, SAHR_OC_MetOnce;
			EnvironmentConditionsMetOnce = SAT_OC_Met = SAT_OC_MetOnce = SARH_OC_Met = SAHR_OC_MetOnce = false;

			MinOA_Msa = StepIns.MinimumOA; //?why you copy it before?
			//RequestedConditioningLoad = -StepIns.RequestedCoolingLoad / 1000; //convert to kw Cooling possitive now, heating negitive
			
			bool DehumidificationRequested = false;
			bool HumidificationRequested = false;
			// Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]
			if (StepIns.ZoneDehumidificationLoad < 0) DehumidificationRequested = true;
			// Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]
			if (StepIns.ZoneMoistureLoad > 0) HumidificationRequested = true;
			//RequestedHumdification = StepIns.ZoneMoistureLoad;
			//RequestedDeHumdification = StepIns.ZoneDehumidificationLoad;
			double averaged_requestedLoad = 0;

			std::list<CMode*>::const_iterator iterator;
			iterator = OperatingModes.begin();
			++iterator;
			for (; iterator != OperatingModes.end(); ++iterator) // iterate though the modes.
			{
				CMode* pMode = *iterator;
				CModeSolutionSpace* solutionspace = &(pMode->sol);
				bool SAHR_OC_MetinMode = false;
				bool SAT_OC_MetinMode = false;
				int solution_map_sizeX = solutionspace->PointX.size() - 1;
				int solution_map_sizeY = solutionspace->PointY.size() - 1;
				int solution_map_sizeM = solutionspace->PointMeta.size() - 1;
				if (solution_map_sizeX != solution_map_sizeY)
				{
					ShowWarningError("Error in solution space mapping, suggest adjusting operating constraints.");
					return -2;//error
				}
				// Check that in this mode the //Outside Air Relative Humidity(0 - 100 % )	//Outside Air Humidity Ratio(g / g)//Outside Air Temperature(°C)
				if (pMode->MeetsOAEnvConstraints(StepIns.Tosa, Wosa, 100 * StepIns.RHosa) == true)//fix this it should not end if it does not meet contraints.
				{
					EnvironmentConditionsMet = EnvironmentConditionsMetOnce = true;
				}
				else
				{
					EnvironmentConditionsMet = false;
					if (!WarmupFlag)
					{
						//ShowWarningError("Failed to meet environment conditions for mode: " + RoundSigDigits(double(pMode->ModeID), 1));
					}
				}

				if (EnvironmentConditionsMet == true)
				{
					double NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same.

					for (point_number = 0; point_number != solution_map_sizeX; point_number++) // within each mode go though all the combinations of solution spaces.
					{
						//Supply Air Mass Flow Rate(kg / s)
						//Outside Air Fraction(0 - 1)
						CSetting* pCandidateSetting = new CSetting;
						MsaRatio = solutionspace->PointX[point_number];// fractions of rated mass flow rate, so for some modes this might be low but others hi
						OSAF = solutionspace->PointY[point_number];
						// there is no operating condition test, becuase it uses those to make the map so only allowed values are here.
						Msa = NormalizationReference * MsaRatio;
						//Calculate the ventilation mass flow rate
						Mvent = Msa * OSAF;

						if (StdRhoAir>1) 	pCandidateSetting->Supply_Air_Ventilation_Volume = Mvent / StdRhoAir;
						else pCandidateSetting->Supply_Air_Ventilation_Volume = Mvent / 1.225;  //Why store it as a volume

						
						if (Mvent > MinOA_Msa) MinVRMet = MinVRMetOnce = true;
						else MinVRMet = false;

						if (MinVRMet)
						{
							//all these points meet the minimum VR requirement
							solutionspace->PointMeta[point_number] = 1;
							// Calculate prospective supply air temperature
							Tsa = pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, TEMP_CURVE); //TEMP_CURVE W_CURVE POWER_CURVE
																														// Check it meets constraints
							if (MeetsSupplyAirTOC(Tsa)) SAT_OC_Met = SAT_OC_MetOnce = SAT_OC_MetinMode = true;
							else SAT_OC_Met = false;

							// Calculate prospective supply air Humidity Ratio
							Wsa = pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, W_CURVE);
							//Return Air Relative Humidity(0 - 100 % ) //Return Air Humidity Ratio(g / g)
							if (MeetsSupplyAirRHOC(Wsa)) SARH_OC_Met = SAHR_OC_MetOnce = SAHR_OC_MetinMode = true;
							else SARH_OC_Met = false;


							if (SARH_OC_Met == true && SAT_OC_Met == true)
							{

								pCandidateSetting->Mode = pMode->ModeID;
								pCandidateSetting->Outside_Air_Fraction = OSAF;
								pCandidateSetting->Supply_Air_Mass_Flow_Rate_Ratio = MsaRatio;
								pCandidateSetting->Supply_Air_Mass_Flow_Rate = Msa;
								pCandidateSetting->pMode = pMode;
								pCandidateSetting->SupplyAirTemperature = Tsa;
								pCandidateSetting->SupplyAirW = Wsa;
								pCandidateSetting->Mode = pMode->ModeID;
								Settings.push_back(pCandidateSetting);

							}

						}
					}
				}
				if (!WarmupFlag)
				{

					if (SAT_OC_MetinMode == false)
					{
						SAT_OC_MetinMode_v[pMode->ModeID] = SAT_OC_MetinMode_v[pMode->ModeID] + 1;	
					}
					if (SAHR_OC_MetinMode == false)
					{
						SAHR_OC_MetinMode_v[pMode->ModeID] = SAHR_OC_MetinMode_v[pMode->ModeID] + 1;
					}
				}
				modenumber++;
			}

			std::list<CSetting*>::const_iterator iteratorSetting;
			for (iteratorSetting = Settings.begin(); iteratorSetting != Settings.end(); ++iteratorSetting) // iterate though the modes.
			{
				CSetting * pSetting = *iteratorSetting;
				//Calculate the delta H 
				OSAF = pSetting->Outside_Air_Fraction;
				Msa = pSetting->Supply_Air_Mass_Flow_Rate;
				Tsa = pSetting->SupplyAirTemperature;
				modenumber = pSetting->Mode;
				Wsa = pSetting->SupplyAirW;
				Tma = StepIns.Tra + OSAF * (StepIns.Tosa - StepIns.Tra);
				Wma = Wra + OSAF * (Wosa - Wra);
				pSetting->Mixed_Air_Temperature = Tma;
				pSetting->Mixed_Air_W = Wma;
				Hma = 1.006 * Tma * (2501 + 1.86 * Tma); //why was it like this?????
				Hma = PsyHFnTdbW(Tma, Wma); // !!!!!!!
				//
				double Hra = 1.006 * StepIns.Tra * (2501 + 1.86 * StepIns.Tra); //why was it like this?????
				Hra = PsyHFnTdbW(StepIns.Tra, Wra); // !!!!!!!

				Hsa = 1.006 * Tsa * (2501 + 1.86 * Tsa);
				Hsa = PsyHFnTdbW(Tsa, Wsa);

				
				double SupplyAirCp = PsyCpAirFnWTdb(Wsa, Tsa) / 1000;
				double ReturnAirCP = PsyCpAirFnWTdb(Wra, StepIns.Tra) / 1000;
				double OutsideAirCP = PsyCpAirFnWTdb(Wosa, StepIns.Tosa) / 1000;
				
				//System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
				//System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
				//System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}
				double SystemCp = ReturnAirCP + OSAF*(OutsideAirCP - ReturnAirCP) + SupplyAirCp;
				double SensibleSystem = Msa*0.5* SystemCp * (Tma - Tsa);//kw  dynamic cp
				double LatentSystem  = 2257 * Msa*(Wma - Wsa);
				TotalSystem = (Hma - Hsa) * Msa / 1000;     // Total system cooling
				//check
				double latentCheck = TotalSystem - SensibleSystem;
				
				//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
				//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
				//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}
				double SensibleRoomORZone = Msa*0.5*(SupplyAirCp + ReturnAirCP) * (StepIns.Tra - Tsa);//kw  dynamic cp
				double latentRoomORZone = 2257 * Msa*(Wra - Wsa);
				double TotalRoomORZone = (Hra - Hsa) * Msa / 1000;     // Total system cooling
				//check
				double LatentRoomORZone = TotalRoomORZone - SensibleRoomORZone;
			

				pSetting->TotalSystem = TotalSystem;
				pSetting->SensibleSystem = SensibleSystem;
				pSetting->LatentSystem = LatentSystem;
				pSetting->TotalZone = TotalRoomORZone;
				pSetting->SensibleZone = SensibleRoomORZone;
				pSetting->LatentZone = LatentRoomORZone;

				pSetting->Dehumidification = 0; //add

				bool Conditioning_load_met = false;
				if (CoolingRequested && SensibleRoomORZone > StepIns.RequestedCoolingLoad) Conditioning_load_met = true;
				if (HeatingRequested && SensibleRoomORZone < StepIns.RequestedHeatingLoad) Conditioning_load_met = true;
				if (!(HeatingRequested || CoolingRequested))  Conditioning_load_met = true;

				bool Humidification_load_met = false;
				//bool Humidification_load_met = false;
				double RequestedHumdificationLoad_kw = RequestedHumdificationLoad / 1000;
				if (DehumidificationRequested && LatentRoomORZone > RequestedHumdificationLoad_kw) Humidification_load_met = true;
				if (HumidificationRequested && LatentRoomORZone < RequestedHumdificationLoad_kw) Humidification_load_met = true;

				if (!(HumidificationRequested || DehumidificationRequested)) Humidification_load_met = true;

	
				double Y_val = pSetting->pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, POWER_CURVE); //fix modenumber not set
				ElectricalPower = Y_val;  //kW
				pSetting->ElectricalPower = ElectricalPower;

				//Calculate EIR and SHR
				EIR = ElectricalPower / TotalSystem; // maybe store and output this?
				SHR = SupplyAirCp * (Tma - Tsa) / (Hma - Hsa);

				// Calculate partload fraction required to meet all requirements
				double PartRuntimeFraction = 0;
				if (CoolingRequested)  PartRuntimeFraction = CalculatePartRuntimeFraction(pSetting->Supply_Air_Ventilation_Volume*StdRhoAir, MinOA_Msa, StepIns.RequestedCoolingLoad, SensibleRoomORZone, LatentRoomORZone, StepIns.ZoneDehumidificationLoad, LatentRoomORZone, StepIns.ZoneMoistureLoad);//
				if (HeatingRequested)  PartRuntimeFraction = CalculatePartRuntimeFraction(pSetting->Supply_Air_Ventilation_Volume*StdRhoAir, MinOA_Msa, StepIns.RequestedHeatingLoad, SensibleRoomORZone, LatentRoomORZone, StepIns.ZoneDehumidificationLoad, LatentRoomORZone, StepIns.ZoneMoistureLoad);//

				double RunFractionTotalFuel = ElectricalPower*PartRuntimeFraction; // fraction can be above 1 meaning its not able to do it completely in a time step.
				pSetting->Runtime_Fraction=PartRuntimeFraction;
				if (Conditioning_load_met && Humidification_load_met)
				{
					//store best performing mode
					if (RunFractionTotalFuel < pOptimal_RunFractionTotalFuel)
					{
						pOptimal_RunFractionTotalFuel = RunFractionTotalFuel;
						pOptimal = pSetting;
						DidWeMeetLoad = true;
						DidWeMeetHumidificaiton = true;
					}
				}
				else
				{
					if (!(DidWeMeetLoad&&DidWeMeetHumidificaiton))
					{
						bool store_best_attempt = false;

						if (Conditioning_load_met)
						{
							DidWeMeetLoad = true; 
							if (HumidificationRequested && (LatentRoomORZone < PreviousMaxiumHumidOrDehumidOutput)) store_best_attempt = true; //these may be backwards
							if (DehumidificationRequested && (LatentRoomORZone > PreviousMaxiumHumidOrDehumidOutput)) store_best_attempt = true;
							if (store_best_attempt) PreviousMaxiumHumidOrDehumidOutput = LatentRoomORZone;
						}
						else
						{
							if (!DidWeMeetLoad)
							{
								if (CoolingRequested && (SensibleRoomORZone > PreviousMaxiumConditioningOutput)) store_best_attempt = true;
								if (HeatingRequested && (SensibleRoomORZone < PreviousMaxiumConditioningOutput)) store_best_attempt = true;
								if (store_best_attempt) PreviousMaxiumConditioningOutput = SensibleRoomORZone;
							}
						}
						if (store_best_attempt)
						{
							pOptimal_RunFractionTotalFuel = RunFractionTotalFuel;
							pOptimal = pSetting;
							//DidWeMeetLoad = true;
							//DidWeMeetHumidificaiton = true;
							DidWePartlyMeetLoad = true;
						}
					}
				}
				
			}
			

			if (EnvironmentConditionsMetOnce == false)
			{

				ErrorCode = 1;
				count_EnvironmentConditionsNotMet++;
			
			}
			if (SAHR_OC_MetOnce == false)
			{

				count_SAHR_OC_MetOnce++;
				ErrorCode = 2;
			}
			if (SAT_OC_MetOnce == false)
			{
				count_SAT_OC_MetOnce++;
				ErrorCode = 3;
			}

			if (DidWeMeetLoad == true)
			{
				//add first operating mode
				ErrorCode = 0;
				list<CSetting*>::iterator iterOperatingSettings2 = CurrentOperatingSettings.begin();
		
				CSetting* pSetting = *iterOperatingSettings2;
				*pSetting = *pOptimal;
				pStandBy->Runtime_Fraction = (1 - pOptimal->Runtime_Fraction);
				PrimaryModeRuntimeFraction = pSetting->Runtime_Fraction;
				PrimaryMode = pSetting->Mode;
				iterOperatingSettings2++;
				pSetting = *iterOperatingSettings2;
				*pSetting = *pStandBy;
		
			}
			else if (DidWeMeetLoad == false && DidWePartlyMeetLoad == true)
			{
				ErrorCode = 0;

				count_DidWeNotMeetLoad++;

				if (pOptimal->ElectricalPower == IMPLAUSIBLE_POWER)
				{
					ShowWarningError("Model was not able to provide cooling for a time step, called in HybridEvapCooling:dostep");
					//cout << "Model was not able to provide cooling.";
					pOptimal->ElectricalPower = 0;
				}

				//CurrentOperatingSettings.push_back(pSubOptimal);
				list<CSetting*>::iterator iterOperatingSettings2 = CurrentOperatingSettings.begin();
				CSetting* pSetting = *iterOperatingSettings2;
				*pSetting = *pOptimal;
				PrimaryModeRuntimeFraction = 1;
				PrimaryMode = pSetting->Mode;

			}
			else
			{
				list<CSetting*>::iterator iterOperatingSettings2 = CurrentOperatingSettings.begin();
				CSetting* pSetting = *iterOperatingSettings2;
				*pSetting = *pStandBy;
				ErrorCode = -1;
				count_DidWeNotMeetLoad++;
			}
			if (EnvironmentConditionsMetOnce == false)
			{
				if (!WarmupFlag) // dto do  
				{
				//	ShowWarningError("In day " + RoundSigDigits((double)DayOfSim, 1) + " of simulation, system " + Name.c_str() + " was unable to operate for a timestep because environment conditions were beyond the allowable operating range for any mode.");
				}
				
			}

			bool foundwarnings = false;
			double TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
			if ((TimeElapsed > 24) && WarnOnceFlag&& !WarmupFlag)
			{
				//count_EnvironmentConditionsMetOnce
				if (count_EnvironmentConditionsNotMet>0) ShowWarningError("In day " + RoundSigDigits((double)DayOfSim, 1) + " of simulation, system " + Name.c_str() + " was unable to operate for " + RoundSigDigits((double)count_EnvironmentConditionsNotMet, 1)+ " timesteps because environment conditions were beyond the allowable operating range for any mode.");
				if (count_SAHR_OC_MetOnce>0)  ShowWarningError("In day " + RoundSigDigits((double)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air humidity ratio for " + RoundSigDigits(double(count_SAHR_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str() + " was set to mode 0");
				if (count_SAT_OC_MetOnce>0) ShowWarningError("In day " + RoundSigDigits((double)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air temperature constraints for " + RoundSigDigits(double(count_SAT_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str()+" was set to mode 0" );
				// In day [XX] of simulation, [object name] failed to meet supply air temperature constraints for [XXX] time steps.  For these time steps [object name] was set to mode 0.
				ShowWarningError("In day " + RoundSigDigits((double)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to  satisfy sensible load for " + RoundSigDigits((double)count_DidWeNotMeetLoad, 1) + " time steps. For these time steps settings were selected to provide as much sensible cooling or heating as possible, given other constraints.");
				//!!!! add these
				
				//** Warning ** In day[XX] of simulation, [object name] failed to satisfy sensible load for[XXX] time steps.For these time steps settings were selected to provide as much sensible cooling or heating as possible, given other constraints.
				//	** Warning ** In day[XX] of simulation, [object name] failed to satisfy latent load for[XXX] time steps.For these time steps settings were selected to provide as much dehumidification or humidification as possible, given other constraints.
				//	** Warning ** In day[XX] of simulation, [object name] failed to satisfy requested outdoor air ventilation rate for[XXX] time steps.For these time steps settings were selected to provide as much ventilation as possible, given other constraints.

				count_SAT_OC_MetOnce = 0;
				count_DidWeNotMeetLoad = 0;
				count_SAHR_OC_MetOnce = 0;
				count_EnvironmentConditionsMetOnce = 0;
				count_EnvironmentConditionsNotMet = 0;
				WarnOnceFlag = false;
				
			}
			if (HourOfDay == 1 && WarnOnceFlag==false && !WarmupFlag)
			{
				WarnOnceFlag = true;
			}
			return ErrorCode; //returning a class variable?
		
		}

		int Model::CurrentPrimaryMode()
		{
			list<CSetting*>::iterator iterOperatingSettings;
			if (CurrentOperatingSettings.size() > 0)
			{
				iterOperatingSettings = CurrentOperatingSettings.begin();
				CSetting* pPrimaryMode = (*iterOperatingSettings);
				return pPrimaryMode->Mode;
			}
			else return -1;
		}
		double Model::CurrentPrimaryRuntimeFraction()
		{
			list<CSetting*>::iterator iterOperatingSettings;
			if (CurrentOperatingSettings.size() > 0)
			{
				iterOperatingSettings = CurrentOperatingSettings.begin();
				CSetting* pPrimaryMode = (*iterOperatingSettings);
				return pPrimaryMode->Runtime_Fraction;
			}
			else return -1;
		
		}

		//doStep is passed some variables that could have just used the class members, but this adds clarity about whats needed, especially helpful in unit testing
		void Model::doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedCoolingLoad, double RequestedHeatingLoad, double OutputRequiredToHumidify, double OutputRequiredToDehumidify, double DesignMinVR) {


		
			MinOA_Msa = DesignMinVR;// as mass flow kg/s
			CStepInputs StepIns;
		
			RequestedHumdificationMass= OutputRequiredToHumidify;
			RequestedHumdificationLoad = OutputRequiredToHumidify * 2257; // [W];
			RequestedHumdificationEnergy = OutputRequiredToHumidify * 2257 * TimeStepSys * SecInHour; // [W]

			RequestedDeHumdificationMass = OutputRequiredToDehumidify;
			RequestedDeHumdificationLoad = OutputRequiredToDehumidify * 2257; // [W]; 
			RequestedDeHumdificationEnergy = OutputRequiredToDehumidify * 2257 * TimeStepSys * SecInHour; // [W] =

			StepIns.RequestedCoolingLoad = -RequestedCoolingLoad/1000; //convert to kw Cooling possitive now, heating negitive
			StepIns.RequestedHeatingLoad = -RequestedHeatingLoad/1000;
			StepIns.Tosa = Tosa; StepIns.Tra = Tra; StepIns.RHosa = RHosa; StepIns.RHra = RHra; StepIns.ZoneMoistureLoad = RequestedHumdificationLoad;  StepIns.ZoneDehumidificationLoad = RequestedDeHumdificationLoad; StepIns.MinimumOA = MinOA_Msa;
			double Wosa = CalcHum_ratio_W(Tosa, RHosa, 101.325);
			double Wra = CalcHum_ratio_W(Tra, RHra, 101.325);
			CoolingRequested = false;
			HeatingRequested = false;
			 
		//	RequestedLoadToCoolingSetpoint = StepIns.RequestedCoolingLoad;
		//	double RequestedConditioningLoad = -StepIns.RequestedCoolingLoad / 1000; //convert to kw Cooling possitive now, heating negitive

																			  //Heat Transfer Rate[W] = ... Moisture Transfer Rate{ kgWater / s } *L{ kJ / kgWater } where L {kJ/kgWater} = latent heat of phase change for water = 2,257 {kJ/kgWater}
			

																			  // establish if conditioning needed 
			if (StepIns.RequestedCoolingLoad >= MINIMUM_LOAD_TO_ACTIVATE)
				CoolingRequested = true;
			if (StepIns.RequestedHeatingLoad <= -MINIMUM_LOAD_TO_ACTIVATE)
				HeatingRequested = true;
		
			// the fist mode (mode0) is always the standby mode.
			std::list<CMode*>::const_iterator iterator;
			iterator = OperatingModes.begin();

			if (SetStandByMode(*iterator, pStandBy, Tosa, Wosa, Tra, Wra))
			{
				ShowSevereError("Standby mode not defined correctly");
			}
			// test availbility status
			UnitOn = 1;
			if (GetCurrentScheduleValue(SchedPtr) <= 0) {
				UnitOn = 0;
			}
			// go into standby if unit is off or not needed
			if (((CoolingRequested ==false)&& (HeatingRequested == false)) || !UnitOn)
			{
				pStandBy->Runtime_Fraction = 1;
				list<CSetting*>::iterator iterOperatingSettings2= CurrentOperatingSettings.begin();
				CSetting* pSetting = *iterOperatingSettings2;
				*pSetting = *pStandBy;
				ErrorCode = 0;
				PrimaryMode = 0;
				PrimaryModeRuntimeFraction = 0;
				//return;
			}
			else ErrorCode = SetOperatingSetting(StepIns);

				if (ErrorCode == 0)
				{
					SupplyVentilationAir = CalculateTimeStepAverage(SYSTEMOUTPUTS::VENTILATION_AIR_V);

					if (StdRhoAir>1) 	SupplyVentilationVolume = SupplyVentilationAir / StdRhoAir;
					else SupplyVentilationVolume = SupplyVentilationAir / 1.225;



					OutletTemp = CheckVal_T(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_TEMP));
					OutletHumRat = CheckVal_W(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_HR));
					
					OutletRH = PsyRhFnTdbWPb(OutletTemp, OutletHumRat, 101325); //dont use a fixed pressure !
					double OperatingAverageMixedAirTemperature = CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_TEMP);
					double OperatingMixedAirW= CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_HR);
					double MixedAirEnthalpy= PsyHFnTdbW(OperatingAverageMixedAirTemperature, OperatingMixedAirW);
					OutletEnthalpy = PsyHFnTdbRhPb(OutletTemp, OutletRH, InletPressure); // is the outlet presure going to be different? 
					OutletMassFlowRate = CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_MASS_FLOW);

					if (StdRhoAir>1) 	OutletVolumetricFlowRate = OutletMassFlowRate / StdRhoAir;
					else OutletVolumetricFlowRate = OutletMassFlowRate / 1.225;
					if (OutletMassFlowRate > 0) averageOSAF = SupplyVentilationAir / OutletMassFlowRate;
					else {
						ShowSevereError("Outlet air mass flow rate <=0");
						averageOSAF = 1;
					}
					PrimaryMode = CurrentPrimaryMode();
					PrimaryModeRuntimeFraction = CurrentPrimaryRuntimeFraction();
				
					double QTotZoneOut = 0;
					double QSensZoneOut = 0;
					double QLatentZoneOut = 0;

					double QTotSystemOut = 0;
					double QSensSystemOut = 0;
					double QLatentSystemOut = 0;
					double Outletcp = PsyCpAirFnWTdb(OutletHumRat, OutletTemp) / 1000;
					double Returncp = PsyCpAirFnWTdb(Wra, Tra) / 1000;
					double Outdoorcp = PsyCpAirFnWTdb(Wosa, Tosa) / 1000;

					if (CoolingRequested || HeatingRequested) //or dehumidification etc?
					{
		
						//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
						//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
						//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}		
						QSensZoneOut = 1000 *OutletMassFlowRate* 0.5* (Returncp+ Outletcp)*(InletTemp - OutletTemp);//kw  dynamic cp
						QLatentZoneOut = 1000 * 2257 * OutletMassFlowRate* (InletHumRat - OutletHumRat);
						QTotZoneOut = OutletMassFlowRate * (InletEnthalpy  - OutletEnthalpy);
						double QLatentCheck = QTotZoneOut - QSensZoneOut;
						
						//(OperatingMixedAirW - OutletHumRat)

						//System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
						//System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
						//System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}
						
						double SystemTimeStepCp = Returncp + averageOSAF*(Outdoorcp - Returncp) + Outletcp;//cpRA + OSAF*(cpOSA-cpRA) + cpSA
						double SystemTimeStepW = InletHumRat + averageOSAF*(Wosa - Wra) - OutletHumRat;//HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA
						double SystemTimeStepT = Tra + averageOSAF*(Tosa - Tra) - OutletTemp;//T_RA + OSAF *(T_OSA - T_RA) - T_SA
						QSensSystemOut = 1000* 0.5* SystemTimeStepCp *OutletMassFlowRate* SystemTimeStepT;//kw  dynamic cp
						QLatentSystemOut = 1000*2257 * OutletMassFlowRate*SystemTimeStepW;
						QTotSystemOut = OutletMassFlowRate * (MixedAirEnthalpy - OutletEnthalpy  );
						QLatentCheck = QTotSystemOut - QSensSystemOut;
					}
					else
					{
						QTotZoneOut = 0;
						QSensZoneOut = 0;
						QLatentZoneOut = 0;
						QTotSystemOut = 0;
						QSensSystemOut = 0;
						QLatentSystemOut = 0;

					}
					//set cooling specific outputs
					if (CoolingRequested )
					{
						UnitTotalCoolingRate = std::abs( QTotZoneOut);
						UnitTotalCoolingEnergy = UnitTotalCoolingRate * TimeStepSys * SecInHour;
						UnitSensibleCoolingRate = std::abs( QSensZoneOut );
						UnitSensibleCoolingEnergy = UnitSensibleCoolingRate * TimeStepSys * SecInHour;
						UnitLatentCoolingRate = UnitTotalCoolingRate - UnitSensibleCoolingRate;
						UnitLatentCoolingEnergy = UnitTotalCoolingEnergy - UnitSensibleCoolingEnergy;

						SystemTotalCoolingRate = std::abs( QTotSystemOut);
						SystemTotalCoolingEnergy = SystemTotalCoolingRate * TimeStepSys * SecInHour;
						SystemSensibleCoolingRate = std::abs( QSensSystemOut );
						SystemSensibleCoolingEnergy = SystemSensibleCoolingRate * TimeStepSys * SecInHour;
						SystemLatentCoolingRate = SystemTotalCoolingRate - SystemSensibleCoolingRate;
						SystemLatentCoolingEnergy = SystemTotalCoolingEnergy - SystemSensibleCoolingEnergy;
					}
					else
					{
						UnitTotalCoolingRate = UnitTotalCoolingEnergy = UnitSensibleCoolingRate = UnitSensibleCoolingEnergy= UnitLatentCoolingRate= UnitLatentCoolingEnergy = 0;
						SystemTotalCoolingRate = SystemTotalCoolingEnergy = SystemSensibleCoolingRate = SystemSensibleCoolingEnergy = SystemLatentCoolingRate = SystemLatentCoolingEnergy = 0;

					}
					// set heating specific outputs
					if (HeatingRequested)
					{
						UnitTotalHeatingRate = std::abs(QTotZoneOut);
						UnitTotalHeatingEnergy = UnitTotalHeatingRate * TimeStepSys * SecInHour;
						UnitSensibleHeatingRate = std::abs( QSensZoneOut );
						UnitSensibleHeatingEnergy = UnitSensibleHeatingRate * TimeStepSys * SecInHour;
						UnitLatentHeatingRate = UnitTotalHeatingRate - UnitSensibleHeatingRate;
						UnitLatentHeatingEnergy = UnitTotalHeatingEnergy - UnitSensibleHeatingEnergy;

						SystemTotalHeatingRate = std::abs( QTotSystemOut);
						SystemTotalHeatingEnergy = SystemTotalHeatingRate * TimeStepSys * SecInHour;
						SystemSensibleHeatingRate = std::abs( QSensSystemOut);
						SystemSensibleHeatingEnergy = SystemSensibleHeatingRate * TimeStepSys * SecInHour;
						SystemLatentHeatingRate = SystemTotalHeatingRate - SystemSensibleHeatingRate;
						SystemLatentHeatingEnergy = SystemTotalHeatingEnergy - SystemSensibleHeatingEnergy;
					}
					else
					{
						UnitTotalHeatingRate = UnitTotalHeatingEnergy = UnitSensibleHeatingRate = UnitSensibleHeatingEnergy = UnitLatentHeatingRate = UnitLatentHeatingEnergy = 0;
						SystemTotalHeatingRate = SystemTotalHeatingEnergy = SystemSensibleHeatingRate = SystemSensibleHeatingEnergy = SystemLatentHeatingRate = SystemLatentHeatingEnergy = 0;

					}
				FinalElectricalPower = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::SYSTEM_FUEL_USE);
				FinalElectricalEnergy= FinalElectricalPower*TimeStepSys * SecInHour;
				}
				else
				{
					OutletRH = InletRH;
					OutletHumRat = InletHumRat;
					OutletEnthalpy = InletEnthalpy;
					OutletTemp = InletTemp;
					OutletMassFlowRate = InletMassFlowRate;
					PrimaryMode = 0;
					PrimaryModeRuntimeFraction = 0;
					//UnitTotalCoolingRate = UnitTotalCoolingEnergy = UnitSensibleCoolingRate = UnitSensibleCoolingEnergy = FinalElectricalPower = 0;
					ResetOutputs();
				
				}
			

		}

	}
}
