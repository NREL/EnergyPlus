//EnergyPlus, Copyright(c) 1996 - 2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.


// C++ Headers
#include <HybridEvapCoolingModel.hh>

#include <UtilityRoutines.hh>

#include <string>
#include <list>
#include <math.h>
//#include <windows.h>
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


		bool CMode::InitializeOutsideAirTemperatureConstraints(Real64 min, Real64 max)
		{
			//note If this field is blank, there will be no lower constraint on outside air temperature
			Minimum_Outside_Air_Temperature = min;
			Maximum_Outside_Air_Temperature = max;
			return true;
		}
		bool CMode::InitializeOutsideAirHumidityRatioConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Outside_Air_Humidity_Ratio = min;
			Maximum_Outside_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeOutsideAirRelativeHumidityConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)

			Minimum_Outside_Air_Relative_Humidity = min;
			Maximum_Outside_Air_Relative_Humidity = max;
			return true;
		}
		bool CMode::InitializeReturnAirTemperatureConstraints(Real64 min, Real64 max)
		{
			//will not be considered when the return air temperature is below the value in this field.
			//If this field is blank, there will be no lower constraint on return air temperature
			Minimum_Return_Air_Temperature = min;
			Maximum_Return_Air_Temperature = max;
			return true;
		}
		bool CMode::InitializeReturnAirHumidityRatioConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Return_Air_Humidity_Ratio = min;
			Maximum_Return_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeReturnAirRelativeHumidityConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)
			Minimum_Return_Air_Relative_Humidity = min;
			Maximum_Return_Air_Relative_Humidity = max;
			return true;
		}


		bool CMode::InitializeOSAFConstraints(Real64 minOSAF, Real64 maxOSAF)
		{
			//minimum 0.00, maximum 1.00, Outside air fractions below this value will not be considered.
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_OAF = minOSAF;
			Max_OAF = maxOSAF;
			return true;
		}
		bool CMode::InitializeMsaRatioConstraints(Real64 minMsa, Real64 maxMsa)
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
		Real64 CMode::CalculateCurveVal(Real64 X_0, Real64 X_1, Real64 X_2, Real64 X_3, Real64 X_4, Real64 X_5, Real64 X_6, int mode_number, int curveType)
		{
			Real64 Y_val = 0;

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

		bool CMode::GenerateSolutionSpace(Real64 ResolutionMsa, Real64 ResolutionOSA)
		{
			Real64 deltaMsa = Max_Msa - Min_Msa;
			Real64 deltaOAF = Max_OAF - Min_OAF;
			if (deltaMsa < ResolutionMsa)
			{
				deltaMsa = ResolutionMsa;
			}
			if (deltaOAF < ResolutionOSA)
			{
				deltaOAF = ResolutionOSA;
			}
			Real64 Msastep_size = (deltaMsa * ResolutionMsa);
			Real64 OAFsteps_size = (deltaOAF * ResolutionOSA);


			for (Real64 Msa_val = Max_Msa; Msa_val >= Min_Msa; Msa_val = Msa_val - Msastep_size)
			{
				for (Real64 OAF_val = Max_OAF; OAF_val >= Min_OAF; OAF_val = OAF_val - OAFsteps_size)
				{
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
			if (MinimumExpectedLength > parmsnumber)
			{
				return false;
			}
			if (OpperatingModes < ModeID)
			{
				return false;
			}
			return true;
		}
		bool CMode::ParseMode0(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{
			if (!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				//this error should never really occur under normal operation of the model, as the model will only attempt to injest the number of modes specified in the IDF 
				ShowSevereError("There was a misalignment between the number of modes spcified in the idf and the number of mode objects generated, in" + cCurrentModuleObject);
				return false;
			}

			bool ErrorsFound = false;
			int inter_Alpha = BLOCK_HEADER_OFFSET_Alpha + MODE_BLOCK_OFFSET_Alpha * ModeID;
			int inter_Number = BLOCK_HEADER_OFFSET_Number + MODE_BLOCK_OFFSET_Number* ModeID;
			std::ostringstream strs;
			strs << ModeID;

			int curveID = -1;
			if (lAlphaBlanks(inter_Alpha - 1)) {
				ModeName = "Mode" + strs.str();
			}
			else
			{
				ModeName = Alphas(inter_Alpha - 1);
			}

			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default when called
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
				InitializeCurve(POWER_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default when called
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

			return false;
		}
		bool CMode::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{
			if (!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				ShowSevereError("There was a misalignment between the number of modes spcified in the idf and the number of mode objects generated, in" + cCurrentModuleObject);
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
				ModeName = "Mode" + strs.str();
			}
			else
			{
				ModeName = Alphas(inter_Alpha - 1);
			}

			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default when called
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

			//A20, \field Mode0 Supply Air Humidity Ratio Lookup Table Name
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(W_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default  when called
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
			bool ok = InitializeOutsideAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1)); 
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N8, \field Mode0  Minimum Outside Air Humidity Ratio
			//N9, \field Mode0  Maximum Outside Air Humidity Ratio
			ok = InitializeOutsideAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N10, \field Mode0 Minimum Outside Air Relative Humidity
			//N11, \field Mode0 Maximum Outside Air Relative Humidity
			ok = InitializeOutsideAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N12, \field Mode0 Minimum Return Air Temperature
			//N13, \field Mode0 Maximum Return Air Temperature
			ok = InitializeReturnAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N14, \field Mode0 Minimum Return Air Humidity Ratio 
			//N15, \field Mode0 Maximum Return Air Humidity Ratio
			ok = InitializeReturnAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N16, \field Mode0 Minimum Return Air Relative HumidityInitialize
			//N17, \field Mode0 Maximum Return Air Relative Humidity
			ok = InitializeReturnAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cAlphaFields(inter_Number) + '=' + Alphas(inter_Number) + "Or Invalid" + cAlphaFields(inter_Number + 1) + '=' + Alphas(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N18, \field Mode0 Minimum Outside Air Fraction
			//N19, \field Mode0 Maximum Outside Air Fraction

			ok = InitializeOSAFConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			//N20, \field Mode0 Minimum Supply Air Mass Flow Rate Ratio
			//N21, \field Mode0 Maximum Supply Air Mass Flow Rate Ratio
			inter_Number = inter_Number + 2;
			ok = InitializeMsaRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}

			return ErrorsFound;
		}


		bool CMode::MeetsOAEnvConstraints(Real64 Tosa, Real64 Wosa, Real64 RHosa)
		{
			bool OATempConstraintmet = false;
			bool OAHRConstraintmet = false;
			bool OARHConstraintmet = false;

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
			if (OATempConstraintmet && OAHRConstraintmet && OARHConstraintmet)
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		bool Model::MeetsSupplyAirTOC(Real64 Tosa)
		{
		
			Real64 MinSAT = 10;
			Real64 MaxSAT = 20;
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

		bool Model::MeetsSupplyAirRHOC(Real64 Wosa)
		{
			Real64 MinRH = 0;
			Real64 MaxRH = 1;
			if (RHsaMin_schedule_pointer > 0) {
				MinRH = GetCurrentScheduleValue(RHsaMin_schedule_pointer);
			}
			if (RHsaMax_schedule_pointer > 0) {
				MaxRH = GetCurrentScheduleValue(RHsaMax_schedule_pointer);
			}
			if (Wosa < MinRH || Wosa > MaxRH)
				return false;
			return true;
		}

		Model::Model() :
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
			Wsa(0.0),
			FinalElectricalPower(0.0),
			FinalElectricalEnergy(0.0),
			SupplyVentilationAir(0.0),
			SupplyVentilationVolume(0.0),
			Initialized(false),
			OutdoorAir(false),
			MinOA_Msa(0.0),
			UnitOn(0),
			averageOSAF(0)
		{
			WarnOnceFlag = false;
			cp = 1; //kJ/degreesC.kg default value, reset during calculation
			Lambna = 2260; //(kJ/kg) latent heat of vaporization ePlus should carry this an available variable
			count_EnvironmentConditionsMetOnce = 0;
			count_EnvironmentConditionsNotMet = 0;
			count_SAHR_OC_MetOnce = 0;
			count_SAT_OC_MetOnce = 0;
			count_DidWeMeetLoad = 0;
			count_DidWeNotMeetLoad = 0;
			// vector below used store the modes in each timestep that don't meet humidity or temperature limits, used in warnings
			vector<int> temp(25);
			SAT_OC_MetinMode_v= temp;
			SAHR_OC_MetinMode_v= temp;
			
			int MAXIMUM_OPERATIONAL_SETTINGS = 5;
			ModeCounter = 0;
			//these are the three setting types currently implimented, pOptimal is the setting that meets conditioning load. 
			pStandBy = new CSetting;
			pOptimal= new CSetting;
			

			for (int i = 0; i< MAXIMUM_OPERATIONAL_SETTINGS; i++)
			{
				CSetting * pointer= new CSetting();
				pointer->ElectricalPower = 0;
				CurrentOperatingSettings.push_back(pointer);
			}
			InitializeModelParams();
		}
		
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
			//reset the power use to a high value, this is replaced during the calculation keeping the "best" setting. 
			pOptimal->ElectricalPower = IMPLAUSIBLE_POWER;
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
		CMode::~CMode()  
		{
	
		}

		int Model::GetID()
		{
			return ID;
		}

		CMode* Model::AddNewOperatingMode(Real64 scaledCorrection)
		{
			CMode* pMode = new CMode;
			pMode->ModeID = ModeCounter;
			pMode->Correction = scaledCorrection;
			pMode->NormalizationReference = GetNormalPoint(1);
			ModeCounter++;
			OperatingModes.push_back(pMode);
			return pMode;
		}
		void Model::Initialize(int ZoneNumber)
		{
			ZoneNum = ZoneNumber;
			if (Initialized){
				return;
			}
			Initialized = true;
			// this variable, at this point hard coded, sets the fidelity of the matrix of possible OSAF and Msa combinations. 
			ResolutionMsa = 0.2; //msa/msaRATED
			ResolutionOSA = 0.2; //OSAF as absolute fraction (not %)
		
			//Iterate through modes of operation generating a matrix of OSAF and Msa to test in the algorithm.
			list<CMode*>::iterator iterModes;

			for (iterModes = OperatingModes.begin(); iterModes != OperatingModes.end(); ++iterModes) {
				CMode* Mode = (*iterModes);
				Mode->GenerateSolutionSpace(ResolutionMsa, ResolutionOSA);
			}
			Initialized = true;
		}


		void Model::ModelLog(std::string fmi_logmessage)
		{
			std::string logmessage(fmi_logmessage);
			cout << logmessage;
		}


		Real64 Model::Part_press(Real64 P, Real64 W)
		{
			// Function to compute partial vapor pressure in [kPa]
			// From page 6.9 equation 38 in ASHRAE Fundamentals handbook (2005)
			//   P = ambient pressure [kPa]
			//   W = humidity ratio [kg/kg dry air]

			return (P * W / (0.62198 + W));
		}
		
		Real64 Model::Sat_press(Real64 Tdb)
		{
			// Function to compute saturation vapor pressure in [kPa]
			//ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
			//   Tdb = Dry bulb temperature [degC]
			// Valid from -100C to 200 C

			Real64  C1 = -5674.5359;
			Real64  C2 = 6.3925247;
			Real64  C3 = -0.009677843;
			Real64  C4 = 0.00000062215701;
			Real64  C5 = 2.0747825E-09;
			Real64  C6 = -9.484024E-13;
			Real64  C7 = 4.1635019;
			Real64  C8 = -5800.2206;
			Real64  C9 = 1.3914993;
			Real64  C10 = -0.048640239;
			Real64  C11 = 0.000041764768;
			Real64  C12 = -0.000000014452093;
			Real64  C13 = 6.5459673;
			Real64  Sat_press_val = 0;

			Real64   TK = Tdb + 273.15;         //Converts from degC to degK

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
		

		Real64 Model::CalcHum_ratio_W(Real64 Tdb, Real64 RH, Real64 P)
		{
			// Function to calculate humidity ratio [kg H2O/kg air]
			// Given dry bulb and wet bulb temperature inputs [degC]
			// ASHRAE Fundamentals handbood (2005)
			//   Tdb = Dry bulb temperature [degC]
			//   RH = Relative Humidity [Fraction or %]
			//   P = Ambient Pressure [kPa]

			Real64 Pws = Sat_press(Tdb);
			Real64 Hum_rat = 0.62198 * RH * Pws / (P - RH * Pws);   // Equation 22, 24, p6.8
			return Hum_rat;
		}

		Real64 Model::CheckVal_W(Real64 W)
		{
			if ((W > 1) || (W < 0))
			{
				ShowWarningError("Humidity ratio exceeded realistic range, check performance curve");
			}
			return W;
		}
		Real64 Model::CheckVal_T(Real64 T)
		{
			if ((T > 100) || (T < 0))
			{
				ShowWarningError("Supply air temperature exceeded realistic range, check performance curve");
			}
			return T;
		}



		bool Model::SetStandByMode(CMode* pMode0, CSetting *pStandBy, Real64 Tosa, Real64 Wosa, Real64 Tra, Real64 Wra)
		{
			bool error;
			CModeSolutionSpace* solutionspace = &(pMode0->sol);
			int solution_map_sizeX = solutionspace->PointX.size();
			Real64 NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same, this is explained in the IO reference

			if (solution_map_sizeX > 0)
			{
				Real64 MsaRatio = solutionspace->PointX[0];
				Real64 Msa = NormalizationReference * MsaRatio;
				Real64 OSAF = solutionspace->PointY[0];
				Real64 ElectricalPower = pMode0->CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, Msa, OSAF, 0, POWER_CURVE);
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

		Real64 Model::CalculateTimeStepAverage(SYSTEMOUTPUTS val)
		{
			Real64 averagedVal = 0;
			Real64 MassFlowDependentDenominator = 0;
			Real64 value = 0;
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
				Real64 part_run = po->Runtime_Fraction;
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

		Real64 Model::CalculatePartRuntimeFraction(Real64 MinOA_Msa, Real64 Mvent, Real64 RequestedCoolingLoad, Real64 RequestedHeatingLoad, Real64 SensibleRoomORZone, Real64 RequestedDehumidificationLoad, Real64 RequestedMoistureLoad, Real64 LatentRoomORZone)
		{
			Real64 PLHumidRatio, PLDehumidRatio, PLVentRatio, PLSensibleCoolingRatio, PLSensibleHeatingRatio, PartRuntimeFraction;
			PLHumidRatio=PLDehumidRatio = PLVentRatio = PLSensibleCoolingRatio = PLSensibleHeatingRatio = 0;

			if (Mvent > 0) {
				PLVentRatio = MinOA_Msa / Mvent;
			}
			PartRuntimeFraction = PLVentRatio; 
			
			if (SensibleRoomORZone > 0) {
				PLSensibleCoolingRatio = abs(RequestedCoolingLoad) / abs(SensibleRoomORZone);
			}
			if (PLSensibleCoolingRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLSensibleCoolingRatio;
			}
			
			if (SensibleRoomORZone > 0) {
				PLSensibleHeatingRatio = abs(RequestedHeatingLoad) / abs(SensibleRoomORZone);
			}

			if (PLSensibleHeatingRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLSensibleHeatingRatio;
			}

			if (RequestedDehumidificationLoad > 0) { 
				PLDehumidRatio = abs(RequestedDehumidificationLoad) / abs(LatentRoomORZone); 
			}
			
			if (PLDehumidRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLDehumidRatio;
			}

			if (RequestedMoistureLoad > 0) {
				PLHumidRatio = abs(RequestedMoistureLoad) / abs(LatentRoomORZone);
			}
			if (PLHumidRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLHumidRatio;
			}

			if (PartRuntimeFraction < 0)
			{
				PartRuntimeFraction = 0;
			}
			if (PartRuntimeFraction > 1)
			{
				PartRuntimeFraction = 1;
			}

			return PartRuntimeFraction;
		}

		int Model::SetOperatingSetting(CStepInputs StepIns)
		{
			using General::RoundSigDigits;
			bool DidWeMeetLoad = false;
			bool DidWeMeetHumidificaiton = false;
			bool DidWePartlyMeetLoad = false;
			int modenumber = 0;
			int point_number = 0;
			Real64 MsaRatio = 0;
			Real64 OSAF = 0;
			Real64 Msa = 0;
			Real64 Mvent = 0;
			Real64 pOptimal_RunFractionTotalFuel = IMPLAUSIBLE_POWER;
			Real64 pVentOnly_RunFractionTotalFuel = IMPLAUSIBLE_POWER;
			Real64 EIR;
			Real64 ElectricalPower;
			Real64 SHR;
			Real64 Tma;
			Real64 Wma;
			Real64 Hsa;
			Real64 Hma;
			Real64 TotalSystem;
			Real64 PreviousMaxiumConditioningOutput = 0;
			Real64 PreviousMaxiumHumidOrDehumidOutput = 0;
			std::string ObjectID = Name.c_str();
			
			if (StepIns.RHosa > 1) 
			{ 
				ShowSevereError("Unitary hybrid system error, required relative humidity value 0-1, called in object" + ObjectID + ".Check inputs" ); 
				return -1;
			} //because it should be fractional, this should only really be possible if its called from a unit test
			
			if (StepIns.RHra > 1) 
			{ 
				ShowSevereError("Unitary hybrid system error,  required relative humidity value 0-1, called in object" + ObjectID + ".Check inputs");
				return -1;
			} //because it should be fractional, this should only really be possible if its called from a unit test

			Real64 Wosa = CalcHum_ratio_W(StepIns.Tosa, StepIns.RHosa, 101.325);
			Real64 Wra = CalcHum_ratio_W(StepIns.Tra, StepIns.RHra, 101.325);
			bool EnvironmentConditionsMet, EnvironmentConditionsMetOnce, MinVRMet, MinVRMetOnce, SAT_OC_Met, SAT_OC_MetOnce, SARH_OC_Met, SAHR_OC_MetOnce;
			EnvironmentConditionsMetOnce = SAT_OC_Met = SAT_OC_MetOnce = SARH_OC_Met = SAHR_OC_MetOnce = false;

			MinOA_Msa = StepIns.MinimumOA; // Set object version of minimum VR Kg/s
			
			DehumidificationRequested = false;
			HumidificationRequested = false;

			// Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]
			if (StepIns.ZoneDehumidificationLoad < 0)
			{
				DehumidificationRequested = true;
			}
			// Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]
			if (StepIns.ZoneMoistureLoad > 0)
			{
				HumidificationRequested = true;
			}
			//RequestedHumdification = StepIns.ZoneMoistureLoad;
			//RequestedDeHumdification = StepIns.ZoneDehumidificationLoad;
			Real64 averaged_requestedLoad = 0;

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
					return -2;
				}
				// Check that in this mode the //Outside Air Relative Humidity(0 - 100 % )	//Outside Air Humidity Ratio(g / g)//Outside Air Temperature(°C)
				if (pMode->MeetsOAEnvConstraints(StepIns.Tosa, Wosa, 100 * StepIns.RHosa)) 
				{
					EnvironmentConditionsMet = EnvironmentConditionsMetOnce = true;
				}
				else
				{
					EnvironmentConditionsMet = false;
				}

				if (EnvironmentConditionsMet)
				{
					Real64 NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same.
					for (point_number = 0; point_number != solution_map_sizeX; point_number++) // within each mode go though all the combinations of solution spaces.
					{
						//Supply Air Mass Flow Rate(kg / s)
						//Outside Air Fraction(0 - 1)
						CSetting* pCandidateSetting = new CSetting;
						MsaRatio = solutionspace->PointX[point_number];// fractions of rated mass flow rate, so for some modes this might be low but others hi
						OSAF = solutionspace->PointY[point_number];
						Msa = NormalizationReference * MsaRatio;
						//Calculate the ventilation mass flow rate
						Mvent = Msa * OSAF;

						if (StdRhoAir > 1)
						{
							pCandidateSetting->Supply_Air_Ventilation_Volume = Mvent / StdRhoAir;
						}
						else
						{
							pCandidateSetting->Supply_Air_Ventilation_Volume = Mvent / 1.225;  //stored as volumetric flow for reporting
						}

						if (Mvent > MinOA_Msa)
						{
							MinVRMet = MinVRMetOnce = true;
						}
						else
						{
							MinVRMet = false;
						}

						if (MinVRMet)
						{
							//all these points meet the minimum VR requirement
							solutionspace->PointMeta[point_number] = 1;
							// Calculate prospective supply air temperature
							Tsa = pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, TEMP_CURVE);
							// Check it meets constraints
							if (MeetsSupplyAirTOC(Tsa))
							{
								SAT_OC_Met = SAT_OC_MetOnce = SAT_OC_MetinMode = true;
							}
							else
							{
								SAT_OC_Met = false;
							}
							// Calculate prospective supply air Humidity Ratio
							Wsa = pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, W_CURVE);
							//Return Air Relative Humidity(0 - 100 % ) //Return Air Humidity Ratio(g / g)
							if (MeetsSupplyAirRHOC(Wsa))
							{
								SARH_OC_Met = SAHR_OC_MetOnce = SAHR_OC_MetinMode = true;
							}
							else
							{
								SARH_OC_Met = false;
							}

							if (SARH_OC_Met && SAT_OC_Met)
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
					// Keep an account of the number of times the supply air temperature and humidity constraints were not met for a given mode but only do this when its not warmup. 
					if (!SAT_OC_MetinMode)
					{
						SAT_OC_MetinMode_v[pMode->ModeID] = SAT_OC_MetinMode_v[pMode->ModeID] + 1;	
					}
					if (!SAHR_OC_MetinMode)
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
			    
				Hma = PsyHFnTdbW(Tma, Wma); 
				// Calculate Enthalpy of return air
				Real64 Hra =  PsyHFnTdbW(StepIns.Tra, Wra); 

				Hsa = 1.006 * Tsa * (2501 + 1.86 * Tsa);
				Hsa = PsyHFnTdbW(Tsa, Wsa);

				Real64 SupplyAirCp = PsyCpAirFnWTdb(Wsa, Tsa) / 1000;
				Real64 ReturnAirCP = PsyCpAirFnWTdb(Wra, StepIns.Tra) / 1000;
				Real64 OutsideAirCP = PsyCpAirFnWTdb(Wosa, StepIns.Tosa) / 1000;
				
				//System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
				//System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
				//System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}
				Real64 SystemCp = ReturnAirCP + OSAF*(OutsideAirCP - ReturnAirCP) + SupplyAirCp;
				Real64 SensibleSystem = Msa*0.5* SystemCp * (Tma - Tsa);//kw  dynamic cp
				Real64 MsaDry = Msa*(1 - Wsa);
				Real64 LatentSystem  = 2257 * MsaDry*(Wma - Wsa);
				// Total system cooling
				TotalSystem = (Hma - Hsa) * Msa / 1000;    
				// Perform latent check
				Real64 latentCheck = TotalSystem - SensibleSystem;
				
				//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
				//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
				//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}
				Real64 SensibleRoomORZone = Msa*0.5*(SupplyAirCp + ReturnAirCP) * (StepIns.Tra - Tsa);//kw  dynamic cp
				Real64 latentRoomORZone = 2257 * MsaDry*(Wra - Wsa);
				// Total room cooling
				Real64 TotalRoomORZone = (Hra - Hsa) * Msa / 1000;     
				//Perform latent check
				Real64 LatentRoomORZone = TotalRoomORZone - SensibleRoomORZone;

				pSetting->TotalSystem = TotalSystem;
				pSetting->SensibleSystem = SensibleSystem;
				pSetting->LatentSystem = LatentSystem;
				pSetting->TotalZone = TotalRoomORZone;
				pSetting->SensibleZone = SensibleRoomORZone;
				pSetting->LatentZone = LatentRoomORZone;

				bool Conditioning_load_met = false;
				if (CoolingRequested && (SensibleRoomORZone > StepIns.RequestedCoolingLoad))
				{
					Conditioning_load_met = true;
				}
				if (HeatingRequested && (SensibleRoomORZone < StepIns.RequestedHeatingLoad))
				{
					Conditioning_load_met = true;
				}
				if (!(HeatingRequested || CoolingRequested))
				{
					Conditioning_load_met = true;
				}

				bool Humidification_load_met = false;
				//bool Humidification_load_met = false;
				Real64 RequestedHumdificationLoad_kw = RequestedHumdificationLoad / 1000;
				if (DehumidificationRequested && LatentRoomORZone > RequestedHumdificationLoad_kw)
				{
					Humidification_load_met = true;
				}
				if (HumidificationRequested && LatentRoomORZone < RequestedHumdificationLoad_kw)
				{
					Humidification_load_met = true;
				}

				if (!(HumidificationRequested || DehumidificationRequested))
				{
					Humidification_load_met = true;
				}
	
				Real64 Y_val = pSetting->pMode->CalculateCurveVal(1, StepIns.Tosa, Wosa, StepIns.Tra, Wra, Msa, OSAF, modenumber, POWER_CURVE); //fix modenumber not set
				ElectricalPower = Y_val;  // [Kw] calculations for fuel in Kw
				pSetting->ElectricalPower = ElectricalPower;

				//Calculate EIR and SHR
				EIR = ElectricalPower / TotalSystem; 
				SHR = SupplyAirCp * (Tma - Tsa) / (Hma - Hsa);

				// Calculate partload fraction required to meet all requirements
				Real64 PartRuntimeFraction = 0;
				PartRuntimeFraction = CalculatePartRuntimeFraction(MinOA_Msa, pSetting->Supply_Air_Ventilation_Volume*StdRhoAir, StepIns.RequestedCoolingLoad, StepIns.RequestedHeatingLoad, SensibleRoomORZone, StepIns.ZoneDehumidificationLoad, StepIns.ZoneMoistureLoad, LatentRoomORZone);//
			
				Real64 RunFractionTotalFuel = ElectricalPower*PartRuntimeFraction; // fraction can be above 1 meaning its not able to do it completely in a time step.
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
					if (!DidWeMeetLoad && DidWeMeetHumidificaiton)
					{
						bool store_best_attempt = false;

						if (Conditioning_load_met)
						{
							DidWeMeetLoad = true; 
							if (HumidificationRequested && (LatentRoomORZone < PreviousMaxiumHumidOrDehumidOutput))
							{
								store_best_attempt = true;
							}
							if (DehumidificationRequested && (LatentRoomORZone > PreviousMaxiumHumidOrDehumidOutput))
							{
								store_best_attempt = true;
							}
							if (store_best_attempt)
							{
								PreviousMaxiumHumidOrDehumidOutput = LatentRoomORZone;
							}
						}
						else
						{
							if (!DidWeMeetLoad)
							{
								if (CoolingRequested && (SensibleRoomORZone > PreviousMaxiumConditioningOutput))
								{
									store_best_attempt = true;
								}
								if (HeatingRequested && (SensibleRoomORZone < PreviousMaxiumConditioningOutput))
								{
									store_best_attempt = true;
								}
								if (store_best_attempt)
								{
									PreviousMaxiumConditioningOutput = SensibleRoomORZone;
								}
							}
						}
						if (store_best_attempt)
						{
							pOptimal_RunFractionTotalFuel = RunFractionTotalFuel;
							pOptimal = pSetting;
							DidWePartlyMeetLoad = true;
						}
					}
				}
				
			}

			if (!EnvironmentConditionsMetOnce)
			{
				ErrorCode = 1;
				count_EnvironmentConditionsNotMet++;
			}
			if (!SAHR_OC_MetOnce)
			{
				count_SAHR_OC_MetOnce++;
				ErrorCode = 2;
			}
			if (!SAT_OC_MetOnce)
			{
				count_SAT_OC_MetOnce++;
				ErrorCode = 3;
			}

			if (DidWeMeetLoad)
			{
				//add first setting to operating modes
				ErrorCode = 0;
				list<CSetting*>::iterator iterOperatingSettings = CurrentOperatingSettings.begin();
				CSetting* pSetting = *iterOperatingSettings;
				*pSetting = *pOptimal;
				Real64 SecondaryRunFraction = 0;
				PrimaryModeRuntimeFraction=pOptimal->Runtime_Fraction;
				iterOperatingSettings++;
				pStandBy->Runtime_Fraction = (1 - PrimaryModeRuntimeFraction );
				if (pStandBy->Runtime_Fraction < 0)
				{
					pStandBy->Runtime_Fraction = 0;
				}
				// add standby mode
				pSetting = *iterOperatingSettings;
				*pSetting = *pStandBy;
			}
			else
			{
				if (!DidWeMeetLoad && DidWePartlyMeetLoad)
				{
					ErrorCode = 0;
					count_DidWeNotMeetLoad++;
					if (pOptimal->ElectricalPower == IMPLAUSIBLE_POWER)
					{
						ShowWarningError("Model was not able to provide cooling for a time step, called in HybridEvapCooling:dostep");
						pOptimal->ElectricalPower = 0;
					}
					list<CSetting*>::iterator iterOperatingSettings2 = CurrentOperatingSettings.begin();
					CSetting* pSetting = *iterOperatingSettings2;
					*pSetting = *pOptimal;
					pSetting->Runtime_Fraction = 1;
					PrimaryMode = pSetting->Mode;
					PrimaryModeRuntimeFraction = 1;
				}
				else
				{
					list<CSetting*>::iterator iterOperatingSettings2 = CurrentOperatingSettings.begin();
					CSetting* pSetting = *iterOperatingSettings2;
					*pSetting = *pStandBy;
					ErrorCode = -1;
					count_DidWeNotMeetLoad++;
				}
			}
			bool foundwarnings = false;
			Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + SysTimeElapsed;

			// Use the elapsed time to only give a summary of warnings related to the number of Timesteps environmental conditions, or supply air temperature constraints were not met for a given day.
			// ideally there would be a clear flag that indicates "this is the last timestep of the day, so report", but that doesn't seem to exist. 
			if ((TimeElapsed > 24) && WarnOnceFlag && !WarmupFlag)
			{
				if (count_EnvironmentConditionsNotMet>0) ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, system " + Name.c_str() + " was unable to operate for " + RoundSigDigits((Real64)count_EnvironmentConditionsNotMet, 1)+ " timesteps because environment conditions were beyond the allowable operating range for any mode.");
				if (count_SAHR_OC_MetOnce>0)  ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air humidity ratio for " + RoundSigDigits(Real64(count_SAHR_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str() + " was set to mode 0");
				if (count_SAT_OC_MetOnce>0) ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air temperature constraints for " + RoundSigDigits(Real64(count_SAT_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str()+" was set to mode 0" );
				
				ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to  satisfy sensible load for " + RoundSigDigits((Real64)count_DidWeNotMeetLoad, 1) + " time steps. For these time steps settings were selected to provide as much sensible cooling or heating as possible, given other constraints.");
				
				count_SAT_OC_MetOnce = 0;
				count_DidWeNotMeetLoad = 0;
				count_SAHR_OC_MetOnce = 0;
				count_EnvironmentConditionsMetOnce = 0;
				count_EnvironmentConditionsNotMet = 0;
				WarnOnceFlag = false;
			}
			if (DataGlobals::HourOfDay == 1 && !WarnOnceFlag && !WarmupFlag)
			{
				WarnOnceFlag = true;
			}
			return ErrorCode; 
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
		Real64 Model::CurrentPrimaryRuntimeFraction()
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
		void Model::doStep(Real64 Tosa, Real64 Tra, Real64 RHosa, Real64 RHra, Real64 RequestedCoolingLoad, Real64 RequestedHeatingLoad, Real64 OutputRequiredToHumidify, Real64 OutputRequiredToDehumidify, Real64 DesignMinVR)
		{
			MinOA_Msa = DesignMinVR;// as mass flow kg/s
			CStepInputs StepIns;

			RequestedHumdificationMass = OutputRequiredToHumidify;
			RequestedHumdificationLoad = OutputRequiredToHumidify * 2257; // [W];
			RequestedHumdificationEnergy = OutputRequiredToHumidify * 2257 * TimeStepSys * SecInHour; // [W]

			RequestedDeHumdificationMass = OutputRequiredToDehumidify;
			RequestedDeHumdificationLoad = OutputRequiredToDehumidify * 2257; // [W]; 
			RequestedDeHumdificationEnergy = OutputRequiredToDehumidify * 2257 * TimeStepSys * SecInHour; // [W] 

			StepIns.RequestedCoolingLoad = -RequestedCoolingLoad / 1000; //convert to kw Cooling possitive now, heating negitive
			StepIns.RequestedHeatingLoad = -RequestedHeatingLoad / 1000;
			// values from function call are copied here to a containter class, this helps with the unit tests so we always know what values need to be set
			StepIns.Tosa = Tosa; StepIns.Tra = Tra; StepIns.RHosa = RHosa; StepIns.RHra = RHra; StepIns.ZoneMoistureLoad = RequestedHumdificationLoad;  StepIns.ZoneDehumidificationLoad = RequestedDeHumdificationLoad; StepIns.MinimumOA = MinOA_Msa;
			Real64 Wosa = CalcHum_ratio_W(Tosa, RHosa, 101.325);
			Real64 Wra = CalcHum_ratio_W(Tra, RHra, 101.325);
			CoolingRequested = false;
			HeatingRequested = false;
			VentilationRequested = false;
			// establish if conditioning needed 
			if (StepIns.RequestedCoolingLoad >= MINIMUM_LOAD_TO_ACTIVATE)
			{
				CoolingRequested = true;
				StepIns.RequestedHeatingLoad = 0;
			}
			if (StepIns.RequestedHeatingLoad <= -MINIMUM_LOAD_TO_ACTIVATE)
			{
				HeatingRequested = true;
				StepIns.RequestedCoolingLoad = 0;
			}
			// establish if ventilation needed 
			if (MinOA_Msa > 0) VentilationRequested = true;

			std::list<CMode*>::const_iterator iterator;
			iterator = OperatingModes.begin();

			if (SetStandByMode(*iterator, pStandBy, Tosa, Wosa, Tra, Wra))
			{
				std::string ObjectID = Name.c_str();
				ShowSevereError("Standby mode not defined correctly, as the mode is defined there are zero combinations of acceptible outside air fractions and supply air mass flow rate, called in object " + ObjectID);
			}
			// test availbility status 
			UnitOn = 1;
			if (GetCurrentScheduleValue(SchedPtr) <= 0) {
				UnitOn = 0;
			}
			// go into standby if unit is off or not needed
			if (((!CoolingRequested && !HeatingRequested) && !VentilationRequested) || !UnitOn)
			{
				pStandBy->Runtime_Fraction = 1;
				list<CSetting*>::iterator iterOperatingSettings = CurrentOperatingSettings.begin();
				CSetting* pSetting = *iterOperatingSettings;
				*pSetting = *pStandBy;
				ErrorCode = 0;
				PrimaryMode = 0;
				PrimaryModeRuntimeFraction = 0;
			}
			else
			{
				ErrorCode = SetOperatingSetting(StepIns);
			}

			if (ErrorCode == 0)
			{
				SupplyVentilationAir = CalculateTimeStepAverage(SYSTEMOUTPUTS::VENTILATION_AIR_V);

				if (StdRhoAir > 1)
				{
					SupplyVentilationVolume = SupplyVentilationAir / StdRhoAir;
				}
				else
				{
					SupplyVentilationVolume = SupplyVentilationAir / 1.225;
				}

				OutletTemp = CheckVal_T(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_TEMP));
				OutletHumRat = CheckVal_W(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_HR));

				OutletRH = PsyRhFnTdbWPb(OutletTemp, OutletHumRat, 101325); //could also use outlet pressure instead of fixed
				Real64 OperatingAverageMixedAirTemperature = CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_TEMP);
				Real64 OperatingMixedAirW = CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_HR);
				Real64 MixedAirEnthalpy = PsyHFnTdbW(OperatingAverageMixedAirTemperature, OperatingMixedAirW);
				OutletEnthalpy = PsyHFnTdbRhPb(OutletTemp, OutletRH, InletPressure); //consider if inlet and outlet presures are different
				OutletMassFlowRate = CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_MASS_FLOW);

				if (StdRhoAir > 1)
				{
					OutletVolumetricFlowRate = OutletMassFlowRate / StdRhoAir;
				}
				else
				{
					OutletVolumetricFlowRate = OutletMassFlowRate / 1.225;
				}
				if (OutletMassFlowRate > 0)
				{
					averageOSAF = SupplyVentilationAir / OutletMassFlowRate;
				}
				else 
				{
					std::string ObjectID = Name.c_str();
					if (CoolingRequested || HeatingRequested)
					{
						ShowSevereError("Outlet air mass flow rate of zero during period with conditioning need, check mode definition. Called in object " + Name);
					}
					averageOSAF = 1;
				}
				PrimaryMode = CurrentPrimaryMode();
				PrimaryModeRuntimeFraction = CurrentPrimaryRuntimeFraction();

				Real64 QTotZoneOut = 0;
				Real64 QSensZoneOut = 0;
				Real64 QLatentZoneOut = 0;

				Real64 QTotSystemOut = 0;
				Real64 QSensSystemOut = 0;
				Real64 QLatentSystemOut = 0;
				Real64 Outletcp = PsyCpAirFnWTdb(OutletHumRat, OutletTemp) / 1000;
				Real64 Returncp = PsyCpAirFnWTdb(Wra, Tra) / 1000;
				Real64 Outdoorcp = PsyCpAirFnWTdb(Wosa, Tosa) / 1000;

				// All powers are calculated in Watts amd energies in Joules
				if (CoolingRequested || HeatingRequested || DehumidificationRequested || HumidificationRequested)
				{
					//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
					//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
					//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}		
					QSensZoneOut = 1000 * OutletMassFlowRate* 0.5* (Returncp + Outletcp)*(InletTemp - OutletTemp);// 
					QLatentZoneOut = 1000 * 2257 * OutletMassFlowRate* (InletHumRat - OutletHumRat);
					QTotZoneOut = OutletMassFlowRate * (InletEnthalpy - OutletEnthalpy);
					Real64 QLatentCheck = QTotZoneOut - QSensZoneOut;

					//System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
					//System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
					//System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}

					Real64 SystemTimeStepCp = Returncp + averageOSAF*(Outdoorcp - Returncp) + Outletcp;//cpRA + OSAF*(cpOSA-cpRA) + cpSA
					Real64 SystemTimeStepW = InletHumRat + averageOSAF*(Wosa - Wra) - OutletHumRat;//HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA
					Real64 SystemTimeStepT = Tra + averageOSAF*(Tosa - Tra) - OutletTemp;//T_RA + OSAF *(T_OSA - T_RA) - T_SA
					QSensSystemOut = 1000 * 0.5* SystemTimeStepCp *OutletMassFlowRate* SystemTimeStepT;//kw  dynamic cp
					QLatentSystemOut = 1000 * 2257 * OutletMassFlowRate*SystemTimeStepW;
					QTotSystemOut = OutletMassFlowRate * (MixedAirEnthalpy - OutletEnthalpy);
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
				if (CoolingRequested)
				{
					UnitTotalCoolingRate = std::abs(QTotZoneOut);
					UnitTotalCoolingEnergy = UnitTotalCoolingRate * TimeStepSys * SecInHour;
					UnitSensibleCoolingRate = std::abs(QSensZoneOut);
					UnitSensibleCoolingEnergy = UnitSensibleCoolingRate * TimeStepSys * SecInHour;
					UnitLatentCoolingRate = UnitTotalCoolingRate - UnitSensibleCoolingRate;
					UnitLatentCoolingEnergy = UnitTotalCoolingEnergy - UnitSensibleCoolingEnergy;

					SystemTotalCoolingRate = std::abs(QTotSystemOut);
					SystemTotalCoolingEnergy = SystemTotalCoolingRate * TimeStepSys * SecInHour;
					SystemSensibleCoolingRate = std::abs(QSensSystemOut);
					SystemSensibleCoolingEnergy = SystemSensibleCoolingRate * TimeStepSys * SecInHour;
					SystemLatentCoolingRate = SystemTotalCoolingRate - SystemSensibleCoolingRate;
					SystemLatentCoolingEnergy = SystemTotalCoolingEnergy - SystemSensibleCoolingEnergy;
				}
				else
				{
					UnitTotalCoolingRate = UnitTotalCoolingEnergy = UnitSensibleCoolingRate = UnitSensibleCoolingEnergy = UnitLatentCoolingRate = UnitLatentCoolingEnergy = 0;
					SystemTotalCoolingRate = SystemTotalCoolingEnergy = SystemSensibleCoolingRate = SystemSensibleCoolingEnergy = SystemLatentCoolingRate = SystemLatentCoolingEnergy = 0;

				}
				// set heating specific outputs 
				if (HeatingRequested)
				{
					UnitTotalHeatingRate = std::abs(QTotZoneOut);
					UnitTotalHeatingEnergy = UnitTotalHeatingRate * TimeStepSys * SecInHour;
					UnitSensibleHeatingRate = std::abs(QSensZoneOut);
					UnitSensibleHeatingEnergy = UnitSensibleHeatingRate * TimeStepSys * SecInHour;
					UnitLatentHeatingRate = UnitTotalHeatingRate - UnitSensibleHeatingRate;
					UnitLatentHeatingEnergy = UnitTotalHeatingEnergy - UnitSensibleHeatingEnergy;

					SystemTotalHeatingRate = std::abs(QTotSystemOut);
					SystemTotalHeatingEnergy = SystemTotalHeatingRate * TimeStepSys * SecInHour;
					SystemSensibleHeatingRate = std::abs(QSensSystemOut);
					SystemSensibleHeatingEnergy = SystemSensibleHeatingRate * TimeStepSys * SecInHour;
					SystemLatentHeatingRate = SystemTotalHeatingRate - SystemSensibleHeatingRate;
					SystemLatentHeatingEnergy = SystemTotalHeatingEnergy - SystemSensibleHeatingEnergy;
				}
				else
				{
					UnitTotalHeatingRate = UnitTotalHeatingEnergy = UnitSensibleHeatingRate = UnitSensibleHeatingEnergy = UnitLatentHeatingRate = UnitLatentHeatingEnergy = 0;
					SystemTotalHeatingRate = SystemTotalHeatingEnergy = SystemSensibleHeatingRate = SystemSensibleHeatingEnergy = SystemLatentHeatingRate = SystemLatentHeatingEnergy = 0;

				}
				// fuel use in calculation is in Kw, powers are typically output in EP in Watts, so do conversion here. 
				FinalElectricalPower = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::SYSTEM_FUEL_USE);
				FinalElectricalEnergy = FinalElectricalPower*TimeStepSys * SecInHour;
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
				ResetOutputs();
			}
		}

	}
}
